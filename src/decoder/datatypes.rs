use std::{cell::RefCell, fmt::Display, hash::Hash, ops::RangeBounds, rc::Rc};

use nom::{
    bits::complete::{bool, tag, take},
    combinator::{map, verify},
    error::Error,
    multi::count,
    sequence::tuple,
    Parser,
};

use crate::{
    decoder::errors::{make_exierror, ExiErrorKind},
    util::{bound_values, lower_bound, BitInput},
};

use super::{ExiResult, StringTable};

// https://www.w3.org/TR/exi/#encodingBinary
fn binary(i: BitInput) -> ExiResult<BitInput, Vec<u8>> {
    let (rest, len) = unsigned_int(i)?;
    count(take(8usize), usize::try_from(len).unwrap())(rest)
}

enum XsdBool {
    False,
    Zero,
    True,
    One,
}

// https://www.w3.org/TR/exi/#encodingBoolean
fn xsd_boolean(i: BitInput) -> ExiResult<BitInput, XsdBool> {
    map(n_bit_unsigned_int(2, true), |b: u64| match b {
        0 => XsdBool::False,
        1 => XsdBool::Zero,
        2 => XsdBool::True,
        3 => XsdBool::One,
        _ => unreachable!(),
    })(i)
}

// https://www.w3.org/TR/exi/#encodingBoolean
fn boolean(i: BitInput) -> ExiResult<BitInput, bool> {
    bool(i)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decimal {
    negative: bool,
    integral: u64,
    // TODO: when turning into string, fractional is reversed
    fractional: u64,
}

impl From<Decimal> for String {
    fn from(value: Decimal) -> Self {
        let mut out = String::new();
        if value.negative {
            out += "-";
        }
        out += &format!("{}.{}", value.integral, value.fractional.reverse_bits());
        out
    }
}

// https://www.w3.org/TR/exi/#encodingDecimal
fn decimal(i: BitInput) -> ExiResult<BitInput, Decimal> {
    let (next, negative) = boolean(i)?;
    let x = map(
        tuple((unsigned_int, unsigned_int)),
        |(integral, fractional)| Decimal {
            negative,
            integral,
            fractional,
        },
    )(next);
    x
}

// https://www.w3.org/TR/exi/#encodingFloat
fn float(i: BitInput) -> ExiResult<BitInput, f64> {
    unimplemented!()
}

// https://www.w3.org/TR/exi/#encodingInteger
fn integer<B>(bound: Option<B>) -> impl FnMut(BitInput) -> ExiResult<BitInput, i64>
where
    B: RangeBounds<i64>,
{
    move |i| {
        if let Some(b) = &bound {
            match (bound_values(b), lower_bound(b)) {
                // If the number of values in the bounds is 4096 or lower, decode as a n-bit
                // unsigned int, offset from the minimum value.
                (Some(num), _) if num <= 4096 => {
                    let n = num.ilog2() + 1;
                    return map(n_bit_unsigned_int(n, true), |v| v as i64)(i);
                }
                // Otherwise, if the integer's lower bound is >=0, decode as an unsigned int
                (_, Some(lower)) if lower >= 0 => {
                    return unsigned_int(i).map(|(i, v)| (i, v as i64))
                }
                // In all other cases, drop through
                _ => {}
            }
        }

        // Decode the integer as a bool sign and unsigned integer
        map(tuple((boolean, unsigned_int)), |(negative, magnitude)| {
            if negative {
                -(magnitude as i64)
            } else {
                magnitude as i64
            }
        })(i)
    }
}

// Parse an exact unsigned int
pub fn unsigned_int_x(x: usize) -> impl FnMut(BitInput) -> ExiResult<BitInput, usize> {
    if x > 128 {
        unimplemented!("Can only do single-byte matches currently.")
    }
    move |i| tag(x, 8usize)(i)
}

// https://www.w3.org/TR/exi/#encodingUnsignedInteger
pub fn unsigned_int(i: BitInput) -> ExiResult<BitInput, u64> {
    let mut value = 0u64;
    let mut multiplier = 1u64;
    let mut rem = i;
    loop {
        let (r, oct): (_, u8) = take(8usize)(rem)?;
        rem = r;
        value += u64::from(oct & 0b0111_1111) * multiplier;
        multiplier *= 128;
        if oct >> 7 == 0 {
            break;
        }
    }
    Ok((rem, value))
}

#[derive(Debug, Clone, Eq)]
pub struct Qname {
    uri: String,
    local_name: String,
    prefix: Option<String>,
}

impl Display for Qname {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut out = String::new();
        if !self.uri.is_empty() {
            out += &format!("{}:", self.uri);
        }
        write!(f, "{}{}", out, self.local_name)
    }
}

impl From<&str> for Qname {
    fn from(value: &str) -> Self {
        Qname {
            uri: "".into(),
            local_name: value.into(),
            prefix: None,
        }
    }
}

impl PartialEq for Qname {
    // "Two qnames are considered equal if they have the same uri and local-name,
    // regardless of their prefix values."
    // TODO: verify that this actually works for us
    fn eq(&self, other: &Self) -> bool {
        self.uri == other.uri && self.local_name == other.local_name
    }
}

impl Hash for Qname {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uri.hash(state);
        self.local_name.hash(state);
    }
}

// https://www.w3.org/TR/exi/#encodingQName
pub fn qname<'a>(
    st: Rc<RefCell<StringTable>>,
    preserve_prefix: bool,
) -> impl Fn(BitInput<'a>) -> ExiResult<BitInput<'a>, Qname> {
    move |i| {
        // FIXME: can these borrow_muts be tidied up?
        let (r, uri) = st.borrow_mut().parse_uri()(i)?;
        let (r1, local_name) = st.borrow_mut().parse_localname(&uri)(r)?;
        if preserve_prefix {
            let (r2, pref) = st.borrow_mut().parse_prefix()(i)?;
            Ok((
                r2,
                Qname {
                    uri,
                    local_name,
                    prefix: Some(pref),
                },
            ))
        } else {
            Ok((
                r1,
                Qname {
                    uri,
                    local_name,
                    prefix: None,
                },
            ))
        }
    }
}

pub struct DateTime {}

// https://www.w3.org/TR/exi/#encodingDateTime
pub fn datetime(_i: BitInput) -> ExiResult<BitInput, DateTime> {
    unimplemented!();
}

// https://www.w3.org/TR/exi/#encodingBoundedUnsigned
pub fn n_bit_unsigned_int(
    n: u32,
    bitpacked: bool,
) -> impl FnMut(BitInput) -> ExiResult<BitInput, u64> {
    move |i| {
        if !bitpacked {
            return unsigned_int(i);
        }
        take(n)(i)
    }
}

// https://www.w3.org/TR/exi/#encodingString
pub fn string(i: BitInput) -> ExiResult<BitInput, String> {
    parse_string_with_len_offset(0)(i)
}

pub fn parse_string_with_len_offset(
    len_offset: u8,
) -> impl FnMut(BitInput) -> ExiResult<BitInput, String> {
    // FIXME: restricted character sets (https://www.w3.org/TR/exi/#restrictedCharSet)
    move |i| {
        let (rem, mut len) = unsigned_int(i)?;
        log::debug!("parse string with len {}, offset {}", len, len_offset);
        len -= len_offset as u64;
        let (rest, codepoints) = count(
            map(
                verify(unsigned_int, |i| {
                    u64::from(u32::MIN) <= *i && *i <= u64::from(u32::MAX)
                }),
                |i| u32::try_from(i).unwrap(),
            ),
            usize::try_from(len).unwrap(),
        )(rem)?;
        let codepoints: Vec<u32> = codepoints
            .into_iter()
            .map(u32::try_from)
            .collect::<Result<_, _>>()
            // TODO: this error handling is cheesy, we should have custom types really
            .map_err(|_| nom::Err::Failure(make_exierror(i, ExiErrorKind::BadString)))?;
        // FIXME: Currently this drops invalid codepoints silently, we should instead return
        // an error
        let s = codepoints
            .into_iter()
            .filter_map(char::from_u32)
            .collect::<String>();
        log::debug!("parse string: parsed {}", s);
        Ok((rest, s))
    }
}

// A function which parses an EXI datatype
type TypeParser<'a, O> = fn(BitInput<'a>) -> ExiResult<BitInput<'a>, O>;

// https://www.w3.org/TR/exi/#encodingList
// Parse an EXI-encoded list with the type parser `tp`
fn parse_list<'a, F, O>(
    tp: TypeParser<'a, O>,
) -> impl FnMut(BitInput<'a>) -> ExiResult<BitInput<'a>, Vec<O>>
where
    F: Parser<BitInput<'a>, O, Error<BitInput<'a>>>,
{
    move |i: BitInput<'a>| {
        let (rest, len) = unsigned_int(i)?;
        count(tp, len.try_into().unwrap())(rest)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Binary(Vec<u8>),
    Boolean(bool), // What about xsdbools?
    DateTime(()),  // TODO
    Decimal(Decimal),
    Float(f32),
    Integer(i32), // Includes n-bit uints
    String(String),
    UnsignedInteger(u32),
    // TODO: maybe qname, list
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl From<Value> for String {
    fn from(value: Value) -> Self {
        match value {
            Value::Binary(_) => unimplemented!(),
            Value::Boolean(b) => (if b { "true" } else { "false" }).into(),
            Value::DateTime(_) => unimplemented!(),
            Value::Decimal(d) => d.into(),
            Value::Float(f) => f.to_string(),
            Value::Integer(i) => i.to_string(),
            Value::String(s) => s,
            Value::UnsignedInteger(u) => u.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_failure_contents<I: Clone + PartialEq + std::fmt::Debug, O: std::fmt::Debug>(
        r: ExiResult<I, O>,
        v: I,
    ) {
        match r {
            Err(nom::Err::Error(e)) => assert_eq!(e.input, v),
            wat => panic!("Unexpected {:?}", wat),
        }
    }

    #[test]
    fn test_parse_unsigned_int_x() {
        assert_eq!(
            unsigned_int_x(8)((&[0b0000_1000], 0)),
            Ok(((vec!().as_slice(), 0), 8)),
        );
        assert_eq!(
            unsigned_int_x(1)((&[0b0000_0001], 0)),
            Ok(((vec!().as_slice(), 0), 1)),
        );
        assert_eq!(
            unsigned_int_x(0)((&[0b0000_0000], 0)),
            Ok(((vec!().as_slice(), 0), 0)),
        );
        check_failure_contents(unsigned_int_x(2)((&[0b0000_0001], 0)), (&[0b0000_0001], 0));
    }

    #[test]
    fn test_parse_unsigned_int() {
        assert_eq!(
            unsigned_int((&[0b0000_0000], 0)),
            Ok(((vec!().as_slice(), 0), 0)),
        );
        assert_eq!(
            unsigned_int((&[0b0111_1111], 0)),
            Ok(((vec!().as_slice(), 0), 127)),
        );
        assert_eq!(
            unsigned_int((&[0b1111_1111, 0b0111_1111], 0)),
            Ok(((vec!().as_slice(), 0), 16383)),
        );
        assert_eq!(
            unsigned_int((&[0b1111_1111, 0b1111_1111, 0b0111_1111], 0)),
            Ok(((vec!().as_slice(), 0), 2097151)),
        );
    }
}
