mod codetree;
mod datatypes;
mod errors;
mod grammars;
mod options;

use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, VecDeque};
use std::fmt::Display;
use std::ops::{Deref, Index, IndexMut};
use std::rc::Rc;

use datatypes::{
    n_bit_unsigned_int, parse_string_with_len_offset, qname, unsigned_int_x, Qname, Value,
};
use errors::ExiError;
use grammars::{DocumentGrammar, ElementGrammar, GrammaryThing};
use nom::branch::alt;
use nom::combinator::{all_consuming, map, success};
use nom::{
    bits::{
        bits,
        complete::{bool, tag, take},
    },
    combinator::opt,
    sequence::{preceded, tuple},
    IResult,
};
use options::Options;

use crate::util::{ilog2_ceil, trailing_bits, BitInput};

type ExiResult<I, O> = IResult<I, O, ExiError<I>>;

/// The EXI stream header. Contains the format `version` and any `options`.
#[derive(Debug, PartialEq)]
pub struct Header {
    pub version: Version,
    pub options: Option<Options>,
}

#[derive(Debug)]
struct PrefixTable<T>(Vec<T>);

impl<T: std::fmt::Debug> PrefixTable<T> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn add(&mut self, i: T) -> usize {
        self.0.push(i);
        log::debug!(
            "Added {:?} to prefix table, contents: {:?}",
            self.0[self.0.len() - 1],
            self.0
        );
        self.0.len() + 1
    }

    fn get(&self, i: usize) -> Option<&T> {
        self.0.get(i)
    }

    fn with_initial<X, I: IntoIterator<Item = X>>(i: I) -> Self
    where
        X: Into<T>,
    {
        Self(Vec::from_iter(i.into_iter().map(|e| e.into())))
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    // Length in bits of the prefix needed to identify values in this table
    fn prefix_length(&self) -> u32 {
        ilog2_ceil(self.len())
    }
}

impl PrefixTable<URIStringTable> {
    fn find(&mut self, uri: &str) -> Option<&mut URIStringTable> {
        self.0.iter_mut().find(|s| s.uri == uri)
    }
}

impl<T> Index<usize> for PrefixTable<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<T> IndexMut<usize> for PrefixTable<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[derive(Debug)]
struct URIStringTable {
    uri: String,
    prefix: PrefixTable<String>,
    local_name: PrefixTable<String>,
}

impl URIStringTable {
    fn with_uri(uri: String) -> Self {
        URIStringTable {
            uri,
            prefix: PrefixTable::new(),
            local_name: PrefixTable::new(),
        }
    }
}

struct StringTable {
    uris: PrefixTable<URIStringTable>,
    local_values: HashMap<Qname, PrefixTable<Rc<Value>>>,
    global_values: PrefixTable<Rc<Value>>,
}

impl StringTable {
    fn add_value(&mut self, qname: Qname, value: Value) {
        log::debug!("Adding {:?} to string table", value);
        let s = Rc::new(value);
        self.global_values.add(s.clone());
        self.local_values
            .entry(qname)
            .or_insert(PrefixTable::new())
            .add(s);
    }

    // Using the string table, parse a URI from the bitstream
    // URI partitions are "Optimized for Frequent use of Compact Identifiers" - the
    // compact identifiers are n bit integers where n = ceil(log2(number_of_entries + 1))
    fn parse_uri<'a, 'b>(
        &'b mut self,
    ) -> impl FnMut(BitInput<'a>) -> ExiResult<BitInput<'a>, String> + 'b {
        move |i| {
            let to_take = ilog2_ceil(self.uris.len() + 1);
            log::debug!(
                "parse_uri from uri partition len {}, using {}-bit prefix",
                self.uris.len(),
                to_take
            );
            let (rest, prefix) = n_bit_unsigned_int(to_take, true)(i)?;
            log::debug!("parse_uri prefix = {}", prefix);
            if prefix == 0 {
                // Miss, what follows is a full uri (string, with length field n+1 where n
                // is the length of the string)
                let (rest2, uri) = parse_string_with_len_offset(1)(i)?;
                log::debug!("parse uri: miss, adding {}", uri);
                self.uris.add(URIStringTable::with_uri(uri.clone()));
                Ok((rest2, uri))
            } else {
                // Hit, uri index is prefix - 1
                // TODO: error rather than panic
                let u = self
                    .uris
                    .get((prefix - 1).try_into().unwrap())
                    .and_then(|u| Some(u.uri.clone()))
                    .unwrap();
                log::debug!("parse uri: hit, got '{}'", u);
                Ok((rest, u))
            }
        }
    }

    fn parse_prefix<'a>(&mut self) -> impl FnMut(BitInput<'a>) -> ExiResult<BitInput<'a>, String> {
        move |i| Ok((i, "fakeprefix".into()))
    }

    // Using the string table, parse a local name from the bitstream.
    // Localname partitions are optimized for "Frequent use of String Literals":
    // For a "hit" in the string table, the value is encoded as 0 as an unsigned int,
    // followed by the compact identifier as an n bit unsigned int.
    // For a "miss", it's the value encoded as a string with the length field incremented
    // by 1.
    fn parse_localname<'a, 'b>(
        &'b mut self,
        uri: &'b str,
    ) -> impl FnMut(BitInput<'a>) -> ExiResult<BitInput<'a>, String> + 'b {
        move |i| {
            let ln = &mut self.uris.find(&uri).unwrap().local_name;
            // Hit - a uint 0 followed by the compact identifier
            if let Ok((rest, s)) = preceded(
                unsigned_int_x(0),
                map(n_bit_unsigned_int(ln.prefix_length(), true), |p| {
                    ln.get(p as usize).unwrap().clone()
                }),
            )(i)
            {
                log::debug!("parse localname: hit, got {}", s);
                return Ok((rest, s));
            }
            // Miss - a string with length incremented by 1
            let (rest, s) = parse_string_with_len_offset(1)(i)?;
            log::debug!("parse localname: miss, adding {}", s);
            ln.add(s.clone());
            Ok((rest, s))
        }
    }

    // Using the string table, parse a value from the bitstream.
    // Value partitions are optimized for "Frequent use of String Literals", and we may
    // find the value in either the global value partition or the uri-local one.
    // - a "hit" in the local value table is encoded by 0 as an unsigned int, followed by
    //   the compact identifier
    // - a "hit" in the global value table is encoded by 1 as an unsigned int, followed by
    //   the compact identifier
    // - a "miss" is encoded by the string value, with the length field incremented by 2.
    fn parse_value<'a, 'b>(
        &'b mut self,
        qname: &'b Qname,
    ) -> impl FnMut(BitInput<'a>) -> ExiResult<BitInput<'a>, Value> + 'b {
        move |i| {
            if let Ok((rest1, foo)) = alt((unsigned_int_x(0), unsigned_int_x(1)))(i) {
                match foo {
                    0 => {
                        let prefix_length = self.local_values.get(&qname).unwrap().prefix_length();
                        let (rest2, idx) = n_bit_unsigned_int(prefix_length, true)(rest1)?;
                        let val = Rc::as_ref(
                            self.local_values
                                .get(&qname)
                                .unwrap()
                                .get(idx as usize)
                                .unwrap(),
                        )
                        .clone();
                        log::trace!(
                            "parsed value {:?} from local table, qname {}, prefix {} (len {})",
                            val,
                            qname,
                            idx,
                            prefix_length
                        );
                        Ok((rest2, val))
                    }
                    1 => {
                        let prefix_length = self.global_values.prefix_length();
                        let (rest2, idx) = n_bit_unsigned_int(prefix_length, true)(rest1)?;
                        let val = Rc::as_ref(self.global_values.get(idx as usize).unwrap()).clone();
                        log::trace!(
                            "parsed value {:?} from global table, prefix {} (len {})",
                            val,
                            idx,
                            prefix_length
                        );
                        Ok((rest2, val))
                    }
                    _ => unreachable!(),
                }
            } else {
                let (rest, s) = parse_string_with_len_offset(2)(i)?;
                self.add_value(qname.clone(), Value::String(s.clone()));
                Ok((rest, Value::String(s)))
            }
        }
    }
}

impl Default for StringTable {
    fn default() -> Self {
        let mut uris = PrefixTable::new();
        uris.add(URIStringTable {
            uri: "".to_string(),
            prefix: PrefixTable::with_initial([""]),
            local_name: PrefixTable::new(),
        });
        uris.add(URIStringTable {
            uri: "http://www.w3.org/XML/1998/namespace".into(),
            prefix: PrefixTable::with_initial(["xml"]),
            local_name: PrefixTable::with_initial(["base", "id", "lang", "space"]),
        });
        uris.add(URIStringTable {
            uri: "http://www.w3.org/2001/XMLSchema-instance".into(),
            prefix: PrefixTable::with_initial(["xsi"]),
            local_name: PrefixTable::with_initial(["nil", "type"]),
        });
        // TODO: schema-informed...
        StringTable {
            uris,
            global_values: PrefixTable::new(),
            local_values: HashMap::new(),
        }
    }
}

/// Events representing the structure and content of XML, decoded from an EXI stream
#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    StartDocument, // SD
    EndDocument,   // ED
    StartElementQname {
        prefix: Option<String>,
    }, // SE ( qname )
    StartElementUri {
        local_name: String,
        prefix: Option<String>,
    }, // SE ( uri:* )
    StartElement {
        qname: Qname,
    }, // SE ( * )
    EndElement,    // EE
    AttributeQname {
        prefix: Option<String>,
        value: Value,
    }, // AT ( qname )
    AttributeUri {
        local_name: String,
        prefix: Option<String>,
        value: Value,
    }, // AT ( uri:* )
    Attribute {
        qname: Qname,
        value: Value,
    }, // AT ( * )
    Characters {
        value: Value,
    }, // CH
    NamespaceDeclaration {
        uri: String,
        prefix: String,
        local_e_ns: bool,
    }, // NS
    Comment {
        text: String,
    }, // CM
    ProcessingInstruction {
        name: String,
        text: String,
    }, // PI
    Doctype {
        name: String,
        public: String,
        system: String,
        text: String,
    }, // DT
    EntityReference {
        name: String,
    }, // ER
    SelfContained, // SC
}

/// The EXI format version
#[derive(Debug, PartialEq)]
pub enum Version {
    Preview(usize),
    Final(usize),
}

impl Version {
    fn parse(i: BitInput) -> IResult<BitInput, Version> {
        let (mut i, pf) = bool(i)?;
        let mut ver: usize = 1;
        loop {
            let (i2, nyb): (_, usize) = take(4usize)(i)?;
            i = i2;
            ver += nyb;
            if nyb != 15 {
                break;
            }
        }
        Ok((
            i,
            match pf {
                false => Version::Final(ver),
                true => Version::Preview(ver),
            },
        ))
    }
}

fn header<'a>(i: BitInput<'a>) -> ExiResult<BitInput<'a>, Header> {
    let (rem, (options_present, ver)) = preceded(
        opt(tag(0x24455849, 32usize)),
        preceded(tag(0b10, 2usize), tuple((bool, Version::parse))),
    )(i)
    .map_err(nom::Err::convert)?;
    if options_present {
        unimplemented!();
    }
    Ok((
        rem,
        Header {
            version: ver,
            options: None,
        },
    ))
}

// Newtype to make things a bit more ergonomic
struct GrammarStack(Vec<Rc<RefCell<dyn GrammaryThing>>>);
impl GrammarStack {
    fn new() -> Self {
        Self(Vec::new())
    }
    fn push(&mut self, v: Rc<RefCell<dyn GrammaryThing>>) {
        self.0.push(v);
        log::debug!(
            "Pushed grammar. Stack: {}",
            self.0
                .iter()
                .map(|g| g.deref().borrow().describe())
                .collect::<Vec<_>>()
                .join(", ")
        );
        self.current().borrow().pprint();
    }

    fn pop(&mut self) {
        self.0.pop();
        log::debug!(
            "Popped grammar. Stack: {}",
            self.0
                .iter()
                .map(|g| g.deref().borrow().describe())
                .collect::<Vec<_>>()
                .join(", ")
        );
        self.current().borrow().pprint();
    }

    fn current(&self) -> &RefCell<dyn GrammaryThing> {
        self.0.last().unwrap().deref()
    }
}

fn body(i: BitInput) -> ExiResult<BitInput, Vec<Event>> {
    let state = Rc::new(DecoderState::new());
    let mut grammar_stack = GrammarStack::new();
    grammar_stack.push(Rc::new(RefCell::new(DocumentGrammar::new(
        state.options.clone(),
    ))));
    let mut output = Vec::new();
    let mut input = i;
    loop {
        let (rest, event) = grammar_stack.current().borrow_mut().parse(input)?;
        input = rest;
        log::debug!("ParseEvent: {:?}", event);
        let (rest, parsed_event) =
            match event {
                ParseEvent::SD => success(Event::StartDocument)(input)?,
                ParseEvent::SE => {
                    let (r1, qname) =
                        qname(state.string_table.clone(), state.options.preserve.prefixes)(input)?;
                    let ev = Event::StartElement {
                        qname: qname.clone(),
                    };
                    // Add a SE ( qname ) production to the current grammar
                    grammar_stack.current().borrow_mut().specialise(&ev);
                    let mut egs = state.element_grammars.borrow_mut();
                    if let Some(eg) = egs.get(&qname) {
                        eg.borrow_mut().reset();
                        grammar_stack.push(eg.clone());
                    } else {
                        log::debug!("creating new global element grammar for qname {}", qname);
                        let ng = Rc::new(RefCell::new(ElementGrammar::new(
                            qname.clone(),
                            state.options.clone(),
                        )));
                        egs.insert(qname, ng.clone());
                        grammar_stack.push(ng);
                    }
                    (r1, ev)
                }
                ParseEvent::CH => {
                    let (r, value) =
                        state.string_table.clone().borrow_mut().parse_value(
                            grammar_stack.current().borrow().context_qname().unwrap(),
                        )(input)?;
                    let ev = Event::Characters { value };
                    grammar_stack.current().borrow_mut().specialise(&ev);
                    (r, ev)
                }
                ParseEvent::EE => {
                    grammar_stack.pop();
                    success(Event::EndElement)(input)?
                }
                ParseEvent::ED => success(Event::EndDocument)(input)?,
                ParseEvent::AT => {
                    // Parse the qname
                    let (r1, qname) =
                        qname(state.string_table.clone(), state.options.preserve.prefixes)(input)?;
                    // Parse the value
                    let (rest, value) =
                        state.string_table.clone().borrow_mut().parse_value(&qname)(r1)?;
                    let ev = Event::Attribute { qname, value };
                    // Add to this element's grammar
                    grammar_stack.current().borrow_mut().specialise(&ev);
                    (rest, ev)
                }
                ParseEvent::ATQname(qname) => {
                    let (rest, value) =
                        state.string_table.clone().borrow_mut().parse_value(&qname)(input)?;
                    (rest, Event::Attribute { qname, value })
                }
                ParseEvent::SEQname(qname) => {
                    let egs = state.element_grammars.borrow_mut();
                    if let Some(eg) = egs.get(&qname) {
                        eg.borrow_mut().reset();
                        grammar_stack.push(eg.clone());
                    } else {
                        panic!("SEQname can't exist without a global element grammar for it");
                    }
                    (input, Event::StartElement { qname })
                }
                e => unimplemented!("Event {:?} unimplemented", e),
            };
        input = rest;
        log::debug!("output event: {:?}", parsed_event);
        output.push(parsed_event);
        // TODO: bit gross?
        if output[output.len() - 1] == Event::EndDocument {
            log::debug!("Reached end of document, parser done");
            break;
        }
    }
    Ok((input, output))
}

/// An EXI stream, consisting of a `header` and `body` made up of a vec of EXI events.
pub struct Stream {
    pub header: Header,
    pub body: Vec<Event>,
}

fn stream<'a>(i: BitInput<'a>) -> ExiResult<BitInput<'a>, Stream> {
    let (rest, header) = header(i)?;
    log::info!(
        "Read EXI header, version {:?}, options: {:?}",
        header.version,
        header.options
    );
    let (rest2, body) = body(rest)?;
    // Clean up any trailing bits
    let (rest3, trailing) = trailing_bits(rest2).map_err(nom::Err::convert)?;
    if trailing != 0 {
        log::warn!("input had non-zero trailing bits!");
    }
    Ok((rest3, Stream { header, body }))
}

/// Decode an EXI stream from input `i`.
pub fn decode(i: &[u8]) -> Result<Stream, Box<dyn std::error::Error>> {
    // "Lovely" little nested map to allow us to return unused input with a different
    // lifetime to `i``
    let (_, s) =
        bits::<_, _, ExiError<(&[u8], usize)>, ExiError<&[u8]>, _>(all_consuming(stream))(i)
            .map_err(|e| e.map(|e2| e2.map_input(|i| i.to_owned())))?;
    Ok(s)
}

// The state of the decoder. Includes the EXI header options (immutable) and string table
// (mutable - added to as the decoder progresses)
struct DecoderState {
    options: Rc<Options>,
    string_table: Rc<RefCell<StringTable>>,
    element_grammars: RefCell<HashMap<Qname, Rc<RefCell<dyn GrammaryThing>>>>,
}

impl DecoderState {
    fn new() -> Self {
        Self {
            options: Rc::new(Options::default()),
            string_table: Rc::new(RefCell::new(StringTable::default())),
            element_grammars: RefCell::new(HashMap::new()),
        }
    }
}

// Each of the possible things an event code can be mapped to. Corresponds to members of
// the Event enum.
#[derive(Clone, Debug)]
enum ParseEvent {
    SD,
    ED,
    SEQname(Qname),
    SEUri,
    SE,
    EE,
    ATQname(Qname),
    ATUri,
    AT,
    CH,
    NS,
    CM,
    PI,
    DT,
    ER,
    SC,
}

impl Display for ParseEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::SD => "SD",
            Self::ED => "ED",
            Self::SEQname(q) => &format!("SE ( {} )", q),
            Self::SEUri => "SE (uri:*)", // TODO: implement when seuri implemented
            Self::SE => "SE",
            Self::EE => "EE",
            Self::ATQname(q) => &format!("AT ( {} )", q),
            Self::ATUri => "AT (uri:*)", // TODO: implement when aturi implemented
            Self::AT => "AT",
            Self::CH => "CH",
            Self::NS => "NS",
            Self::CM => "CM",
            Self::PI => "PI",
            Self::DT => "DT",
            Self::ER => "ER",
            Self::SC => "SC",
        };
        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Read, path::PathBuf};

    use super::*;
    use test_log::test;

    #[test]
    fn test_version() {
        // Simple
        assert_eq!(
            Version::parse((&[0b1000_0000u8], 0)),
            Ok((
                (vec!(0b1000_0000u8).as_slice(), 5usize),
                Version::Preview(1)
            )),
        );
        assert_eq!(
            Version::parse((&[0b0111_0000], 0)),
            Ok(((vec!(0b0111_0000).as_slice(), 5usize), Version::Final(15))),
        );
        // Multi-nybble
        assert_eq!(
            Version::parse((&[0b0111_1000, 0b0111_1111], 0)),
            Ok(((vec!(0b0111_1111).as_slice(), 1usize), Version::Final(16))),
        );
        // Multi-nybble, non byte-aligned
        assert_eq!(
            Version::parse((&[0b1110_0111], 3)), // Start from bit 3
            Ok(((vec!().as_slice(), 0usize), Version::Final(8))),
        );
    }

    // #[test]
    // fn test_header() {
    //     let raw = "$EXI".as_bytes().to_vec();
    //     raw.extend([0b0]);
    //     match header(&raw) {
    //         Ok((res, header)) => {
    //             assert_eq!(res, []);
    //             assert_eq!(header, Header{});
    //         },
    //         Err(e) => {
    //             panic!("{}", e);
    //         }
    //     }
    //

    #[test]
    fn helloworld() -> Result<(), Box<dyn std::error::Error>> {
        let d: PathBuf = [env!("CARGO_MANIFEST_DIR"), "test", "helloworld.xml.exi"]
            .iter()
            .collect();
        let mut buf = Vec::new();
        File::open(d)?.read_to_end(&mut buf)?;
        let s = decode(&buf)?;
        assert_eq!(
            s.body,
            vec!(
                Event::StartDocument,
                Event::StartElement {
                    qname: "hello".into()
                },
                Event::Characters {
                    value: "world".into()
                },
                Event::EndElement,
                Event::EndDocument,
            )
        );
        Ok(())
    }

    #[test]
    fn notebook() -> Result<(), Box<dyn std::error::Error>> {
        let d: PathBuf = [env!("CARGO_MANIFEST_DIR"), "test", "notebook.xml.exi"]
            .iter()
            .collect();
        let mut buf = Vec::new();
        File::open(d)?.read_to_end(&mut buf)?;
        let s = decode(&buf)?;
        assert_eq!(
            s.body,
            vec!(
                Event::StartDocument,
                Event::StartElement {
                    qname: "notebook".into()
                },
                Event::Attribute {
                    qname: "date".into(),
                    value: "2007-09-12".into()
                },
                Event::StartElement {
                    qname: "note".into()
                },
                Event::Attribute {
                    qname: "category".into(),
                    value: "EXI".into()
                },
                Event::Attribute {
                    qname: "date".into(),
                    value: "2007-07-23".into()
                },
                Event::StartElement {
                    qname: "subject".into()
                },
                Event::Characters {
                    value: "EXI".into()
                },
                Event::EndElement,
                Event::StartElement {
                    qname: "body".into()
                },
                Event::Characters {
                    value: "Do not forget it!".into()
                },
                Event::EndElement,
                Event::EndElement,
                Event::StartElement {
                    qname: "note".into()
                },
                Event::Attribute {
                    qname: "date".into(),
                    value: "2007-09-12".into()
                },
                Event::StartElement {
                    qname: "subject".into()
                },
                Event::Characters {
                    value: "Shopping List".into()
                },
                Event::EndElement,
                Event::StartElement {
                    qname: "body".into()
                },
                Event::Characters {
                    value: "milk, honey".into()
                },
                Event::EndElement,
                Event::EndElement,
                Event::EndElement,
                Event::EndDocument,
            )
        );
        Ok(())
    }
}
