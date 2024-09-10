mod datatypes;
mod grammars;
mod options;

use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use datatypes::{
    n_bit_unsigned_int, parse_string_with_len_offset, qname, unsigned_int_x, Qname, Value,
};
use grammars::{DocumentGrammar, ElementGrammar, GrammaryThing};
use nom::branch::alt;
use nom::combinator::{all_consuming, map, success};
use nom::{
    bits::{
        bits,
        complete::{bool, tag, take},
    },
    combinator::opt,
    error::Error,
    sequence::{preceded, tuple},
    IResult,
};
use options::Options;

use crate::util::{ilog2_ceil, trailing_bits, BitInput};

/// The EXI stream header. Contains the format `version` and any `options`.
#[derive(Debug, PartialEq)]
pub struct Header {
    pub version: Version,
    pub options: Option<Options>,
}

struct PrefixTable<T>(Vec<T>);

impl<T> PrefixTable<T> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn add(&mut self, i: T) -> usize {
        self.0.push(i);
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
        ilog2_ceil(self.len() + 1)
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
        let s = Rc::new(value);
        self.global_values.add(s.clone());
        self.local_values
            .entry(qname)
            .or_insert(PrefixTable::new())
            .add(s);
    }

    // Using the string table, parse a URI from the bistream
    // URI partitions are "Optimized for Frequent use of Compact Identifiers" - the
    // compact identifiers are n bit integers where n = ceil(log2(number_of_entries + 1))
    fn parse_uri<'a, 'b>(
        &'b mut self,
    ) -> impl FnMut(BitInput<'a>) -> IResult<BitInput<'a>, String> + 'b {
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
                log::debug!("parse_uri string table miss, added {} to table", uri);
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
                log::debug!("parse_uri string table hit, got '{}'", u);
                Ok((rest, u))
            }
        }
    }

    fn parse_prefix<'a>(&mut self) -> impl FnMut(BitInput<'a>) -> IResult<BitInput<'a>, String> {
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
    ) -> impl FnMut(BitInput<'a>) -> IResult<BitInput<'a>, String> + 'b {
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
                return Ok((rest, s));
            }
            // Miss - a string with length incremented by 1
            let (rest, s) = parse_string_with_len_offset(1)(i)?;
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
    ) -> impl FnMut(BitInput<'a>) -> IResult<BitInput<'a>, Value> + 'b {
        move |i| {
            if let Ok((rest1, foo)) = alt((unsigned_int_x(0), unsigned_int_x(1)))(i) {
                match foo {
                    0 => {
                        let (rest2, idx) = n_bit_unsigned_int(
                            self.local_values.get(&qname).unwrap().prefix_length(),
                            true,
                        )(rest1)?;
                        Ok((
                            rest2,
                            Rc::as_ref(
                                self.local_values
                                    .get(&qname)
                                    .unwrap()
                                    .get(idx as usize)
                                    .unwrap(),
                            )
                            .clone(),
                        ))
                    }
                    1 => {
                        let (rest2, idx) =
                            n_bit_unsigned_int(self.global_values.prefix_length(), true)(rest1)?;
                        Ok((
                            rest2,
                            Rc::as_ref(self.global_values.get(idx as usize).unwrap()).clone(),
                        ))
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

fn header<'a>(i: BitInput<'a>) -> IResult<BitInput<'a>, Header> {
    let (rem, (options_present, ver)) = preceded(
        opt(tag(0x24455849, 32usize)),
        preceded(tag(0b10, 2usize), tuple((bool, Version::parse))),
    )(i)?;
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

fn body(i: BitInput) -> IResult<BitInput, Vec<Event>> {
    let state = Rc::new(DecoderState::new());
    let mut grammar_stack: VecDeque<Rc<RefCell<dyn GrammaryThing>>> = VecDeque::new();
    grammar_stack.push_front(Rc::new(RefCell::new(DocumentGrammar::new(
        state.options.clone(),
    ))));
    let mut output = Vec::new();
    let mut input = i;
    loop {
        let g = &grammar_stack[0];
        let (rest, event) = g.borrow_mut().parse(input)?;
        input = rest;
        log::debug!("ParseEvent: {:?}", event);
        let (rest, parsed_event) = match event {
            ParseEvent::SD => success(Event::StartDocument)(input)?,
            ParseEvent::SE => {
                let (r1, qname) =
                    qname(state.string_table.clone(), state.options.preserve.prefixes)(input)?;
                // Add a SE ( qname ) production to the current grammar
                grammar_stack[0]
                    .borrow_mut()
                    .add_se_specialised(qname.clone());
                let mut egs = state.element_grammars.borrow_mut();
                if let Some(eg) = egs.get(&qname) {
                    grammar_stack.push_front(eg.clone());
                } else {
                    log::debug!("creating new global element grammar for qname {:?}", qname);
                    let ng = Rc::new(RefCell::new(ElementGrammar::new(
                        qname.clone(),
                        state.options.clone(),
                    )));
                    egs.insert(qname.clone(), ng.clone());
                    grammar_stack.push_front(ng);
                }
                (r1, Event::StartElement { qname })
            }
            ParseEvent::CH => {
                let (r, value) = state
                    .string_table
                    .clone()
                    .borrow_mut()
                    .parse_value(g.borrow().context_qname().unwrap())(
                    input
                )?;
                (r, Event::Characters { value })
            }
            ParseEvent::EE => {
                grammar_stack.pop_front();
                success(Event::EndElement)(input)?
            }
            ParseEvent::ED => success(Event::EndDocument)(input)?,
            ParseEvent::AT => {
                // Parse the qname
                let (r1, qname) =
                    qname(state.string_table.clone(), state.options.preserve.prefixes)(input)?;
                // Parse the value
                let (rest, value) =
                    state
                        .string_table
                        .clone()
                        .borrow_mut()
                        .parse_value(g.borrow().context_qname().unwrap())(r1)?;
                // Add to this element's grammar
                let _ = &grammar_stack[0]
                    .borrow_mut()
                    .add_at_specialised(qname.clone());
                (rest, Event::Attribute { qname, value })
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

fn stream<'a>(i: BitInput<'a>) -> IResult<BitInput<'a>, Stream> {
    let (rest, header) = header(i)?;
    log::info!(
        "Read EXI header, version {:?}, options: {:?}",
        header.version,
        header.options
    );
    let (rest2, body) = body(rest)?;
    // Clean up any trailing bits
    let (rest3, trailing) = trailing_bits(rest2)?;
    if trailing != 0 {
        log::warn!("input had non-zero trailing bits!");
    }
    Ok((rest3, Stream { header, body }))
}

/// Decode an EXI stream from input `i`.
pub fn decode(i: &[u8]) -> Result<Stream, Box<dyn std::error::Error>> {
    // lol
    let (rest, s) = bits::<_, _, Error<(&[u8], usize)>, Error<&[u8]>, _>(all_consuming(stream))(i)
        .map_err(|e| e.to_owned())?;
    assert_eq!(rest.len(), 0);
    Ok(s)
}

// Describes the tree of event codes in an EXI grammar.
// `CodeTree<T>`s always:
// - Consist of 1+ levels
// - Each level having 0+ left `T`s
// - And have exactly one right entry, which is either another CodeTree, or a terminal `T``
#[derive(Clone, PartialEq, Debug)]
enum CodeTree<T: Clone> {
    Terminal(T),
    Node {
        left: Vec<T>,
        right: Rc<CodeTree<T>>,
    },
}

impl<T: Clone> CodeTree<T> {
    // Convenience function to build a codetree from a series of vecs, with each vec
    // containing elements to add at a "layer" of the codetree.
    fn from_vecs(mut vecs: Vec<Vec<T>>) -> Self {
        let this = vecs.remove(0);
        let islast = vecs.is_empty();
        match (this.len(), islast) {
            (l @ _, false) => CodeTree::Node {
                left: this,
                right: Rc::new(CodeTree::from_vecs(vecs)),
            },
            (0, true) => {
                panic!("Trailing 0-length codetree layers are invalid")
            }
            (1, true) => CodeTree::Node {
                left: vec![],
                right: Rc::new(CodeTree::Terminal(this[0].to_owned())),
            },
            (l @ 2.., true) => CodeTree::Node {
                left: this[0..l - 1].to_owned(),
                right: Rc::new(CodeTree::Terminal(this[l - 1].to_owned())),
            },
        }
    }
}

impl<T: Clone> CodeTree<T> {
    fn len(&self) -> Vec<usize> {
        match self {
            Self::Terminal(_) => {
                vec![1]
            }
            Self::Node { left, right } => {
                let slen = left.len() + 1;
                if let Self::Terminal(_) = right.as_ref() {
                    vec![slen]
                } else {
                    let mut v = right.len();
                    v.insert(0, slen);
                    v
                }
            }
        }
    }

    // The minimum number of bits needed to represent each component of the code
    // For a code with lengths (2, 3) this would be (1, 2). If the code has just one
    // terminal, thus having a length (1,), the bits required would be (0,) - a single
    // possiblity is encoded in 0 bits.
    fn bits(&self) -> Vec<u32> {
        self.len().into_iter().map(ilog2_ceil).collect()
    }

    // Insert an element at the index described by `index`. Returns a copy of codetree
    // with the element inserted
    fn insert(&mut self, index: usize, v: T) -> Self {
        match (self, index) {
            // Turn a terminal into a node, adding to the left
            (CodeTree::Terminal(i), 0) => CodeTree::Node {
                left: vec![v],
                right: Rc::new(CodeTree::Terminal(i.clone())),
            },
            // Turn a terminal into a node, adding to the right
            (CodeTree::Terminal(i), 1) => CodeTree::Node {
                left: vec![i.clone()],
                right: Rc::new(CodeTree::Terminal(v)),
            },
            // Given a terminal is just one node, any other insert index is invalid
            (CodeTree::Terminal(_), _) => panic!("Can't insert"),
            // Modify a node, inserting somewhere in the left hand side
            (CodeTree::Node { left, right }, idx) => {
                if index > left.len() {
                    panic!("can't insert");
                }
                let mut l = left.clone();
                l.insert(idx, v);
                CodeTree::Node {
                    left: left.clone(),
                    right: right.clone(),
                }
            }
        }
    }
}

impl<T: Clone> CodeTree<T> {
    // Given this codetree, produce the next event from bitstream i
    fn parse<'a>(&self, i: BitInput<'a>) -> IResult<BitInput<'a>, T> {
        match self {
            // If we only have one entry there's only one possiblity
            CodeTree::Terminal(e) => {
                log::debug!("codetree parsed terminal, no input consumed");
                Ok((i, e.clone()))
            }
            // we have multiple entries - parse the number of bits needed and return the
            // matching one
            CodeTree::Node { left, right } => {
                let bits = self.bits()[0];
                log::debug!("codetree getting {}-bit prefix", bits);
                let (rest, idx): (_, usize) = take(bits)(i)?;
                if idx == left.len() {
                    right.parse(rest)
                } else if idx < left.len() {
                    Ok((rest, left.get(idx).unwrap().clone()))
                } else {
                    panic!("Bad index {} for len {}", idx, self.len()[0]);
                }
            }
        }
    }
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
    // }

    #[test]
    fn test_code_tree_len_bits() {
        let t = CodeTree::Node {
            left: vec![Event::EndElement],
            right: Rc::new(CodeTree::Terminal(Event::EndDocument)),
        };
        assert_eq!(t.len(), vec!(2));
        assert_eq!(t.bits(), vec!(1));

        let t = CodeTree::Terminal(Event::EndDocument);
        assert_eq!(t.len(), vec!(1));
        assert_eq!(t.bits(), vec!(0));

        let t = CodeTree::Node {
            left: vec![Event::EndDocument, Event::EndDocument, Event::EndDocument],
            right: Rc::new(CodeTree::Node {
                left: vec![Event::EndDocument],
                right: Rc::new(CodeTree::Node {
                    left: vec![Event::EndDocument, Event::EndDocument],
                    right: Rc::new(CodeTree::Terminal(Event::EndDocument)),
                }),
            }),
        };
        assert_eq!(t.len(), vec!(4, 2, 3));
        assert_eq!(t.bits(), vec! {2, 1, 2});
    }

    #[test]
    fn test_codetree_parse() {
        // Simple, 0-bit case.
        assert_eq!(
            CodeTree::Terminal(1).parse((&[0b1010_1010], 0)),
            Ok(((vec!(0b1010_1010u8).as_slice(), 0), 1))
        );
        // 1-bit cases
        assert_eq!(
            CodeTree::Node {
                left: vec!(1),
                right: Rc::new(CodeTree::Terminal(2))
            }
            .parse((&[0b0000_0000], 0)),
            Ok(((vec!(0b0000_0000).as_slice(), 1), 1))
        );
        assert_eq!(
            CodeTree::Node {
                left: vec!(1),
                right: Rc::new(CodeTree::Terminal(2))
            }
            .parse((&[0b1000_0000], 0)),
            Ok(((vec!(0b1000_0000).as_slice(), 1), 2))
        );
        // 2-bit case
        assert_eq!(
            CodeTree::Node {
                left: vec!(1, 2),
                right: Rc::new(CodeTree::Terminal(3))
            }
            .parse((&[0b0100_0000], 0)),
            Ok(((vec!(0b0100_0000).as_slice(), 2), 2))
        );

        // (0,1)-bit case
        assert_eq!(
            CodeTree::Node {
                left: vec!(),
                right: Rc::new(CodeTree::Node {
                    left: vec!(1),
                    right: Rc::new(CodeTree::Terminal(2)),
                })
            }
            .parse((&[0b1000_0000], 0)),
            Ok(((vec!(0b1000_0000).as_slice(), 1), 2))
        );
        // (2,2)-bit case
        assert_eq!(
            CodeTree::Node {
                left: vec!(1, 2, 3),
                right: Rc::new(CodeTree::Node {
                    left: vec!(4, 5, 6),
                    right: Rc::new(CodeTree::Terminal(7)),
                })
            }
            .parse((&[0b1110_0000], 0)),
            Ok(((vec!(0b1110_0000).as_slice(), 4), 6))
        );
    }

    #[test]
    fn test_codetree_from_vecs() {
        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1, 2))),
            CodeTree::Node {
                left: vec!(1),
                right: Rc::new(CodeTree::Terminal(2)),
            },
        );
        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1, 2, 3))),
            CodeTree::Node {
                left: vec!(1, 2),
                right: Rc::new(CodeTree::Terminal(3)),
            },
        );

        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1, 2), vec!(3))),
            CodeTree::Node {
                left: vec!(1, 2),
                right: Rc::new(CodeTree::Node {
                    left: vec!(),
                    right: Rc::new(CodeTree::Terminal(3))
                })
            }
        );

        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1), vec!(), vec!(3, 4))),
            CodeTree::Node {
                left: vec!(1),
                right: Rc::new(CodeTree::Node {
                    left: vec!(),
                    right: Rc::new(CodeTree::Node {
                        left: vec!(3),
                        right: Rc::new(CodeTree::Terminal(4)),
                    }),
                })
            }
        );
    }

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
