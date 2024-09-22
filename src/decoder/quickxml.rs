use quick_xml::events::{BytesEnd, BytesStart, Event as qEvent};

use super::{datatypes::Qname, Event};

// impl<'a> Into<quick_xml::name::QName<'a>> for Qname {
//     fn into(self) -> quick_xml::name::QName<'a> {
//         quick_xml::name::QName(self.to_string().as_bytes())
//     }
// }

/// Struct implementing iterative mapping from [`crate::decoder::Event`] to [`quick_xml::events::Event`].
pub struct QuickXMLEventMapper<'a, T> {
    event_iter: &T,
    state: MapperState<'a>,
    end_stack: Vec<BytesEnd<'a>>,
}

impl<'a, T> From<T> for QuickXMLEventMapper<'a, T>
where
    T: Iterator<Item = Event>,
{
    fn from(value: T) -> Self {
        Self {
            event_iter: value,
            state: MapperState::Start,
            end_stack: vec![],
        }
    }
}

// The state of the mapping from
enum MapperState<'a> {
    Start,
    TagStarted(BytesStart<'a>),
}

impl<'a, T> Iterator for QuickXMLEventMapper<'a, T>
where
    T: Iterator<Item = Event>,
{
    type Item = qEvent<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        for ev in self.event_iter {
            let mut to_emit = None;
            match ev {
                // TODO: maybe check we only get one start/end?
                Event::StartDocument => continue,
                Event::EndDocument => return None,
                Event::StartElement(qname) => {
                    let new_start = BytesStart::new(qname.to_string());
                    // Push end tag onto stack
                    self.end_stack.push(BytesEnd::new(qname.to_string()));
                    if let MapperState::TagStarted(b) = self.state {
                        to_emit = Some(qEvent::Start(b));
                    };
                    self.state = MapperState::TagStarted(new_start);
                }
                Event::Attribute { qname, value } => {
                    if let MapperState::TagStarted(b) = self.state {
                        self.state =
                            MapperState::TagStarted(b.with_attributes([
                                (qname.to_string().as_str(), value.to_string().as_str()),
                            ]));
                    } else {
                        panic!("Got attributes while not in starttag");
                    }
                }
                Event::EndElement => {
                    return Some(qEvent::End(
                        self.end_stack
                            .pop()
                            .expect("More EndElement tags than StartElement"),
                    ))
                }
                Event::Characters(_) => todo!(),
                Event::Doctype {
                    name,
                    public,
                    system,
                    text,
                } => todo!(),
                Event::Comment(_) => todo!(),
                Event::NamespaceDeclaration {
                    uri,
                    prefix,
                    local_e_ns,
                } => todo!(),
                Event::ProcessingInstruction { name, text } => todo!(),
                Event::EntityReference(_) => todo!(),
                Event::SelfContained => todo!(),
            }
            if to_emit.is_some() {
                return to_emit;
            }
        }
        None // We've reached the end of the event stream
    }
}
