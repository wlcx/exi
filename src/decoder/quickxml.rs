use std::collections::VecDeque;

use quick_xml::events::{BytesEnd, BytesStart, BytesText, Event as qEvent};

use super::Event;

/// Struct implementing iterative mapping from [`crate::decoder::Event`] to [`quick_xml::events::Event`].
pub struct QuickXMLEventMapper<'a, T> {
    event_iter: T,
    doc_started: bool,
    doc_ended: bool,
    start_tag: Option<BytesStart<'a>>,
    end_stack: Vec<BytesEnd<'a>>,
    output_queue: VecDeque<qEvent<'a>>,
}

impl<'a, T> QuickXMLEventMapper<'a, T> {
    fn new(event_iter: T) -> Self {
        Self {
            event_iter,
            doc_started: false,
            doc_ended: false,
            start_tag: None,
            end_stack: Vec::new(),
            // We shouldn't need more than 2 things in the queue at any one time, since
            // each input event results in either 0, 1, or 2 output events.
            output_queue: VecDeque::with_capacity(2),
        }
    }

    fn process(&mut self, e: Event) {
        match e {
            Event::StartDocument => {
                if self.doc_started {
                    panic!("Got multiple startdocuments in event stream.")
                }
                self.doc_started = true;
            }
            // We're done - stop iterating
            Event::EndDocument => {
                self.doc_ended = true;
            }
            Event::StartElement(qname) => {
                let new_start = BytesStart::new(qname.clone().to_string());
                // Push end tag onto stack
                self.end_stack.push(BytesEnd::new(qname.to_string()));
                // If we have parent start tag contents already, it's now complete and
                // ready to emit.
                if let Some(b) = std::mem::replace(&mut self.start_tag, Some(new_start)) {
                    self.output_queue.push_back(qEvent::Start(b));
                }
            }
            Event::Attribute { qname, value } => {
                if let Some(ref mut b) = self.start_tag {
                    // Add the attribute
                    let v: String = value.into();
                    b.extend_attributes([(qname.to_string().as_str(), v.as_str())]);
                } else {
                    panic!("Got attributes while not in starttag");
                }
            }
            Event::EndElement => {
                // Finish an open start tag (i.e. this was an empty element)
                if let Some(b) = std::mem::take(&mut self.start_tag) {
                    self.output_queue.push_back(qEvent::Start(b));
                }
                self.output_queue.push_back(qEvent::End(
                    self.end_stack
                        .pop()
                        .expect("More EndElement tags than StartElement"),
                ))
            }
            Event::Characters(v) => {
                // Finish an open start tag
                if let Some(b) = std::mem::take(&mut self.start_tag) {
                    self.output_queue.push_back(qEvent::Start(b));
                }
                let vs: String = v.into();
                self.output_queue
                    .push_back(qEvent::Text(BytesText::new(vs.as_str()).into_owned()))
            }
            Event::Doctype {
                name,
                public,
                system,
                text,
            } => {
                // TODO: is this the right way to construct this?
                self.output_queue.push_back(qEvent::DocType(
                    BytesText::new(format!("{} {} {} {}", name, public, system, text).as_str())
                        .into_owned(),
                ));
            }
            Event::Comment(c) => {
                self.output_queue
                    .push_back(qEvent::Comment(BytesText::new(&c).into_owned()));
            }
            Event::NamespaceDeclaration { .. } => todo!(),
            Event::ProcessingInstruction { .. } => todo!(),
            Event::EntityReference(_) => todo!(),
            Event::SelfContained => todo!(),
        }
    }
}

pub trait QuickXMLIterator<'a, T>: Iterator<Item = T> + Sized {
    fn to_quick_xml(self) -> QuickXMLEventMapper<'a, Self> {
        QuickXMLEventMapper::new(self)
    }
}

impl<'a, T, I: Iterator<Item = T>> QuickXMLIterator<'a, T> for I {}

impl<'a, T> Iterator for QuickXMLEventMapper<'a, T>
where
    T: Iterator<Item = Event>,
{
    type Item = qEvent<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.doc_ended {
            return None;
        }
        loop {
            // If we have any events to emit, emit them first
            if !self.output_queue.is_empty() {
                return self.output_queue.pop_front();
            }
            match self.event_iter.next() {
                Some(e) => self.process(e),
                None => return None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{borrow::Cow, io::Cursor};

    use quick_xml::Writer;

    use super::*;

    #[test]
    fn test_simple() {
        let got = vec![
            Event::StartDocument,
            Event::StartElement("text".into()),
            Event::Characters("hello world".into()),
            Event::EndElement,
            Event::EndDocument,
        ]
        .into_iter()
        .to_quick_xml()
        .collect::<Vec<qEvent>>();
        println!("{:?}", got);
        assert_eq!(
            got,
            vec![
                qEvent::Start(BytesStart::new(Cow::from("text"))),
                qEvent::Text(BytesText::new("hello world")),
                qEvent::End(BytesEnd::new(Cow::from("text"))),
            ]
        )
    }

    #[test]
    fn test_nested() -> Result<(), Box<dyn std::error::Error>> {
        let mut writer = Writer::new(Cursor::new(Vec::new()));
        for ev in vec![
            Event::StartDocument,
            Event::StartElement("text".into()),
            Event::Characters("hello world".into()),
            Event::StartElement("foo".into()),
            Event::Attribute {
                qname: "bar".into(),
                value: "baz".into(),
            },
            Event::EndElement,
            Event::EndElement,
            Event::EndDocument,
        ]
        .into_iter()
        .to_quick_xml()
        {
            writer.write_event(ev)?;
        }
        assert_eq!(
            writer.into_inner().into_inner(),
            r#"<text>hello world<foo bar="baz"></foo></text>"#.as_bytes(),
        );
        Ok(())
    }
}
