use std::rc::Rc;

use nom::IResult;

use crate::util::BitInput;

use super::{datatypes::Qname, options::Options, CodeTree, ParseEvent};

// A grammar is a codetree which contains parseevents and another generic type
pub type Grammar<T: Clone> = CodeTree<(ParseEvent, Option<T>)>;

pub trait GrammaryThing {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> IResult<BitInput<'a>, ParseEvent>;
    fn add_at_specialised(&mut self, _: Qname) {
        unimplemented!();
    }
    fn add_se_specialised(&mut self, _: Qname) {
        unimplemented!();
    }
    fn context_qname(&self) -> Option<&Qname> {
        None
    }
}

pub struct DocumentGrammar {
    document: Grammar<DocumentSubgrammar>,
    doc_content: Grammar<DocumentSubgrammar>,
    doc_end: Grammar<DocumentSubgrammar>,

    current_subgrammar: Option<DocumentSubgrammar>,
}

#[derive(Clone, Debug, PartialEq)]
enum DocumentSubgrammar {
    Document,
    DocContent,
    DocEnd,
}

impl DocumentGrammar {
    pub fn new(o: Rc<Options>) -> Self {
        let mut dc_v = vec![vec![(ParseEvent::SE, Some(DocumentSubgrammar::DocEnd))]];

        let mut dcv_l2 = vec![];
        if o.preserve.dtd {
            dcv_l2.push((ParseEvent::DT, Some(DocumentSubgrammar::DocContent)));
            dc_v.push(dcv_l2.clone());
        }

        if o.preserve.comments || o.preserve.pis {
            dc_v.push(dcv_l2);
            let mut dcv_l3 = vec![];
            if o.preserve.comments {
                dcv_l3.push((ParseEvent::CM, Some(DocumentSubgrammar::DocContent)));
            }
            if o.preserve.pis {
                dcv_l3.push((ParseEvent::PI, Some(DocumentSubgrammar::DocContent)));
            }
            dc_v.push(dcv_l3)
        }

        let mut de_v = vec![vec![(ParseEvent::ED, None)]];
        if o.preserve.comments || o.preserve.pis {
            let mut dev_l2 = vec![];
            if o.preserve.comments {
                dev_l2.push((ParseEvent::CM, Some(DocumentSubgrammar::DocContent)));
            }
            if o.preserve.pis {
                dev_l2.push((ParseEvent::PI, Some(DocumentSubgrammar::DocContent)));
            }
            de_v.push(dev_l2)
        }
        Self {
            document: CodeTree::Terminal((ParseEvent::SD, Some(DocumentSubgrammar::DocContent))),
            doc_content: CodeTree::from_vecs(dc_v),
            doc_end: CodeTree::from_vecs(de_v),
            current_subgrammar: Some(DocumentSubgrammar::Document),
        }
    }
}

impl GrammaryThing for DocumentGrammar {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> IResult<BitInput<'a>, ParseEvent> {
        let (rest, (event, next)) = match self.current_subgrammar {
            Some(DocumentSubgrammar::Document) => self.document.parse(i),
            Some(DocumentSubgrammar::DocContent) => self.doc_content.parse(i),
            Some(DocumentSubgrammar::DocEnd) => self.doc_end.parse(i),
            None => panic!("Grammar has terminated!"),
        }?;
        if self.current_subgrammar != next {
            log::debug!("subgrammar {:?} -> {:?}", self.current_subgrammar, next);
            self.current_subgrammar = next;
        }
        Ok((rest, event))
    }

    fn add_se_specialised(&mut self, _: Qname) {
        // Currently we call this in our main body parser because we don't differentiate
        // between grammar types, so we just ignore it.
    }
}

pub struct FragmentGrammar {
    fragment: Grammar<FragmentSubgrammar>,
    fragment_content: Grammar<FragmentSubgrammar>,

    current_subgrammar: Option<FragmentSubgrammar>,
}

#[derive(Clone)]
enum FragmentSubgrammar {
    Fragment,
    FragmentContent,
}

impl FragmentGrammar {
    pub fn new() -> Self {
        Self {
            fragment: CodeTree::Terminal((
                ParseEvent::SD,
                Some(FragmentSubgrammar::FragmentContent),
            )),
            fragment_content: CodeTree::Node {
                left: vec![
                    (ParseEvent::SE, Some(FragmentSubgrammar::FragmentContent)),
                    (ParseEvent::ED, None),
                ],
                right: Rc::new(CodeTree::Node {
                    left: vec![(ParseEvent::CM, Some(FragmentSubgrammar::FragmentContent))],
                    right: Rc::new(CodeTree::Terminal((
                        ParseEvent::PI,
                        Some(FragmentSubgrammar::FragmentContent),
                    ))),
                }),
            },
            current_subgrammar: Some(FragmentSubgrammar::Fragment),
        }
    }
}
impl GrammaryThing for FragmentGrammar {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> IResult<BitInput<'a>, ParseEvent> {
        let (rest, (event, next)) = match self.current_subgrammar {
            Some(FragmentSubgrammar::Fragment) => self.fragment.parse(i),
            Some(FragmentSubgrammar::FragmentContent) => self.fragment_content.parse(i),
            None => panic!("Grammar has terminated!"),
        }?;
        self.current_subgrammar = next;
        Ok((rest, event))
    }
}

pub struct ElementGrammar {
    for_qname: Qname,
    start_tag_content: Grammar<ElementSubgrammar>,
    element_content: Grammar<ElementSubgrammar>,

    current_subgrammar: Option<ElementSubgrammar>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElementSubgrammar {
    StartTagContent,
    ElementContent,
    Fragment, // TODO: fuck
}

impl ElementGrammar {
    pub fn new(qname: Qname, o: Rc<Options>) -> Self {
        // TODO: have a means to factor out common grammar macro here (childcontentitems)
        // and attach to separate codetrees.
        let mut stcv = vec![vec![]];
        let mut stc_l2 = vec![
            (ParseEvent::EE, None),
            (ParseEvent::AT, Some(ElementSubgrammar::StartTagContent)),
        ];
        if o.preserve.prefixes {
            stc_l2.push((ParseEvent::NS, Some(ElementSubgrammar::StartTagContent)));
        }
        if o.self_contained {
            stc_l2.push((ParseEvent::SC, Some(ElementSubgrammar::Fragment)))
        }
        stc_l2.append(&mut vec![
            (ParseEvent::SE, Some(ElementSubgrammar::ElementContent)),
            (ParseEvent::CH, Some(ElementSubgrammar::ElementContent)),
        ]);
        if o.preserve.dtd {
            stc_l2.push((ParseEvent::ER, Some(ElementSubgrammar::ElementContent)))
        }
        stcv.push(stc_l2);

        if o.preserve.comments || o.preserve.pis {
            let mut stc_l3 = vec![];
            if o.preserve.comments {
                stc_l3.push((ParseEvent::CM, Some(ElementSubgrammar::ElementContent)));
            }
            if o.preserve.pis {
                stc_l3.push((ParseEvent::PI, Some(ElementSubgrammar::ElementContent)));
            }
            stcv.push(stc_l3)
        }

        let mut ecv = vec![vec![(ParseEvent::EE, None)]];

        let mut ec_l2 = vec![
            (ParseEvent::SE, Some(ElementSubgrammar::ElementContent)),
            (ParseEvent::CH, Some(ElementSubgrammar::ElementContent)),
        ];
        if o.preserve.dtd {
            ec_l2.push((ParseEvent::ER, Some(ElementSubgrammar::ElementContent)))
        }
        ecv.push(ec_l2);

        if o.preserve.comments || o.preserve.pis {
            let mut ec_l3 = vec![];
            if o.preserve.comments {
                ec_l3.push((ParseEvent::CM, Some(ElementSubgrammar::ElementContent)));
            }
            if o.preserve.pis {
                ec_l3.push((ParseEvent::PI, Some(ElementSubgrammar::ElementContent)));
            }
            ecv.push(ec_l3)
        }

        Self {
            for_qname: qname,
            start_tag_content: CodeTree::from_vecs(stcv),
            element_content: CodeTree::from_vecs(ecv),
            current_subgrammar: Some(ElementSubgrammar::StartTagContent),
        }
    }
}

impl GrammaryThing for ElementGrammar {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> IResult<BitInput<'a>, ParseEvent> {
        let (rest, (event, next)) = match self.current_subgrammar {
            Some(ElementSubgrammar::StartTagContent) => self.start_tag_content.parse(i),
            Some(ElementSubgrammar::ElementContent) => self.element_content.parse(i),
            Some(ElementSubgrammar::Fragment) => panic!("Can't do selfcontained fragments soz"),
            None => panic!("Grammar has terminated!"),
        }?;
        if self.current_subgrammar != next {
            log::debug!("subgrammar {:?} -> {:?}", self.current_subgrammar, next);
            self.current_subgrammar = next;
        }
        Ok((rest, event))
    }

    fn add_at_specialised(&mut self, qname: Qname) {
        // TODO: support xsi:type.
        // See https://www.w3.org/TR/exi/#builtinElemGrammars semantics
        self.start_tag_content.insert(
            0,
            (
                ParseEvent::ATQname(qname),
                Some(ElementSubgrammar::StartTagContent),
            ),
        );
    }

    fn add_se_specialised(&mut self, qname: Qname) {
        self.start_tag_content.insert(
            0,
            (
                ParseEvent::SEQname(qname),
                Some(ElementSubgrammar::StartTagContent),
            ),
        );
    }

    fn context_qname(&self) -> Option<&Qname> {
        Some(&self.for_qname)
    }
}
