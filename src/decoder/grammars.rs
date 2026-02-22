use std::{fmt::Display, rc::Rc};

use crate::util::BitInput;

use super::{
    Event, ExiResult, ParseEvent,
    codetree::{CodeTree, codetree},
    datatypes::Qname,
    errors::{ExiErrorKind, make_exierror},
    options::Options,
};

// TODO: rethink how all this works
// Currently grammar productions map to an enum describing the *next* grammar to use. This
// was easier to implement, but results in a bunch of extra code - it would be much neater
// for each production to map to a reference to the next grammar to use (which may be
// self-referential).

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Production<T>(ParseEvent, Option<T>);

impl<T> From<(ParseEvent, Option<T>)> for Production<T> {
    fn from(value: (ParseEvent, Option<T>)) -> Self {
        Self(value.0, value.1)
    }
}

impl<T: Display> Display for Production<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)?;
        if let Some(rhs) = &self.1 {
            write!(f, "\t\t")?;
            rhs.fmt(f)?;
        }
        Ok(())
    }
}
// A grammar is a codetree which contains parseevents and another generic type
pub(super) type Grammar<T> = CodeTree<Production<T>>;

pub trait GrammaryThing {
    // Parse some bits from `i` and evaluate the matching production in the grammar
    fn parse<'a>(&mut self, i: BitInput<'a>) -> ExiResult<BitInput<'a>, ParseEvent>;

    // The fully parsed event is fed back to the grammar to allow it to specialise if it
    // wants to.
    fn specialise(&mut self, ev: &Event);

    // Put the grammar back to its initial state
    fn reset(&mut self);

    // A somewhat useful short summary of the grammar
    fn describe(&self) -> String;

    fn context_qname(&self) -> Option<&Qname>;
    fn pprint(&self);
}

#[derive(Clone)]
pub(crate) enum GrammarEnum {
    Document(DocumentGrammar),
    Element(ElementGrammar),
    Fragment(FragmentGrammar),
}

impl GrammarEnum {
    pub fn parse<'a>(&mut self, i: BitInput<'a>) -> ExiResult<BitInput<'a>, ParseEvent> {
        match self {
            Self::Document(d) => d.parse(i),
            Self::Element(d) => d.parse(i),
            Self::Fragment(d) => d.parse(i),
        }
    }

    pub fn specialise(&mut self, ev: &Event) {
        match self {
            Self::Document(d) => d.specialise(ev),
            Self::Element(d) => d.specialise(ev),
            Self::Fragment(d) => d.specialise(ev),
        }
    }

    pub fn reset(&mut self) {
        match self {
            Self::Document(d) => d.reset(),
            Self::Element(d) => d.reset(),
            Self::Fragment(d) => d.reset(),
        }
    }

    pub fn describe(&self) -> String {
        match self {
            Self::Document(d) => d.describe(),
            Self::Element(d) => d.describe(),
            Self::Fragment(d) => d.describe(),
        }
    }

    pub fn context_qname(&self) -> Option<&Qname> {
        match self {
            Self::Document(d) => d.context_qname(),
            Self::Element(d) => d.context_qname(),
            Self::Fragment(d) => d.context_qname(),
        }
    }

    pub fn pprint(&self) {
        match self {
            Self::Document(d) => d.pprint(),
            Self::Element(d) => d.pprint(),
            Self::Fragment(d) => d.pprint(),
        }
    }
}

#[derive(Clone)]
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

impl Display for DocumentSubgrammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl DocumentGrammar {
    pub fn new(o: Rc<Options>) -> Self {
        let mut dc_v = vec![vec![
            (ParseEvent::SE, Some(DocumentSubgrammar::DocEnd)).into(),
        ]];

        let mut dcv_l2 = vec![];
        if o.preserve.dtd {
            dcv_l2.push((ParseEvent::DT, Some(DocumentSubgrammar::DocContent)).into());
            dc_v.push(dcv_l2.clone());
        }

        if o.preserve.comments || o.preserve.pis {
            dc_v.push(dcv_l2);
            let mut dcv_l3 = vec![];
            if o.preserve.comments {
                dcv_l3.push((ParseEvent::CM, Some(DocumentSubgrammar::DocContent)).into());
            }
            if o.preserve.pis {
                dcv_l3.push((ParseEvent::PI, Some(DocumentSubgrammar::DocContent)).into());
            }
            dc_v.push(dcv_l3)
        }

        let mut de_v = vec![vec![(ParseEvent::ED, None).into()]];
        if o.preserve.comments || o.preserve.pis {
            let mut dev_l2 = vec![];
            if o.preserve.comments {
                dev_l2.push((ParseEvent::CM, Some(DocumentSubgrammar::DocContent)).into());
            }
            if o.preserve.pis {
                dev_l2.push((ParseEvent::PI, Some(DocumentSubgrammar::DocContent)).into());
            }
            de_v.push(dev_l2)
        }
        Self {
            document: CodeTree::Terminal(
                (ParseEvent::SD, Some(DocumentSubgrammar::DocContent)).into(),
            ),
            doc_content: CodeTree::from_vecs(dc_v),
            doc_end: CodeTree::from_vecs(de_v),
            current_subgrammar: Some(DocumentSubgrammar::Document),
        }
    }
}

impl GrammaryThing for DocumentGrammar {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> ExiResult<BitInput<'a>, ParseEvent> {
        let (rest, Production(event, next)) = match self.current_subgrammar {
            Some(DocumentSubgrammar::Document) => self.document.parse(i),
            Some(DocumentSubgrammar::DocContent) => self.doc_content.parse(i),
            Some(DocumentSubgrammar::DocEnd) => self.doc_end.parse(i),
            None => panic!("Grammar has terminated!"),
        }
        .map_err(nom::Err::convert)?;
        if self.current_subgrammar != next {
            log::trace!("subgrammar {:?} -> {:?}", self.current_subgrammar, next);
            self.current_subgrammar = next;
        }
        Ok((rest, event))
    }

    fn specialise(&mut self, _ev: &Event) {
        // TODO: probably do something
    }

    fn reset(&mut self) {
        self.current_subgrammar = Some(DocumentSubgrammar::Document);
    }

    fn describe(&self) -> String {
        format!("Document({:?})", self.current_subgrammar)
    }

    fn pprint(&self) {
        log::trace!("Document:\n{}", self.document.pprint());
        log::trace!("Doc content:\n{}", self.doc_content.pprint());
        log::trace!("Doc end:\n{}", self.doc_end.pprint());
    }

    fn context_qname(&self) -> Option<&Qname> {
        None
    }
}

#[derive(Clone)]
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
            fragment: CodeTree::Terminal(
                (ParseEvent::SD, Some(FragmentSubgrammar::FragmentContent)).into(),
            ),
            fragment_content: CodeTree::Node {
                left: vec![
                    (ParseEvent::SE, Some(FragmentSubgrammar::FragmentContent)).into(),
                    (ParseEvent::ED, None).into(),
                ],
                right: CodeTree::Node {
                    left: vec![(ParseEvent::CM, Some(FragmentSubgrammar::FragmentContent)).into()],
                    right: CodeTree::Terminal(
                        (ParseEvent::PI, Some(FragmentSubgrammar::FragmentContent)).into(),
                    )
                    .into(),
                }
                .into(),
            },
            current_subgrammar: Some(FragmentSubgrammar::Fragment),
        }
    }
}
impl GrammaryThing for FragmentGrammar {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> ExiResult<BitInput<'a>, ParseEvent> {
        let (rest, Production(event, next)) = match self.current_subgrammar {
            Some(FragmentSubgrammar::Fragment) => self.fragment.parse(i),
            Some(FragmentSubgrammar::FragmentContent) => self.fragment_content.parse(i),
            None => panic!("Grammar has terminated!"),
        }
        .map_err(nom::Err::convert)?;
        self.current_subgrammar = next;
        Ok((rest, event))
    }

    fn pprint(&self) {
        unimplemented!()
    }

    fn specialise(&mut self, _ev: &Event) {
        todo!()
    }

    fn reset(&mut self) {
        self.current_subgrammar = Some(FragmentSubgrammar::Fragment);
    }

    fn describe(&self) -> String {
        todo!()
    }

    fn context_qname(&self) -> Option<&Qname> {
        None
    }
}

#[derive(Clone)]
pub struct ElementGrammar {
    for_qname: Qname,
    start_tag_content: Grammar<ElementSubgrammar>,
    start_tag_content_has_ch: bool,
    element_content: Grammar<ElementSubgrammar>,
    element_content_has_ch: bool,

    current_subgrammar: ElementSubgrammar,
    next_subgrammar: Option<ElementSubgrammar>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ElementSubgrammar {
    StartTagContent,
    ElementContent,
    Fragment, // TODO: fuck
}

impl Display for ElementSubgrammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl ElementGrammar {
    pub fn new(qname: Qname, o: Rc<Options>) -> Self {
        // TODO: have a means to factor out common grammar macro here (childcontentitems)
        // and attach to separate codetrees.
        let stc = codetree!(
            0,0  => Production::from((ParseEvent::EE, None));
            0,0  => (ParseEvent::AT, Some(ElementSubgrammar::StartTagContent)).into();
            0,0  => (ParseEvent::NS, Some(ElementSubgrammar::StartTagContent)).into(), o.preserve.prefixes;
            0,0  => (ParseEvent::SC, Some(ElementSubgrammar::Fragment)).into(), o.self_contained;
            0,0  => (ParseEvent::SE, Some(ElementSubgrammar::ElementContent)).into();
            0,0  => (ParseEvent::CH, Some(ElementSubgrammar::ElementContent)).into();
            0,0  => (ParseEvent::ER, Some(ElementSubgrammar::ElementContent)).into(), o.preserve.dtd;

            0,0,0 => (ParseEvent::CM, Some(ElementSubgrammar::ElementContent)).into(), o.preserve.comments;
            0,0,0 => (ParseEvent::PI, Some(ElementSubgrammar::ElementContent)).into(), o.preserve.pis;
        );

        let ec = codetree!(
            0     => Production::from((ParseEvent::EE, None));
            0,0   => (ParseEvent::SE, Some(ElementSubgrammar::ElementContent)).into();
            0,0   => (ParseEvent::CH, Some(ElementSubgrammar::ElementContent)).into();
            0,0   => (ParseEvent::ER, Some(ElementSubgrammar::ElementContent)).into(), o.preserve.dtd;
            0,0,0 => (ParseEvent::CM, Some(ElementSubgrammar::ElementContent)).into(), o.preserve.comments;
            0,0,0 => (ParseEvent::PI, Some(ElementSubgrammar::ElementContent)).into(), o.preserve.pis;
        );

        Self {
            for_qname: qname,
            start_tag_content: stc,
            start_tag_content_has_ch: false,
            element_content: ec,
            element_content_has_ch: false,
            current_subgrammar: ElementSubgrammar::StartTagContent,
            next_subgrammar: Some(ElementSubgrammar::StartTagContent),
        }
    }
}

impl GrammaryThing for ElementGrammar {
    fn parse<'a>(&mut self, i: BitInput<'a>) -> ExiResult<BitInput<'a>, ParseEvent> {
        // Note the separation between current and next subgrammar - this is so that after
        // we have parsed an event (and next subgrammar may have advanced to a different)
        // subgrammar, we can still use current_subgrammar to add any new productions to
        // the grammar we just used (`add_at_production`/`add_se_production`).
        self.current_subgrammar = self.next_subgrammar.ok_or(nom::Err::Failure(make_exierror(
            i,
            ExiErrorKind::GrammarTerminated,
        )))?;
        let (rest, Production(event, next)) = match self.current_subgrammar {
            ElementSubgrammar::StartTagContent => self.start_tag_content.parse(i),
            ElementSubgrammar::ElementContent => self.element_content.parse(i),
            ElementSubgrammar::Fragment => panic!("Can't do selfcontained fragments soz"),
        }
        .map_err(nom::Err::convert)?;
        self.next_subgrammar = next;
        if Some(self.current_subgrammar) != self.next_subgrammar {
            log::trace!(
                "next subgrammar {:?} -> {:?}",
                self.current_subgrammar,
                next
            );
        }
        Ok((rest, event))
    }

    fn specialise(&mut self, ev: &Event) {
        match (ev, self.current_subgrammar) {
            (Event::StartElement(qname), current) => {
                let new: Production<ElementSubgrammar> = (
                    ParseEvent::SEQname(qname.clone()),
                    Some(ElementSubgrammar::ElementContent),
                )
                    .into();
                // Sanity check: there shouldn't be matching SEQname in the grammar already
                assert!(
                    self.element_content
                        .iter()
                        .position(|e| *e == new)
                        .is_none()
                );
                match current {
                    ElementSubgrammar::ElementContent => {
                        self.element_content = self.element_content.insert(0, new);
                    }
                    ElementSubgrammar::StartTagContent => {
                        self.start_tag_content = self.start_tag_content.insert(0, new);
                    }
                    ElementSubgrammar::Fragment => unimplemented!(),
                }
            }
            (Event::Attribute { qname, .. }, ElementSubgrammar::StartTagContent) => {
                self.start_tag_content = self.start_tag_content.insert(
                    0,
                    (
                        ParseEvent::ATQname(qname.clone()),
                        Some(ElementSubgrammar::StartTagContent),
                    )
                        .into(),
                )
            }
            (Event::Characters { .. }, current) => {
                // Handle adding short CH productions only once. The *_has_ch is a bit
                // gross, perhaps looking up in the codetree would be neater?
                match current {
                    ElementSubgrammar::StartTagContent => {
                        if !self.start_tag_content_has_ch {
                            self.start_tag_content = self.start_tag_content.insert(
                                0,
                                (ParseEvent::CH, Some(ElementSubgrammar::ElementContent)).into(),
                            );
                            self.start_tag_content_has_ch = true;
                        }
                    }
                    ElementSubgrammar::ElementContent => {
                        if !self.element_content_has_ch {
                            self.element_content = self.element_content.insert(
                                0,
                                (ParseEvent::CH, Some(ElementSubgrammar::ElementContent)).into(),
                            );
                            self.element_content_has_ch = true;
                        }
                    }
                    ElementSubgrammar::Fragment => unimplemented!(),
                }
            }
            (Event::Attribute { .. }, _) => {
                panic!("Shouldn't be attributing in other subgrammars...");
            }
            _ => {}
        }
    }

    fn reset(&mut self) {
        self.current_subgrammar = ElementSubgrammar::StartTagContent;
        self.next_subgrammar = Some(ElementSubgrammar::StartTagContent);
    }

    fn context_qname(&self) -> Option<&Qname> {
        Some(&self.for_qname)
    }

    fn describe(&self) -> String {
        format!("Element{}({})", self.for_qname, self.current_subgrammar)
    }

    fn pprint(&self) {
        log::trace!("Element content:\n{}", self.element_content.pprint());
        log::trace!("start tag content:\n{}", self.start_tag_content.pprint());
    }
}
