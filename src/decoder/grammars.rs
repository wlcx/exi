use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::util::BitInput;

use super::{
    Event, ExiResult, ParseEvent,
    codetree::{CodeTree, codetree},
    datatypes::Qname,
    errors::{ExiErrorKind, make_exierror},
    options::Options,
};

// A grammar (in the context of EXI) is a (finite) state machine, which accepts variable-bit-length codes
// and produces parseevents.
// Grammars are mutable - they are "specialised" as the EXI decoder runs, with new "productions" (mappings from code to parse event) being inserted.
pub(super) struct Grammar {
    states: Vec<GrammarState>,
    r#type: GrammarType,
}

#[derive(Clone)]
pub(super) enum GrammarType {
    Document,
    Element(Qname),
    Fragment,
}

// A GrammarState is a state in a grammar - it has a name (for debugging) and a CodeTree
pub(super) type GrammarState = (String, CodeTree<Prod>);

// A "handle" (index) into a grammar to a state
pub(super) type StateHandle = usize;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum GrammarInstanceState {
    Live(StateHandle),
    Terminated,
}

impl GrammarInstanceState {
    fn describe_with<'a>(&self, g: &'a Grammar) -> &'a str {
        match self {
            Self::Live(handle) => &g.states[*handle].0,
            Self::Terminated => &"terminated",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Prod(ParseEvent, GrammarInstanceState);

impl Display for Prod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)?;
        if let GrammarInstanceState::Live(rhs) = &self.1 {
            write!(f, "\t\t")?;
            rhs.fmt(f)?;
        }
        Ok(())
    }
}

impl Grammar {
    // Parse some bits from `i`, using state index `state` and evaluate the matching production in the grammar
    fn parse<'a>(&mut self, state: StateHandle, i: BitInput<'a>) -> ExiResult<BitInput<'a>, Prod> {
        // TODO: probably some error handling around invalid array indices...
        self.states[state].1.parse(i).map_err(nom::Err::convert)
    }

    fn pprint(&self) {
        log::trace!(
            "{}",
            self.states
                .iter()
                .map(|(name, codetree)| { format!("{}\n{}\n\n", name, codetree.pprint()) })
                .collect::<String>()
        )
    }

    fn specialise(&mut self, state: StateHandle, ev: &Event) {
        match self.r#type {
            GrammarType::Document => {
                // Document grammars aren't specialised
                return;
            }
            GrammarType::Element(_) => {
                let s = &mut self.states[state].1;
                match ev {
                    Event::StartElement(qname) => {
                        log::trace!("Adding SEQname({}) to grammar", qname);
                        let new = Prod(
                            ParseEvent::SEQname(qname.clone()),
                            GrammarInstanceState::Live(1),
                        );
                        // Sanity check: there shouldn't be matching SEQname in the grammar already
                        assert!(s.iter().position(|e| *e == new).is_none());
                        *s = s.insert(0, new);
                    }
                    Event::Attribute { qname, .. } => {
                        log::trace!("Adding ATQname({}) to grammar", qname);
                        // Only applicable in starttagcontent
                        if state == 0 {
                            let new = Prod(
                                ParseEvent::ATQname(qname.clone()),
                                GrammarInstanceState::Live(0),
                            );
                            assert!(s.iter().position(|e| *e == new).is_none());
                            *s = s.insert(0, new);
                        }
                    }
                    Event::Characters(_) => {
                        let p = Prod(ParseEvent::CH, GrammarInstanceState::Live(1));
                        if s.find(&p, 0).is_none() {
                            log::trace!("Adding CH to grammar");
                            // TODO: may be faster to keep this state in a bool...
                            *s = s.insert(0, p);
                        }
                    }
                    Event::EndElement => {
                        let p = Prod(ParseEvent::EE, GrammarInstanceState::Terminated);
                        if s.find(&p, 0).is_none() {
                            log::trace!("Adding EE to grammar");
                            *s = s.insert(0, p);
                        }
                    }
                    _ => {}
                }
            }
            GrammarType::Fragment => {
                unimplemented!();
            }
        }
    }

    fn describe(&self, state: &GrammarInstanceState) -> String {
        match &self.r#type {
            GrammarType::Document => {
                format!("Document[{}]", state.describe_with(self))
            }
            GrammarType::Element(qname) => {
                format!("Element({})[{}]", qname, state.describe_with(self))
            }
            GrammarType::Fragment => {
                unimplemented!()
            }
        }
    }

    // Constractors for built-in grammars

    // https://www.w3.org/TR/exi/#builtinElemGrammars
    pub(super) fn builtin_element_grammar(o: Rc<Options>, qname: Qname) -> Self {
        Self {
            r#type: GrammarType::Element(qname),
            states: vec![
                (
                    "StartTagContent".into(),
                    codetree!(
                        0,0  => Prod(ParseEvent::EE, GrammarInstanceState::Terminated);
                        0,0  => Prod(ParseEvent::AT, GrammarInstanceState::Live(0));
                        0,0  => Prod(ParseEvent::NS, GrammarInstanceState::Live(0)), o.preserve.prefixes;
                        // FIXME: would cause a crash (2) but we should be panicing on self_contained = true pre-parse
                        0,0  => Prod(ParseEvent::SC, GrammarInstanceState::Live(2)), o.self_contained;
                        0,0  => Prod(ParseEvent::SE, GrammarInstanceState::Live(1));
                        0,0  => Prod(ParseEvent::CH, GrammarInstanceState::Live(1));
                        0,0  => Prod(ParseEvent::ER, GrammarInstanceState::Live(1)), o.preserve.dtd;

                        0,0,0 => Prod(ParseEvent::CM, GrammarInstanceState::Live(1)), o.preserve.comments;
                        0,0,0 => Prod(ParseEvent::PI, GrammarInstanceState::Live(1)), o.preserve.pis;
                    ),
                ),
                (
                    "ElementContent".into(),
                    codetree!(
                        0     => Prod(ParseEvent::EE, GrammarInstanceState::Terminated);
                        0,0   => Prod(ParseEvent::SE, GrammarInstanceState::Live(1));
                        0,0   => Prod(ParseEvent::CH, GrammarInstanceState::Live(1));
                        0,0   => Prod(ParseEvent::ER, GrammarInstanceState::Live(1)), o.preserve.dtd;
                        0,0,0 => Prod(ParseEvent::CM, GrammarInstanceState::Live(1)), o.preserve.comments;
                        0,0,0 => Prod(ParseEvent::PI, GrammarInstanceState::Live(1)), o.preserve.pis;
                    ),
                ),
            ],
        }
    }

    // https://www.w3.org/TR/exi/#builtinDocGrammars
    pub(super) fn builtin_document_grammar(o: Rc<Options>) -> Self {
        Self {
            r#type: GrammarType::Document,
            states: vec![
                (
                    "Document".into(),
                    codetree!(
                        0 => Prod(ParseEvent::SD, GrammarInstanceState::Live(1));
                    ),
                ),
                (
                    "DocContent".into(),
                    codetree!(
                        0     => Prod(ParseEvent::SE, GrammarInstanceState::Live(2));
                        1,0   => Prod(ParseEvent::DT, GrammarInstanceState::Live(1)), o.preserve.dtd;
                        1,1,0 => Prod(ParseEvent::CM, GrammarInstanceState::Live(1)), o.preserve.comments;
                        1,1,1 => Prod(ParseEvent::PI, GrammarInstanceState::Live(1)), o.preserve.pis;
                    ),
                ),
                (
                    "DocEnd".into(),
                    codetree!(
                        0     => Prod(ParseEvent::ED, GrammarInstanceState::Terminated);
                        1,0   => Prod(ParseEvent::CM, GrammarInstanceState::Live(1)), o.preserve.comments;
                        1,1   => Prod(ParseEvent::PI, GrammarInstanceState::Live(1)), o.preserve.pis;

                    ),
                ),
            ],
        }
    }

    // https://www.w3.org/TR/exi/#builtinFragGrammars
    pub(super) fn builtin_fragment_grammar(o: Rc<Options>) -> Self {
        Self {
            r#type: GrammarType::Fragment,
            states: vec![
                (
                    "Fragment".into(),
                    codetree!(
                        0     => Prod(ParseEvent::SD, GrammarInstanceState::Live(1));
                    ),
                ),
                (
                    "FragmentContent".into(),
                    codetree!(
                        0    => Prod(ParseEvent::SE, GrammarInstanceState::Live(1));
                        1    => Prod(ParseEvent::ED, GrammarInstanceState::Terminated);
                        2,0   => Prod(ParseEvent::CM, GrammarInstanceState::Live(1)), o.preserve.comments;
                        2,1   => Prod(ParseEvent::PI, GrammarInstanceState::Live(1)), o.preserve.pis;
                    ),
                ),
            ],
        }
    }
}

// A GrammarInstance is a "real" instance of a grammar: it has state (literally - the current state of the grammar) and holds a shared reference to the grammar itself.
// This is required because multiple instances of the same grammar may be active at a given time (e.g. for nested elements with the same qname) and specialisation should be shared across all of them.
pub(super) struct GrammarInstance {
    grammar: Rc<RefCell<Grammar>>,
    state: GrammarInstanceState,
}

impl GrammarInstance {
    // From a grammar, create a new instance with the state machine in its starting state.
    pub(super) fn instantiate(grammar: Rc<RefCell<Grammar>>) -> Self {
        Self {
            grammar,
            // By definition, the 0th state is the starting state
            state: GrammarInstanceState::Live(0),
        }
    }

    // Parse some input using the grammar instance. Returns the parsed event, plus the StateHandle used to parse it
    // This is needed because we then use this statehandle to target a state for specialisation with the fully parsed event. TODO: this is a bit messy, but is required because the logic for parsing
    pub(super) fn parse<'a>(
        &mut self,
        i: BitInput<'a>,
    ) -> ExiResult<BitInput<'a>, (ParseEvent, GrammarInstanceState)> {
        let GrammarInstanceState::Live(state_handle) = self.state else {
            return Err(nom::Err::Failure(make_exierror(
                i,
                ExiErrorKind::GrammarTerminated,
            )));
        };
        let mut g = self.grammar.borrow_mut();
        let (rest, Prod(ev, next_state)) = g.parse(state_handle, i)?;
        if next_state != self.state {
            // Log state transition
            log::trace!(
                "{} -> {}",
                g.states[state_handle].0,
                next_state.describe_with(&g),
            );
            return Ok((rest, (ev, std::mem::replace(&mut self.state, next_state))));
        }
        Ok((rest, (ev, self.state.clone())))
    }

    pub(super) fn describe(&self) -> String {
        self.grammar.borrow().describe(&self.state)
    }

    pub(super) fn specialise(&mut self, state: GrammarInstanceState, ev: &Event) {
        if let GrammarInstanceState::Live(handle) = state {
            self.grammar.borrow_mut().specialise(handle, ev)
        }
    }

    pub(super) fn grammar_type(&self) -> GrammarType {
        self.grammar.borrow().r#type.clone()
    }

    pub(super) fn pprint(&self) {
        self.grammar.borrow().pprint()
    }
}
