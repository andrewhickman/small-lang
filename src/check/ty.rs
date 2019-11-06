use std::cmp::Ordering;
use std::fmt::{self, Write};
use std::mem::{discriminant, Discriminant};
use std::vec;

use codespan::Span;
use im::OrdMap;
use mlsub::auto::StateSet;
use mlsub::Polarity;

use crate::syntax::Symbol;

#[derive(Clone, Debug)]
pub struct Constructor {
    kind: ConstructorKind,
    spans: Vec<Span>,
}

#[derive(Clone, Debug)]
pub enum ConstructorKind {
    Null,
    Bool,
    Int,
    String,
    Func(StateSet, StateSet),
    Record(OrdMap<Symbol, StateSet>),
    Enum(OrdMap<Symbol, StateSet>),
}

impl Constructor {
    pub fn new(kind: ConstructorKind, span: Span) -> Self {
        Constructor {
            kind,
            spans: vec![span],
        }
    }

    pub fn span(&self) -> impl fmt::Display {
        let mut s = String::new();
        write!(s, "{}:{}", self.spans[0].start(), self.spans[0].end()).unwrap();
        for span in &self.spans[1..] {
            write!(s, "and {}:{}", span.start(), span.end()).unwrap();
        }
        s
    }
}

impl mlsub::Constructor for Constructor {
    type Label = Label;
    type Component = Discriminant<ConstructorKind>;
    type Params = vec::IntoIter<(Label, StateSet)>;

    fn component(&self) -> Self::Component {
        discriminant(&self.kind)
    }

    fn join(&mut self, other: &Self, pol: Polarity) {
        self.spans.extend(&other.spans);

        match (&mut self.kind, &other.kind) {
            (ConstructorKind::Null, ConstructorKind::Null) => (),
            (ConstructorKind::Bool, ConstructorKind::Bool) => (),
            (ConstructorKind::Int, ConstructorKind::Int) => (),
            (ConstructorKind::String, ConstructorKind::String) => (),
            (ConstructorKind::Func(ld, lr), ConstructorKind::Func(rd, rr)) => {
                ld.union(rd);
                lr.union(rr);
            }
            (ConstructorKind::Record(lhs), ConstructorKind::Record(rhs)) => match pol {
                Polarity::Pos => {
                    *lhs = lhs.clone().intersection_with(rhs.clone(), |mut l, r| {
                        l.union(&r);
                        l
                    })
                }
                Polarity::Neg => {
                    *lhs = lhs.clone().union_with(rhs.clone(), |mut l, r| {
                        l.union(&r);
                        l
                    })
                }
            },
            (ConstructorKind::Enum(lhs), ConstructorKind::Enum(rhs)) => match pol {
                Polarity::Neg => {
                    *lhs = lhs.clone().intersection_with(rhs.clone(), |mut l, r| {
                        l.union(&r);
                        l
                    })
                }
                Polarity::Pos => {
                    *lhs = lhs.clone().union_with(rhs.clone(), |mut l, r| {
                        l.union(&r);
                        l
                    })
                }
            },
            _ => unreachable!(),
        }
    }

    fn params(&self) -> Self::Params {
        match &self.kind {
            ConstructorKind::Null
            | ConstructorKind::Bool
            | ConstructorKind::Int
            | ConstructorKind::String => vec![],
            ConstructorKind::Func(d, r) => {
                vec![(Label::Domain, d.clone()), (Label::Range, r.clone())]
            }
            ConstructorKind::Record(fields) => fields
                .clone()
                .into_iter()
                .map(|(label, set)| (Label::Label(label), set))
                .collect(),
            ConstructorKind::Enum(fields) => fields
                .clone()
                .into_iter()
                .map(|(label, set)| (Label::Label(label), set))
                .collect(),
        }
        .into_iter()
    }

    fn map<F>(self, mut mapper: F) -> Self
    where
        F: FnMut(Self::Label, StateSet) -> StateSet,
    {
        let kind = match self.kind {
            ConstructorKind::Func(d, r) => {
                ConstructorKind::Func(mapper(Label::Domain, d), mapper(Label::Range, r))
            }
            ConstructorKind::Record(fields) => ConstructorKind::Record(
                fields
                    .into_iter()
                    .map(|(label, set)| (label, mapper(Label::Label(label), set)))
                    .collect(),
            ),
            ConstructorKind::Enum(fields) => ConstructorKind::Enum(
                fields
                    .into_iter()
                    .map(|(label, set)| (label, mapper(Label::Label(label), set)))
                    .collect(),
            ),
            scalar => scalar,
        };
        Constructor {
            kind,
            spans: self.spans,
        }
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.kind, &other.kind) {
            (ConstructorKind::Null, ConstructorKind::Null) => Some(Ordering::Equal),
            (ConstructorKind::Bool, ConstructorKind::Bool) => Some(Ordering::Equal),
            (ConstructorKind::Int, ConstructorKind::Int) => Some(Ordering::Equal),
            (ConstructorKind::Func(..), ConstructorKind::Func(..)) => Some(Ordering::Equal),
            (ConstructorKind::Record(lhs), ConstructorKind::Record(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys()).map(Ordering::reverse)
            }
            (ConstructorKind::Enum(lhs), ConstructorKind::Enum(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys())
            }
            _ => None,
        }
    }
}

impl PartialEq for Constructor {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum Label {
    Domain,
    Range,
    Label(Symbol),
}

impl mlsub::Label for Label {
    fn polarity(&self) -> Polarity {
        match self {
            Label::Domain => Polarity::Neg,
            Label::Range | Label::Label(_) => Polarity::Pos,
        }
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ConstructorKind::Null => "null".fmt(f),
            ConstructorKind::Bool => "bool".fmt(f),
            ConstructorKind::Int => "int".fmt(f),
            ConstructorKind::String => "string".fmt(f),
            ConstructorKind::Func(..) => "func".fmt(f),
            ConstructorKind::Record(..) => "record".fmt(f),
            ConstructorKind::Enum(..) => "enum".fmt(f),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Label::Domain => "domain".fmt(f),
            Label::Range => "range".fmt(f),
            Label::Label(field) => write!(f, "field `{}`", field),
        }
    }
}
