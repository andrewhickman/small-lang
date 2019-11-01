use std::cmp::Ordering;
use std::mem::{discriminant, Discriminant};
use std::vec;

use im::OrdMap;
use mlsub::auto::StateSet;
use mlsub::Polarity;

use crate::syntax::Symbol;

#[derive(Clone, Debug)]
pub enum Constructor {
    Bool,
    Int,
    String,
    Func(StateSet, StateSet),
    Record(OrdMap<Symbol, StateSet>),
    Enum(OrdMap<Symbol, StateSet>),
}

impl mlsub::Constructor for Constructor {
    type Label = Label;
    type Component = Discriminant<Self>;
    type Params = vec::IntoIter<(Label, StateSet)>;

    fn component(&self) -> Self::Component {
        discriminant(self)
    }

    fn join(&mut self, other: &Self, pol: Polarity) {
        match (self, other) {
            (Constructor::Bool, Constructor::Bool) => (),
            (Constructor::Int, Constructor::Int) => (),
            (Constructor::String, Constructor::String) => (),
            (Constructor::Func(ld, lr), Constructor::Func(rd, rr)) => {
                ld.union(rd);
                lr.union(rr);
            }
            (Constructor::Record(ref mut lhs), Constructor::Record(ref rhs)) => match pol {
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
            (Constructor::Enum(ref mut lhs), Constructor::Enum(ref rhs)) => match pol {
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
        match self {
            Constructor::Bool | Constructor::Int | Constructor::String => vec![],
            Constructor::Func(d, r) => vec![(Label::Domain, d.clone()), (Label::Range, r.clone())],
            Constructor::Record(fields) => fields
                .clone()
                .into_iter()
                .map(|(label, set)| (Label::Label(label), set))
                .collect(),
            Constructor::Enum(fields) => fields
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
        match self {
            Constructor::Func(d, r) => {
                Constructor::Func(mapper(Label::Domain, d), mapper(Label::Range, r))
            }
            Constructor::Record(fields) => Constructor::Record(
                fields
                    .into_iter()
                    .map(|(label, set)| (label, mapper(Label::Label(label), set)))
                    .collect(),
            ),
            Constructor::Enum(fields) => Constructor::Enum(
                fields
                    .into_iter()
                    .map(|(label, set)| (label, mapper(Label::Label(label), set)))
                    .collect(),
            ),
            scalar => scalar,
        }
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Constructor::Bool, Constructor::Bool) => Some(Ordering::Equal),
            (Constructor::Int, Constructor::Int) => Some(Ordering::Equal),
            (Constructor::Func(..), Constructor::Func(..)) => Some(Ordering::Equal),
            (Constructor::Record(ref lhs), Constructor::Record(ref rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys()).map(Ordering::reverse)
            }
            (Constructor::Enum(ref lhs), Constructor::Enum(ref rhs)) => {
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

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
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
