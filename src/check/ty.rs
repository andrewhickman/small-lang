use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::mem::{discriminant, Discriminant};

use im::OrdSet;
use mlsub::polar::Ty;
use mlsub::Polarity;

use crate::syntax::Symbol;

#[derive(Debug)]
pub struct TypeSystem;

impl mlsub::TypeSystem for TypeSystem {
    type Constructor = Constructor;
    type Symbol = Label;
}

#[derive(Clone, Debug)]
pub enum Constructor {
    Bool,
    Func,
    Record(OrdSet<Symbol>),
}

impl mlsub::Constructor for Constructor {
    type Component = Discriminant<Self>;

    fn component(&self) -> Self::Component {
        discriminant(self)
    }

    fn join(&mut self, other: &Self) {
        match (self, other) {
            (Constructor::Bool, Constructor::Bool) => (),
            (Constructor::Func, Constructor::Func) => (),
            (Constructor::Record(ref mut lhs), Constructor::Record(ref rhs)) => {
                *lhs = lhs.clone().intersection(rhs.clone())
            }
            _ => unreachable!(),
        }
    }

    fn meet(&mut self, other: &Self) {
        match (self, other) {
            (Constructor::Bool, Constructor::Bool) => (),
            (Constructor::Func, Constructor::Func) => (),
            (Constructor::Record(ref mut lhs), Constructor::Record(ref rhs)) => {
                *lhs = lhs.clone().union(rhs.clone())
            }
            _ => unreachable!(),
        }
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Constructor::Bool, Constructor::Bool) => Some(Ordering::Equal),
            (Constructor::Func, Constructor::Func) => Some(Ordering::Equal),
            (Constructor::Record(ref lhs), Constructor::Record(ref rhs)) => {
                iter_set::cmp(lhs, rhs).map(Ordering::reverse)
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

impl mlsub::auto::Symbol for Label {
    fn polarity(&self) -> Polarity {
        match self {
            Label::Domain => Polarity::Neg,
            Label::Range | Label::Label(_) => Polarity::Pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuildConstructor {
    Bool,
    Func(Box<Ty<Self, char>>, Box<Ty<Self, char>>),
    Record(BTreeMap<Symbol, Box<Ty<Self, char>>>),
}

impl mlsub::auto::Build<TypeSystem, char> for BuildConstructor {
    fn constructor(&self) -> Constructor {
        match self {
            BuildConstructor::Bool => Constructor::Bool,
            BuildConstructor::Func(..) => Constructor::Func,
            BuildConstructor::Record(fields) => {
                Constructor::Record(fields.keys().cloned().collect())
            }
        }
    }

    fn visit_transitions<'a, F>(&'a self, mut visit: F)
    where
        F: FnMut(Label, &'a Ty<BuildConstructor, char>),
    {
        match self {
            BuildConstructor::Bool => (),
            BuildConstructor::Func(domain, range) => {
                visit(Label::Domain, &*domain);
                visit(Label::Range, &*range);
            }
            BuildConstructor::Record(fields) => {
                for (&label, ty) in fields {
                    visit(Label::Label(label), &*ty);
                }
            }
        }
    }
}
