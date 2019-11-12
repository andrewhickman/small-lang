use std::cmp::Ordering;
use std::iter::FromIterator;
use std::mem::{discriminant, Discriminant};
use std::{cmp, fmt, vec};

use im::OrdMap;
use mlsub::auto::StateSet;
use mlsub::Polarity;

use crate::check::FileSpan;
use crate::syntax::Symbol;

#[derive(Clone, Debug)]
pub struct Constructor {
    kind: ConstructorKind,
    spans: Vec<FileSpan>,
}

#[derive(Clone, Debug)]
pub enum ConstructorKind {
    Null,
    Bool,
    Number(Number),
    String,
    Func(StateSet, StateSet),
    Record(OrdMap<Symbol, StateSet>),
    Enum(OrdMap<Symbol, StateSet>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Float,
    Int,
}

impl Constructor {
    pub fn new(kind: ConstructorKind, span: Option<FileSpan>) -> Self {
        Constructor {
            kind,
            spans: Vec::from_iter(span),
        }
    }

    pub fn spans(&self) -> &[FileSpan] {
        &self.spans
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
        debug_assert_eq!(self.component(), other.component());

        self.spans.extend(&other.spans);

        match (&mut self.kind, &other.kind) {
            (ConstructorKind::Null, ConstructorKind::Null) => (),
            (ConstructorKind::Bool, ConstructorKind::Bool) => (),
            (ConstructorKind::Number(lhs), ConstructorKind::Number(rhs)) => {
                *lhs = cmp::max(*lhs, *rhs)
            }
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
            | ConstructorKind::Number(_)
            | ConstructorKind::String => vec![],
            ConstructorKind::Func(d, r) => {
                vec![(Label::Domain, d.clone()), (Label::Range, r.clone())]
            }
            ConstructorKind::Record(fields) => fields
                .clone()
                .into_iter()
                .map(|(label, set)| (Label::Field(label), set))
                .collect(),
            ConstructorKind::Enum(fields) => fields
                .clone()
                .into_iter()
                .map(|(label, set)| (Label::Tag(label), set))
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
                    .map(|(label, set)| (label, mapper(Label::Field(label), set)))
                    .collect(),
            ),
            ConstructorKind::Enum(fields) => ConstructorKind::Enum(
                fields
                    .into_iter()
                    .map(|(label, set)| (label, mapper(Label::Tag(label), set)))
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

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Number::Float, Number::Float) => Ordering::Equal,
            (Number::Int, Number::Int) => Ordering::Equal,
            (Number::Int, Number::Float) => Ordering::Less,
            (Number::Float, Number::Int) => Ordering::Greater,
        }
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.kind, &other.kind) {
            (ConstructorKind::Null, ConstructorKind::Null) => Some(Ordering::Equal),
            (ConstructorKind::Bool, ConstructorKind::Bool) => Some(Ordering::Equal),
            (ConstructorKind::Number(lhs), ConstructorKind::Number(rhs)) => lhs.partial_cmp(rhs),
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
    Field(Symbol),
    Tag(Symbol),
}

impl mlsub::Label for Label {
    fn polarity(&self) -> Polarity {
        match self {
            Label::Domain => Polarity::Neg,
            Label::Range | Label::Field(_) | Label::Tag(_) => Polarity::Pos,
        }
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ConstructorKind::Null => "null".fmt(f),
            ConstructorKind::Bool => "bool".fmt(f),
            ConstructorKind::Number(num) => num.fmt(f),
            ConstructorKind::String => "string".fmt(f),
            ConstructorKind::Func(..) => "func".fmt(f),
            ConstructorKind::Record(labels) => write!(f, "record {{{}}}", Labels(labels)),
            ConstructorKind::Enum(labels) => write!(f, "enum [{}]", Labels(labels)),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Int => "int".fmt(f),
            Number::Float => "number".fmt(f),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Label::Domain => "domain".fmt(f),
            Label::Range => "range".fmt(f),
            Label::Field(field) => write!(f, "field `{}`", field),
            Label::Tag(field) => write!(f, "tag `{}`", field),
        }
    }
}

struct Labels<'a>(&'a OrdMap<Symbol, StateSet>);

impl<'a> fmt::Display for Labels<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut labels: Vec<_> = self.0.keys().collect();
        labels.sort_by_key(|symbol| symbol.as_str());

        let mut iter = labels.into_iter();
        if let Some(label) = iter.next() {
            write!(f, "{}", label)?;
            for label in iter {
                write!(f, ", {}", label)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use im::ordmap;
    use mlsub::auto::{Automaton, StateId, StateSet};
    use mlsub::Polarity;

    use crate::check::ty::{Constructor, ConstructorKind, Number};
    use crate::syntax::Symbol;

    #[test]
    fn number_ordering() {
        assert!(Number::Int < Number::Float);
        assert!(Number::Float > Number::Int);
    }

    #[test]
    fn record_ordering() {
        let id = dummy_id();

        let subtype = Constructor::new(
            ConstructorKind::Record(ordmap! {
                Symbol::new("a") => StateSet::new(id),
                Symbol::new("b") => StateSet::new(id)
            }),
            None,
        );
        let supertype = Constructor::new(
            ConstructorKind::Record(ordmap! {
                Symbol::new("a") => StateSet::new(id)
            }),
            None,
        );
        assert!(subtype < supertype);
    }

    #[test]
    fn enum_ordering() {
        let id = dummy_id();

        let subtype = Constructor::new(
            ConstructorKind::Enum(ordmap! {
                Symbol::new("a") => StateSet::new(id)
            }),
            None,
        );
        let supertype = Constructor::new(
            ConstructorKind::Enum(ordmap! {
                Symbol::new("a") => StateSet::new(id),
                Symbol::new("b") => StateSet::new(id)
            }),
            None,
        );
        assert!(subtype < supertype);
    }

    fn dummy_id() -> StateId {
        Automaton::<Constructor>::new().build_empty(Polarity::Pos)
    }
}
