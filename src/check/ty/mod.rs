mod build;

use std::cmp::Ordering;
use std::iter::FromIterator;
use std::{cmp, fmt};

use im::OrdMap;
use mlsub::auto::StateSet;
use mlsub::Polarity;
use small_ord_set::SmallOrdSet;

use crate::check::FileSpan;
use crate::syntax::Symbol;

#[derive(Clone, Debug)]
pub struct Constructor {
    kind: ConstructorKind,
    spans: SmallOrdSet<[FileSpan; 4]>,
}

#[derive(Clone, Debug)]
pub enum ConstructorKind {
    Null,
    Bool,
    String,
    Number(NumberConstructor),
    Func(FuncConstructor),
    Object(ObjectConstructor),
    Record(OrdMap<Symbol, StateSet>),
    Enum(OrdMap<Symbol, StateSet>),
    Capabilities(OrdMap<Symbol, StateSet>),
}

/// std::mem::Discriminant does not implement Ord so we cannot use it here :(
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Component {
    Null,
    Bool,
    String,
    Number,
    Func,
    Object,
    Record,
    Enum,
    Capabilities,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumberConstructor {
    Float,
    Int,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncConstructor {
    domain: StateSet,
    range: StateSet,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectConstructor {
    data: StateSet,
    capabilities: Option<StateSet>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Label {
    Domain,
    Range,
    Field(Symbol),
    Tag(Symbol),
    ObjectData,
    ObjectCapabilities,
    Capability(Symbol),
}

impl Constructor {
    pub fn new(kind: ConstructorKind, span: Option<FileSpan>) -> Self {
        Constructor {
            kind,
            spans: SmallOrdSet::from_iter(span),
        }
    }

    pub fn spans(&self) -> &[FileSpan] {
        &self.spans
    }

    pub fn is_object(&self) -> bool {
        match self.kind {
            ConstructorKind::Object(_) => true,
            _ => false,
        }
    }
}

impl mlsub::Constructor for Constructor {
    type Label = Label;
    type Component = Component;

    fn component(&self) -> Self::Component {
        match &self.kind {
            ConstructorKind::Null => Component::Null,
            ConstructorKind::Bool => Component::Bool,
            ConstructorKind::String => Component::String,
            ConstructorKind::Number(_) => Component::Number,
            ConstructorKind::Func(_) => Component::Func,
            ConstructorKind::Object(_) => Component::Object,
            ConstructorKind::Record(_) => Component::Record,
            ConstructorKind::Enum(_) => Component::Enum,
            ConstructorKind::Capabilities(_) => Component::Capabilities,
        }
    }

    fn join(&mut self, other: &Self, pol: Polarity) {
        debug_assert_eq!(self.component(), other.component());

        self.spans.extend(other.spans.iter().copied());

        match (&mut self.kind, &other.kind) {
            (ConstructorKind::Null, ConstructorKind::Null) => (),
            (ConstructorKind::Bool, ConstructorKind::Bool) => (),
            (ConstructorKind::Number(lhs), ConstructorKind::Number(rhs)) => {
                *lhs = cmp::max(*lhs, *rhs)
            }
            (ConstructorKind::String, ConstructorKind::String) => (),
            (ConstructorKind::Func(lhs), ConstructorKind::Func(rhs)) => lhs.join(rhs),
            (ConstructorKind::Object(lhs), ConstructorKind::Object(rhs)) => lhs.join(rhs),
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
            (ConstructorKind::Capabilities(lhs), ConstructorKind::Capabilities(rhs)) => match pol {
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
            _ => unreachable!(),
        }
    }

    fn visit_params_intersection<F, E>(&self, other: &Self, visit: F) -> Result<(), E>
    where
        F: FnMut(Self::Label, &StateSet, &StateSet) -> Result<(), E>,
    {
        debug_assert_eq!(self.component(), other.component());

        match (&self.kind, &other.kind) {
            (ConstructorKind::Func(l), ConstructorKind::Func(r)) => {
                l.visit_params_intersection(r, visit)
            }
            (ConstructorKind::Object(l), ConstructorKind::Object(r)) => {
                l.visit_params_intersection(r, visit)
            }
            (ConstructorKind::Record(l), ConstructorKind::Record(r)) => {
                visit_ordmap_intersection(l, r, visit, Label::Field)
            }
            (ConstructorKind::Enum(l), ConstructorKind::Enum(r)) => {
                visit_ordmap_intersection(l, r, visit, Label::Tag)
            }
            (ConstructorKind::Capabilities(l), ConstructorKind::Capabilities(r)) => {
                visit_ordmap_intersection(l, r, visit, Label::Capability)
            }
            _ => Ok(()),
        }
    }

    fn map<F>(self, mut mapper: F) -> Self
    where
        F: FnMut(Self::Label, StateSet) -> StateSet,
    {
        let kind = match self.kind {
            ConstructorKind::Func(func) => ConstructorKind::Func(func.map(mapper)),
            ConstructorKind::Object(obj) => ConstructorKind::Object(obj.map(mapper)),
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
            ConstructorKind::Capabilities(capabilities) => ConstructorKind::Capabilities(
                capabilities
                    .into_iter()
                    .map(|(label, set)| (label, mapper(Label::Capability(label), set)))
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

impl PartialOrd for NumberConstructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NumberConstructor {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (NumberConstructor::Float, NumberConstructor::Float) => Ordering::Equal,
            (NumberConstructor::Int, NumberConstructor::Int) => Ordering::Equal,
            (NumberConstructor::Int, NumberConstructor::Float) => Ordering::Less,
            (NumberConstructor::Float, NumberConstructor::Int) => Ordering::Greater,
        }
    }
}

impl PartialOrd for Constructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.kind, &other.kind) {
            (ConstructorKind::Null, ConstructorKind::Null) => Some(Ordering::Equal),
            (ConstructorKind::Bool, ConstructorKind::Bool) => Some(Ordering::Equal),
            (ConstructorKind::String, ConstructorKind::String) => Some(Ordering::Equal),
            (ConstructorKind::Number(lhs), ConstructorKind::Number(rhs)) => lhs.partial_cmp(rhs),
            (ConstructorKind::Func(_), ConstructorKind::Func(_)) => Some(Ordering::Equal),
            (ConstructorKind::Object(lhs), ConstructorKind::Object(rhs)) => {
                PartialOrd::partial_cmp(lhs, rhs)
            }
            (ConstructorKind::Record(lhs), ConstructorKind::Record(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys()).map(Ordering::reverse)
            }
            (ConstructorKind::Enum(lhs), ConstructorKind::Enum(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys())
            }
            (ConstructorKind::Capabilities(lhs), ConstructorKind::Capabilities(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys()).map(Ordering::reverse)
            }
            _ => None,
        }
    }
}

impl Constructor {
    pub fn visit_params<F>(&self, visit: F)
    where
        F: FnMut(Label, &StateSet),
    {
        match &self.kind {
            ConstructorKind::Null
            | ConstructorKind::Bool
            | ConstructorKind::String
            | ConstructorKind::Number(_) => (),
            ConstructorKind::Func(func) => func.visit_params(visit),
            ConstructorKind::Object(object) => object.visit_params(visit),
            ConstructorKind::Record(fields) => visit_ordmap(fields, visit, Label::Field),
            ConstructorKind::Enum(tags) => visit_ordmap(tags, visit, Label::Tag),
            ConstructorKind::Capabilities(caps) => visit_ordmap(caps, visit, Label::Capability),
        }
    }

    pub fn short_name(&self) -> &'static str {
        match &self.kind {
            ConstructorKind::Null => "null",
            ConstructorKind::Bool => "bool",
            ConstructorKind::String => "string",
            ConstructorKind::Number(num) => num.short_name(),
            ConstructorKind::Func(_) => "func",
            ConstructorKind::Object(_) => "object",
            ConstructorKind::Record(_) => "record",
            ConstructorKind::Enum(_) => "enum",
            ConstructorKind::Capabilities(_) => "capabilities",
        }
    }
}

impl PartialEq for Constructor {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl FuncConstructor {
    fn join(&mut self, other: &Self) {
        self.domain.union(&other.domain);
        self.range.union(&other.range);
    }

    fn visit_params_intersection<F, E>(&self, other: &Self, mut visit: F) -> Result<(), E>
    where
        F: FnMut(Label, &StateSet, &StateSet) -> Result<(), E>,
    {
        visit(Label::Domain, &self.domain, &other.domain)?;
        visit(Label::Range, &self.range, &other.range)?;
        Ok(())
    }

    fn visit_params<F>(&self, mut visit: F)
    where
        F: FnMut(Label, &StateSet),
    {
        visit(Label::Domain, &self.domain);
        visit(Label::Range, &self.range);
    }

    fn map<F>(self, mut mapper: F) -> Self
    where
        F: FnMut(Label, StateSet) -> StateSet,
    {
        FuncConstructor {
            domain: mapper(Label::Domain, self.domain),
            range: mapper(Label::Range, self.range),
        }
    }
}

impl ObjectConstructor {
    fn join(&mut self, other: &Self) {
        self.data.union(&other.data);
        match (&mut self.capabilities, &other.capabilities) {
            (Some(lhs), Some(rhs)) => lhs.union(rhs),
            (None, Some(rhs)) => self.capabilities = Some(rhs.clone()),
            (_, None) => (),
        };
    }

    fn visit_params_intersection<F, E>(&self, other: &Self, mut visit: F) -> Result<(), E>
    where
        F: FnMut(Label, &StateSet, &StateSet) -> Result<(), E>,
    {
        visit(Label::ObjectData, &self.data, &other.data)?;
        if let (Some(lhs), Some(rhs)) = (&self.capabilities, &other.capabilities) {
            visit(Label::ObjectCapabilities, lhs, rhs)?;
        }
        Ok(())
    }

    fn visit_params<F>(&self, mut visit: F)
    where
        F: FnMut(Label, &StateSet),
    {
        visit(Label::ObjectData, &self.data);
        if let Some(capabilities) = &self.capabilities {
            visit(Label::ObjectCapabilities, capabilities);
        }
    }

    fn map<F>(self, mut mapper: F) -> Self
    where
        F: FnMut(Label, StateSet) -> StateSet,
    {
        ObjectConstructor {
            data: mapper(Label::ObjectData, self.data),
            capabilities: self
                .capabilities
                .map(|capabilities| mapper(Label::ObjectCapabilities, capabilities)),
        }
    }
}

impl PartialOrd for ObjectConstructor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for ObjectConstructor {
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.capabilities, &other.capabilities) {
            (Some(_), Some(_)) => Ordering::Equal,
            (None, Some(_)) => Ordering::Greater,
            (Some(_), None) => Ordering::Less,
            (None, None) => Ordering::Equal,
        }
    }
}

impl mlsub::Label for Label {
    fn polarity(&self) -> Polarity {
        match self {
            Label::Domain => Polarity::Neg,
            Label::Range
            | Label::ObjectData
            | Label::ObjectCapabilities
            | Label::Field(_)
            | Label::Capability(_)
            | Label::Tag(_) => Polarity::Pos,
        }
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let short_name = self.short_name();
        match &self.kind {
            ConstructorKind::Record(labels) => write!(f, "{} {{{}}}", short_name, Labels(labels)),
            ConstructorKind::Enum(labels) => write!(f, "{} [{}]", short_name, Labels(labels)),
            ConstructorKind::Capabilities(labels) => {
                write!(f, "{} {{{}}}", short_name, Labels(labels))
            }
            _ => short_name.fmt(f),
        }
    }
}

impl NumberConstructor {
    pub fn short_name(self) -> &'static str {
        match self {
            NumberConstructor::Int => "int",
            NumberConstructor::Float => "number",
        }
    }
}

impl fmt::Display for NumberConstructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.short_name().fmt(f)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Label::Domain => "domain".fmt(f),
            Label::Range => "range".fmt(f),
            Label::ObjectData => "data".fmt(f),
            Label::ObjectCapabilities => "capabilities".fmt(f),
            Label::Field(field) => write!(f, "field `{}`", field),
            Label::Tag(field) => write!(f, "tag `{}`", field),
            Label::Capability(name) => write!(f, "capability `{}`", name),
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

fn visit_ordmap_intersection<F, E, L>(
    l: &OrdMap<Symbol, StateSet>,
    r: &OrdMap<Symbol, StateSet>,
    mut visit: F,
    mut label: L,
) -> Result<(), E>
where
    F: FnMut(Label, &StateSet, &StateSet) -> Result<(), E>,
    L: FnMut(Symbol) -> Label,
{
    itertools::merge_join_by(l, r, |l, r| Ord::cmp(&l.0, &r.0)).try_for_each(|eob| match eob {
        itertools::EitherOrBoth::Both(l, r) => visit(label(l.0), &l.1, &r.1),
        _ => Ok(()),
    })
}

fn visit_ordmap<F, L>(map: &OrdMap<Symbol, StateSet>, mut visit: F, mut label: L)
where
    F: FnMut(Label, &StateSet),
    L: FnMut(Symbol) -> Label,
{
    map.iter()
        .for_each(|(symbol, id)| visit(label(*symbol), id))
}

#[cfg(test)]
mod tests {
    use im::ordmap;
    use mlsub::auto::{Automaton, StateId, StateSet};
    use mlsub::Polarity;

    use crate::check::ty::{Constructor, ConstructorKind, NumberConstructor};
    use crate::syntax::Symbol;

    #[test]
    fn number_ordering() {
        assert!(NumberConstructor::Int < NumberConstructor::Float);
        assert!(NumberConstructor::Float > NumberConstructor::Int);
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
