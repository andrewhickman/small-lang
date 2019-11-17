mod capabilities;

pub(in crate::check) use capabilities::Capabilities;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::iter::{once, FromIterator};
use std::mem::{discriminant, Discriminant};
use std::rc::Rc;
use std::{cmp, fmt, vec};

use im::OrdMap;
use mlsub::auto::{StateId, StateSet};
use mlsub::Polarity;

use crate::check::{Context, FileSpan};
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
    String,
    Number(NumberConstructor),
    Func(FuncConstructor),
    Object(ObjectConstructor),
    Record(OrdMap<Symbol, StateSet>),
    Enum(OrdMap<Symbol, StateSet>),
    Capabilities(Rc<RefCell<Option<im::OrdMap<Symbol, StateSet>>>>),
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
    capabilities: StateSet,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
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
            spans: Vec::from_iter(span),
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
            (ConstructorKind::Capabilities(lhs), ConstructorKind::Capabilities(rhs)) => {
                let a = lhs.borrow().clone().expect("capabilities not set");
                let b = rhs.borrow().clone().expect("capabilities not set");

                if Rc::ptr_eq(lhs, rhs) {
                    return;
                }

                let joined = match pol {
                    Polarity::Pos => a.intersection_with(b, |mut l, r| {
                        l.union(&r);
                        l
                    }),
                    Polarity::Neg => a.union_with(b, |mut l, r| {
                        l.union(&r);
                        l
                    }),
                };
                *lhs = Rc::new(RefCell::new(Some(joined)));
            }
            _ => unreachable!(),
        }
    }

    fn params(&self) -> Self::Params {
        match &self.kind {
            ConstructorKind::Null
            | ConstructorKind::Bool
            | ConstructorKind::Number(_)
            | ConstructorKind::String => vec![],
            ConstructorKind::Func(func) => func.params(),
            ConstructorKind::Object(obj) => obj.params(),
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
            ConstructorKind::Capabilities(capabilities) => capabilities
                .borrow()
                .clone()
                .expect("capabilities not set")
                .into_iter()
                .map(|(name, set)| (Label::Capability(name), set))
                .collect(),
        }
        .into_iter()
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
            ConstructorKind::Capabilities(capabilities) => {
                ConstructorKind::Capabilities(Rc::new(RefCell::new(Some(
                    capabilities
                        .borrow()
                        .clone()
                        .expect("capabilities not set")
                        .into_iter()
                        .map(|(name, set)| (name, mapper(Label::Capability(name), set)))
                        .collect(),
                ))))
            }
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
            (ConstructorKind::Object(_), ConstructorKind::Object(_)) => Some(Ordering::Equal),
            (ConstructorKind::Record(lhs), ConstructorKind::Record(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys()).map(Ordering::reverse)
            }
            (ConstructorKind::Enum(lhs), ConstructorKind::Enum(rhs)) => {
                iter_set::cmp(lhs.keys(), rhs.keys())
            }
            (ConstructorKind::Capabilities(lhs), ConstructorKind::Capabilities(rhs)) => {
                iter_set::cmp(
                    lhs.borrow().as_ref().expect("capabilities not set").keys(),
                    rhs.borrow().as_ref().expect("capabilities not set").keys(),
                )
                .map(Ordering::reverse)
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

impl FuncConstructor {
    fn join(&mut self, other: &Self) {
        self.domain.union(&other.domain);
        self.range.union(&other.range);
    }

    fn params(&self) -> Vec<(Label, StateSet)> {
        vec![
            (Label::Domain, self.domain.clone()),
            (Label::Range, self.range.clone()),
        ]
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
        self.capabilities.union(&other.capabilities);
    }

    fn params(&self) -> Vec<(Label, StateSet)> {
        vec![
            (Label::ObjectData, self.data.clone()),
            (Label::ObjectCapabilities, self.capabilities.clone()),
        ]
    }

    fn map<F>(self, mut mapper: F) -> Self
    where
        F: FnMut(Label, StateSet) -> StateSet,
    {
        ObjectConstructor {
            data: mapper(Label::ObjectData, self.data),
            capabilities: mapper(Label::ObjectCapabilities, self.capabilities),
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

impl<'a> Context<'a> {
    pub fn build_null(&mut self, pol: Polarity, span: Option<FileSpan>) -> StateId {
        self.build_object(
            pol,
            span,
            ConstructorKind::Null,
            self.capabilities.empty(pol),
        )
    }

    pub fn build_bool(&mut self, pol: Polarity, span: Option<FileSpan>) -> StateId {
        self.build_object(
            pol,
            span,
            ConstructorKind::Bool,
            self.capabilities.empty(pol),
        )
    }

    pub fn build_number(
        &mut self,
        pol: Polarity,
        span: Option<FileSpan>,
        num: NumberConstructor,
    ) -> StateId {
        self.build_object(
            pol,
            span,
            ConstructorKind::Number(num),
            self.capabilities.number(pol, num),
        )
    }

    pub fn build_string(&mut self, pol: Polarity, span: Option<FileSpan>) -> StateId {
        self.build_object(
            pol,
            span,
            ConstructorKind::String,
            self.capabilities.string(pol),
        )
    }

    pub fn build_func(
        &mut self,
        pol: Polarity,
        span: Option<FileSpan>,
        domain: StateId,
        range: StateId,
    ) -> StateId {
        self.build_object(
            pol,
            span,
            ConstructorKind::Func(FuncConstructor {
                domain: StateSet::new(domain),
                range: StateSet::new(range),
            }),
            self.capabilities.empty(pol),
        )
    }

    pub fn build_record<I>(&mut self, pol: Polarity, span: Option<FileSpan>, iter: I) -> StateId
    where
        I: IntoIterator<Item = (Symbol, StateId)>,
    {
        self.build_object(
            pol,
            span,
            ConstructorKind::Record(
                iter.into_iter()
                    .map(|(sym, id)| (sym, StateSet::new(id)))
                    .collect(),
            ),
            self.capabilities.empty(pol),
        )
    }

    pub fn build_enum<I>(&mut self, pol: Polarity, span: Option<FileSpan>, iter: I) -> StateId
    where
        I: IntoIterator<Item = (Symbol, StateId)>,
    {
        self.build_object(
            pol,
            span,
            ConstructorKind::Enum(
                iter.into_iter()
                    .map(|(tag, ty)| (tag, StateSet::new(ty)))
                    .collect(),
            ),
            self.capabilities.empty(pol),
        )
    }

    pub fn build_enum_variant(
        &mut self,
        pol: Polarity,
        span: Option<FileSpan>,
        field: Symbol,
        expr: StateId,
    ) -> StateId {
        self.build_enum(pol, span, once((field, expr)))
    }

    pub fn build_capability(
        &mut self,
        pol: Polarity,
        span: Option<FileSpan>,
        name: Symbol,
        ty: StateId,
    ) -> StateId {
        let data = self.auto.build_empty(pol);
        let capabilities = self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Capabilities(Rc::new(RefCell::new(Some(
                    once((name, StateSet::new(ty))).collect(),
                )))),
                span,
            ),
        );
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Object(ObjectConstructor {
                    data: StateSet::new(data),
                    capabilities: StateSet::new(capabilities),
                }),
                span,
            ),
        )
    }

    fn build_object(
        &mut self,
        pol: Polarity,
        span: Option<FileSpan>,
        data: ConstructorKind,
        capabilities: Rc<RefCell<Option<OrdMap<Symbol, StateSet>>>>,
    ) -> StateId {
        let data = self
            .auto
            .build_constructed(pol, Constructor::new(data, span));
        let capabilities = self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Capabilities(capabilities), span),
        );
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Object(ObjectConstructor {
                    data: StateSet::new(data),
                    capabilities: StateSet::new(capabilities),
                }),
                span,
            ),
        )
    }
}

impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ConstructorKind::Null => "null".fmt(f),
            ConstructorKind::Bool => "bool".fmt(f),
            ConstructorKind::Number(num) => num.fmt(f),
            ConstructorKind::String => "string".fmt(f),
            ConstructorKind::Func(_) => "func".fmt(f),
            ConstructorKind::Object(_) => "object".fmt(f),
            ConstructorKind::Record(labels) => write!(f, "record {{{}}}", Labels(labels)),
            ConstructorKind::Enum(labels) => write!(f, "enum [{}]", Labels(labels)),
            ConstructorKind::Capabilities(labels) => write!(
                f,
                "capabilities {{{}}}",
                Labels(labels.borrow().as_ref().expect("capabilities not set"))
            ),
        }
    }
}

impl fmt::Display for NumberConstructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NumberConstructor::Int => "int".fmt(f),
            NumberConstructor::Float => "number".fmt(f),
        }
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
