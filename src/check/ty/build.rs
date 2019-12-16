use std::iter::once;

use im::ordmap;
use mlsub::auto::{flow, Build, StateId, StateSet};
use mlsub::{polar, Polarity};

use crate::check::ty::{
    Constructor, ConstructorKind, FuncConstructor, Label, NumberConstructor, ObjectConstructor,
};
use crate::check::{Context, FileSpan};
use crate::syntax::Symbol;

#[derive(Debug)]
enum CapabilitiesBuilder {
    Object {
        data: Box<polar::Ty<Self, flow::Pair>>,
        capabilities: Option<Box<polar::Ty<Self, flow::Pair>>>,
        span: Option<FileSpan>,
    },
    StringData {
        span: Option<FileSpan>,
    },
    NumberData {
        num: NumberConstructor,
        span: Option<FileSpan>,
    },
    StringCapabilities {
        add: Box<polar::Ty<Self, flow::Pair>>,
    },
    NumberCapabilities {
        add: Box<polar::Ty<Self, flow::Pair>>,
        sub: Box<polar::Ty<Self, flow::Pair>>,
    },
    Func {
        domain: Box<polar::Ty<Self, flow::Pair>>,
        range: Box<polar::Ty<Self, flow::Pair>>,
    },
}

impl Build<Constructor, flow::Pair> for CapabilitiesBuilder {
    fn map<'a, F>(&'a self, mut mapper: F) -> Constructor
    where
        F: FnMut(Label, &'a polar::Ty<Self, flow::Pair>) -> StateSet,
    {
        use CapabilitiesBuilder::*;

        match self {
            Object {
                span,
                data,
                capabilities,
            } => Constructor::new(
                ConstructorKind::Object(ObjectConstructor {
                    data: mapper(Label::ObjectData, data),
                    capabilities: capabilities
                        .as_ref()
                        .map(|capabilities| mapper(Label::ObjectCapabilities, capabilities)),
                }),
                *span,
            ),
            StringData { span } => Constructor::new(ConstructorKind::String, *span),
            NumberData { span, num } => Constructor::new(ConstructorKind::Number(*num), *span),
            StringCapabilities { add } => Constructor::new(
                ConstructorKind::Capabilities(ordmap! {
                    Symbol::new("add") => mapper(Label::Capability(Symbol::new("add")), add)
                }),
                None,
            ),
            NumberCapabilities { add, sub } => Constructor::new(
                ConstructorKind::Capabilities(ordmap! {
                    Symbol::new("add") => mapper(Label::Capability(Symbol::new("add")), add),
                    Symbol::new("sub") => mapper(Label::Capability(Symbol::new("sub")), sub)
                }),
                None,
            ),
            Func { domain, range } => Constructor::new(
                ConstructorKind::Func(FuncConstructor {
                    domain: mapper(Label::Domain, domain),
                    range: mapper(Label::Range, range),
                }),
                None,
            ),
        }
    }
}

impl<T, F> Context<T, F> {
    pub fn build_null(&mut self, pol: Polarity, span: Option<FileSpan>) -> StateId {
        self.build_object(pol, span, ConstructorKind::Null)
    }

    pub fn build_bool(&mut self, pol: Polarity, span: Option<FileSpan>) -> StateId {
        self.build_object(pol, span, ConstructorKind::Bool)
    }

    pub fn build_number(
        &mut self,
        pol: Polarity,
        span: Option<FileSpan>,
        num: NumberConstructor,
    ) -> StateId {
        let add_flow = self.auto.build_var();
        let sub_flow = self.auto.build_var();

        let polar_ty = polar::Ty::Recursive(Box::new(polar::Ty::Constructed(
            CapabilitiesBuilder::Object {
                data: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::NumberData {
                    num,
                    span,
                })),
                capabilities: Some(Box::new(polar::Ty::Constructed(
                    CapabilitiesBuilder::NumberCapabilities {
                        add: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Object {
                            data: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Func {
                                domain: Box::new(polar::Ty::Add(
                                    Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Object {
                                        data: Box::new(polar::Ty::Constructed(
                                            CapabilitiesBuilder::NumberData {
                                                num: NumberConstructor::Float,
                                                span: None,
                                            },
                                        )),
                                        capabilities: None,
                                        span: None,
                                    })),
                                    Box::new(polar::Ty::UnboundVar(add_flow)),
                                )),
                                range: Box::new(polar::Ty::Add(
                                    Box::new(polar::Ty::BoundVar(0)),
                                    Box::new(polar::Ty::UnboundVar(add_flow)),
                                )),
                            })),
                            capabilities: None,
                            span: None,
                        })),
                        sub: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Object {
                            data: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Func {
                                domain: Box::new(polar::Ty::Add(
                                    Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Object {
                                        data: Box::new(polar::Ty::Constructed(
                                            CapabilitiesBuilder::NumberData {
                                                num: NumberConstructor::Float,
                                                span: None,
                                            },
                                        )),
                                        capabilities: None,
                                        span: None,
                                    })),
                                    Box::new(polar::Ty::UnboundVar(sub_flow)),
                                )),
                                range: Box::new(polar::Ty::Add(
                                    Box::new(polar::Ty::BoundVar(0)),
                                    Box::new(polar::Ty::UnboundVar(sub_flow)),
                                )),
                            })),
                            capabilities: None,
                            span: None,
                        })),
                    },
                ))),
                span,
            },
        )));

        self.auto.build_polar_simple(pol, &polar_ty)
    }

    pub fn build_string(&mut self, pol: Polarity, span: Option<FileSpan>) -> StateId {
        let polar_ty = polar::Ty::Recursive(Box::new(polar::Ty::Constructed(
            CapabilitiesBuilder::Object {
                data: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::StringData {
                    span,
                })),
                capabilities: Some(Box::new(polar::Ty::Constructed(
                    CapabilitiesBuilder::StringCapabilities {
                        add: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Object {
                            data: Box::new(polar::Ty::Constructed(CapabilitiesBuilder::Func {
                                domain: Box::new(polar::Ty::Constructed(
                                    CapabilitiesBuilder::Object {
                                        data: Box::new(polar::Ty::Constructed(
                                            CapabilitiesBuilder::StringData { span: None },
                                        )),
                                        capabilities: None,
                                        span: None,
                                    },
                                )),
                                range: Box::new(polar::Ty::BoundVar(0)),
                            })),
                            capabilities: None,
                            span: None,
                        })),
                    },
                ))),
                span,
            },
        )));

        self.auto.build_polar_simple(pol, &polar_ty)
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
                ConstructorKind::Capabilities(once((name, StateSet::new(ty))).collect()),
                span,
            ),
        );
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Object(ObjectConstructor {
                    data: StateSet::new(data),
                    capabilities: Some(StateSet::new(capabilities)),
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
    ) -> StateId {
        let data = self
            .auto
            .build_constructed(pol, Constructor::new(data, span));
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Object(ObjectConstructor {
                    data: StateSet::new(data),
                    capabilities: None,
                }),
                span,
            ),
        )
    }
}
