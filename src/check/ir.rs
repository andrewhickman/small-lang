use std::rc::Rc;

use mlsub::auto::{flow, StateId};

use crate::check::scheme::Scheme;
use crate::rt::Value;
use crate::syntax::{Symbol, SymbolMap};

pub struct Expr {
    pub scheme: Scheme,
    pub kind: ExprKind,
}

pub enum ExprKind {
    Literal(Value),
    Var(Symbol),
    Call(Box<Call>),
    Let(Box<Let>),
    Func(Box<Func>),
    If(Box<If>),
    Proj(Box<Proj>),
    Enum(Box<Enum>),
    Record(SymbolMap<RecordEntry>),
    Match(Box<Match>),
    Import(Rc<Expr>),
}

pub struct Func {
    pub arg: Symbol,
    pub body: Expr,
    pub rec_name: Option<Symbol>,
}

pub struct Call {
    pub arg: Expr,
    pub func: Expr,
}

pub struct Let {
    pub name: Symbol,
    pub val: Expr,
    pub body: Expr,
}

pub struct If {
    pub cond: Expr,
    pub cons: Expr,
    pub alt: Expr,
}

pub struct Proj {
    pub expr: Expr,
    pub field: Symbol,
}

pub struct Enum {
    pub tag: Symbol,
    pub expr: Expr,
}

pub struct RecordEntry {
    pub pair: flow::Pair,
    pub expr: Expr,
}

pub struct Match {
    pub expr: Expr,
    pub cases: SymbolMap<MatchCase>,
}

pub struct MatchCase {
    pub expr: Expr,
    pub name: Option<Symbol>,
    pub val_pair: flow::Pair,
    pub val_ty: Option<StateId>,
    pub scheme: Scheme,
}