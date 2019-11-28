use std::rc::Rc;

use crate::rt::Value;
use crate::syntax::{Symbol, SymbolMap};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum Expr {
    Literal(Value),
    Var(Symbol),
    Call(Box<Call>),
    Let(Box<Let>),
    Func(Box<Func>),
    If(Box<If>),
    Proj(Box<Proj>),
    Enum(Box<Enum>),
    Record(SymbolMap<Expr>),
    Match(Box<Match>),
    Import(Rc<Expr>),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Func {
    pub arg: Symbol,
    pub body: Expr,
    pub rec_name: Option<Symbol>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Call {
    pub arg: Expr,
    pub func: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Let {
    pub name: Symbol,
    pub val: Expr,
    pub body: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct If {
    pub cond: Expr,
    pub cons: Expr,
    pub alt: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Proj {
    pub expr: Expr,
    pub field: Symbol,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Enum {
    pub tag: Symbol,
    pub expr: Expr,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Match {
    pub expr: Expr,
    pub cases: SymbolMap<MatchCase>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct MatchCase {
    pub expr: Expr,
    pub name: Option<Symbol>,
}
