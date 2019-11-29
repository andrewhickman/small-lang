use std::rc::Rc;

use crate::rt::{Command, Value};
use crate::syntax::{Symbol, SymbolMap};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum Expr<T = Rc<[Command]>> {
    Literal(Value),
    Var(Symbol),
    Call(Box<Call<T>>),
    Let(Box<Let<T>>),
    Func(Box<Func<T>>),
    If(Box<If<T>>),
    Proj(Box<Proj<T>>),
    Enum(Box<Enum<T>>),
    Record(SymbolMap<Self>),
    Match(Box<Match<T>>),
    Import(T),
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Func<T = Rc<[Command]>> {
    pub arg: Symbol,
    pub body: Expr<T>,
    pub rec_name: Option<Symbol>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Call<T = Rc<[Command]>> {
    pub arg: Expr<T>,
    pub func: Expr<T>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Let<T = Rc<[Command]>> {
    pub name: Symbol,
    pub val: Expr<T>,
    pub body: Expr<T>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct If<T = Rc<[Command]>> {
    pub cond: Expr<T>,
    pub cons: Expr<T>,
    pub alt: Expr<T>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Proj<T = Rc<[Command]>> {
    pub expr: Expr<T>,
    pub field: Symbol,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Enum<T = Rc<[Command]>> {
    pub tag: Symbol,
    pub expr: Expr<T>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Match<T = Rc<[Command]>> {
    pub expr: Expr<T>,
    pub cases: SymbolMap<MatchCase<T>>,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct MatchCase<T = Rc<[Command]>> {
    pub expr: Expr<T>,
    pub name: Option<Symbol>,
}
