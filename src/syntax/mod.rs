pub mod symbol;
#[cfg(test)]
pub mod tests;
lalrpop_mod!(parser, "/syntax/parser.rs");

pub use symbol::{Symbol, SymbolMap};

use std::rc::Rc;
use std::str::FromStr;
use std::string::ToString;

use lalrpop_util::lalrpop_mod;
use lazy_static::lazy_static;
use parser::ExprParser;

#[derive(Clone, Debug)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    String(String),
    Var(Symbol),
    Cons(SymbolMap<Expr>),
    Abs(Symbol, Rc<Expr>),
    App(Rc<Expr>, Rc<Expr>),
    Let(Symbol, Rc<Expr>, Rc<Expr>),
    Rec(Symbol, Rc<Expr>, Rc<Expr>),
    If(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Proj(Rc<Expr>, Symbol),
    Import(String),
}

impl FromStr for Expr {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref PARSER: ExprParser = ExprParser::new();
        }

        PARSER
            .parse(&mut symbol::Interner::write(), input)
            .map_err(|e| e.to_string())
    }
}
