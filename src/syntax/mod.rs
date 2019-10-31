pub mod symbol;
#[cfg(test)]
pub mod tests;
lalrpop_mod!(parser, "/syntax/parser.rs");

pub use symbol::{ImSymbolMap, Symbol, SymbolMap};

use std::str::FromStr;
use std::string::ToString;

use lalrpop_util::lalrpop_mod;
use lazy_static::lazy_static;
use parser::ExprParser;

#[derive(Debug)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    String(String),
    Var(Symbol),
    Record(SymbolMap<Expr>),
    Func(Symbol, Box<Expr>),
    Call(Box<Expr>, Box<Expr>),
    Let(Symbol, Box<Expr>, Box<Expr>),
    Rec(Symbol, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Proj(Box<Expr>, Symbol),
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
