pub mod location;
pub mod symbol;
lalrpop_mod!(parser, "/syntax/parser.rs");

pub use symbol::{Symbol, SymbolMap};
pub use location::{Location, Source};

use std::rc::Rc;
use std::str::FromStr;
use std::string::ToString;

use lalrpop_util::lalrpop_mod;
use lazy_static::lazy_static;
use parser::LocExprParser;

#[derive(Clone, Debug)]
pub enum Expr {
    Var(Symbol),
    Abs(Symbol, Rc<LocExpr>),
    App(Rc<LocExpr>, Rc<LocExpr>),
    Let(Symbol, Rc<LocExpr>, Rc<LocExpr>),
    True,
    False,
    If(Rc<LocExpr>, Rc<LocExpr>, Rc<LocExpr>),
    Cons(SymbolMap<LocExpr>),
    Proj(Rc<LocExpr>, Symbol),
}

#[derive(Clone, Debug)]
pub struct LocExpr {
    pub start: Location, 
    pub expr: Expr,
    pub end: Location,
}

impl FromStr for LocExpr {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref PARSER: LocExprParser = LocExprParser::new();
        }

        PARSER
            .parse(&mut symbol::Interner::write(), &Source::new(input), input)
            .map_err(|e| e.to_string())
    }
}
