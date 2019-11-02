pub mod symbol;
#[cfg(test)]
pub mod tests;

lalrpop_mod!(
    #[allow(clippy::all)]
    parser,
    "/syntax/parser.rs"
);
mod source;

pub use source::SourceMap;
pub use symbol::{ImSymbolMap, Symbol, SymbolMap};

use codespan::{ByteIndex, RawIndex, Span};
use lalrpop_util::{lalrpop_mod, ParseError};
use lazy_static::lazy_static;
use parser::{SpannedExprParser, Token};

#[derive(Debug)]
pub enum Expr {
    Null,
    Bool(bool),
    Int(i64),
    String(String),
    Var(Symbol),
    Record(SymbolMap<Spanned<Expr>>),
    Enum(Box<EnumExpr>),
    Func(Box<FuncExpr>),
    Call(Box<CallExpr>),
    Let(Box<LetExpr>),
    Rec(Box<RecExpr>),
    If(Box<IfExpr>),
    Proj(Box<ProjExpr>),
    Match(Box<MatchExpr>),
    Import(String),
}

#[derive(Debug, Copy, Clone)]
pub struct Spanned<T> {
    pub val: T,
    pub span: Span,
}

#[derive(Debug)]
pub struct FuncExpr {
    pub arg: Spanned<Symbol>,
    pub body: Spanned<Expr>,
}

#[derive(Debug)]
pub struct CallExpr {
    pub func: Spanned<Expr>,
    pub arg: Spanned<Expr>,
}

#[derive(Debug)]
pub struct LetExpr {
    pub name: Spanned<Symbol>,
    pub val: Spanned<Expr>,
    pub body: Spanned<Expr>,
}

#[derive(Debug)]
pub struct RecExpr {
    pub name: Spanned<Symbol>,
    pub func: Spanned<FuncExpr>,
    pub body: Spanned<Expr>,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Spanned<Expr>,
    pub cons: Spanned<Expr>,
    pub alt: Spanned<Expr>,
}

#[derive(Debug)]
pub struct ProjExpr {
    pub expr: Spanned<Expr>,
    pub field: Spanned<Symbol>,
}

#[derive(Debug)]
pub struct MatchExpr {
    pub expr: Spanned<Expr>,
    pub cases: SymbolMap<Spanned<MatchExprCase>>,
}

#[derive(Debug)]
pub struct MatchExprCase {
    pub name: Option<Spanned<Symbol>>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
pub struct EnumExpr {
    pub tag: Spanned<Symbol>,
    pub expr: Option<Spanned<Expr>>,
}

impl Expr {
    pub(crate) fn parse<'a>(
        input: &'a str,
    ) -> Result<Spanned<Expr>, ParseError<ByteIndex, Token<'a>, &'static str>> {
        lazy_static! {
            static ref PARSER: SpannedExprParser = SpannedExprParser::new();
        }

        PARSER
            .parse(&mut symbol::Interner::write(), input)
            .map_err(|err| err.map_location(|idx| ByteIndex(idx as RawIndex)))
    }
}
