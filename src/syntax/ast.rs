use codespan::Span;

use crate::syntax::{Symbol, SymbolMap};

#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
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

#[derive(Copy, Clone)]
pub struct Spanned<T> {
    pub val: T,
    pub span: Span,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct FuncExpr {
    pub arg: Spanned<Symbol>,
    pub body: Spanned<Expr>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct CallExpr {
    pub func: Spanned<Expr>,
    pub arg: Spanned<Expr>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct LetExpr {
    pub name: Spanned<Symbol>,
    pub val: Spanned<Expr>,
    pub body: Spanned<Expr>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct RecExpr {
    pub name: Spanned<Symbol>,
    pub func: Spanned<FuncExpr>,
    pub body: Spanned<Expr>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct IfExpr {
    pub cond: Spanned<Expr>,
    pub cons: Spanned<Expr>,
    pub alt: Spanned<Expr>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct ProjExpr {
    pub expr: Spanned<Expr>,
    pub field: Spanned<Symbol>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct MatchExpr {
    pub expr: Spanned<Expr>,
    pub cases: SymbolMap<Spanned<MatchExprCase>>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct MatchExprCase {
    pub name: Option<Spanned<Symbol>>,
    pub expr: Spanned<Expr>,
}

#[cfg_attr(test, derive(PartialEq))]
pub struct EnumExpr {
    pub tag: Spanned<Symbol>,
    pub expr: Option<Spanned<Expr>>,
}

#[cfg(test)]
impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}
