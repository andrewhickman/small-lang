use std::fmt::Debug;

use codespan::{FileId, Span};
use proptest::prelude::*;

use crate::syntax::*;

pub fn arb_expr() -> impl Strategy<Value = ast::Spanned<ast::Expr>> {
    prop_oneof![
        1 => prop::strategy::LazyJust::new(|| ast::Expr::Null),
        4 => any::<bool>().prop_map(ast::Expr::Bool),
        3 => any::<i64>().prop_map(ast::Expr::Int),
        2 => (-1000_000_000f64..1000_000_000.0).prop_map(ast::Expr::Float),
        1 => any::<String>().prop_map(ast::Expr::String),
        3 => arb_var_symbol().prop_map(ast::Expr::Var),
    ]
    .prop_map(spanned)
    .prop_recursive(8, 128, 4, |expr| {
        prop_oneof![
            arb_symbol_map(expr.clone()).prop_map(ast::Expr::Record),
            arb_func(expr.clone()).prop_map(|func| ast::Expr::Func(Box::new(func))),
            (expr.clone(), expr.clone())
                .prop_map(|(func, arg)| ast::Expr::Call(Box::new(ast::CallExpr { func, arg }))),
            (
                arb_var_symbol().prop_map(spanned),
                expr.clone(),
                expr.clone()
            )
                .prop_map(|(name, val, body)| ast::Expr::Let(Box::new(ast::LetExpr {
                    name,
                    val,
                    body
                }))),
            (
                arb_var_symbol().prop_map(spanned),
                arb_func(expr.clone()).prop_map(spanned),
                expr.clone()
            )
                .prop_map(|(name, func, body)| ast::Expr::Rec(Box::new(
                    ast::RecExpr { name, func, body }
                ))),
            (expr.clone(), expr.clone(), expr.clone()).prop_map(|(cond, cons, alt)| ast::Expr::If(
                Box::new(ast::IfExpr { cond, cons, alt })
            )),
            (expr.clone(), arb_label_symbol().prop_map(spanned))
                .prop_map(|(expr, field)| ast::Expr::Proj(Box::new(ast::ProjExpr { expr, field }))),
            (
                arb_label_symbol().prop_map(spanned),
                prop::option::weighted(0.8, expr.clone())
            )
                .prop_map(|(tag, expr)| ast::Expr::Enum(Box::new(ast::EnumExpr { tag, expr }))),
            (
                expr.clone(),
                arb_symbol_map(arb_match_case(expr.clone()).boxed())
            )
                .prop_map(|(expr, cases)| ast::Expr::Match(Box::new(ast::MatchExpr {
                    expr,
                    cases
                }))),
        ]
        .prop_map(spanned)
    })
}

fn arb_func(expr: BoxedStrategy<ast::Spanned<ast::Expr>>) -> impl Strategy<Value = ast::FuncExpr> {
    (arb_var_symbol().prop_map(spanned), expr).prop_map(|(arg, body)| ast::FuncExpr { arg, body })
}

fn arb_match_case(
    expr: BoxedStrategy<ast::Spanned<ast::Expr>>,
) -> impl Strategy<Value = ast::Spanned<ast::MatchExprCase>> {
    (prop::option::of(arb_label_symbol().prop_map(spanned)), expr)
        .prop_map(|(name, expr)| ast::MatchExprCase { name, expr })
        .prop_map(spanned)
}

fn arb_symbol_map<T: Debug>(strat: BoxedStrategy<T>) -> impl Strategy<Value = SymbolMap<T>> {
    prop::collection::hash_map(arb_label_symbol(), strat, 0..4)
        .prop_map(|map| map.into_iter().collect())
}

fn arb_label_symbol() -> impl Strategy<Value = Symbol> {
    prop_oneof![
        Just(Symbol::new("p")),
        Just(Symbol::new("q")),
        Just(Symbol::new("l")),
        Just(Symbol::new("r")),
    ]
}

fn arb_var_symbol() -> impl Strategy<Value = Symbol> {
    prop_oneof![
        5 => Just(Symbol::new("a")),
        4 => Just(Symbol::new("b")),
        3 => Just(Symbol::new("c")),
        1 => Just(Symbol::new("__builtin_eq")),
        1 => Just(Symbol::new("__builtin_get_add")),
        1 => Just(Symbol::new("__builtin_get_sub")),
    ]
}

fn spanned<T>(val: T) -> ast::Spanned<T> {
    ast::Spanned {
        val,
        span: Span::initial(),
    }
}

pub fn dummy_file_id() -> FileId {
    codespan::Files::new().add("", "")
}

proptest! {
    #[test]
    fn parse_roundtrip(expr in arb_expr()) {
        let source = expr.to_string();
        let parsed = ast::Expr::parse(&source).unwrap();
        assert_eq!(expr, parsed)
    }
}
