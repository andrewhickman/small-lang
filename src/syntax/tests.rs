use std::fmt::Debug;

use codespan::Span;
use proptest::prelude::*;

use crate::syntax::*;

pub fn arb_expr() -> impl Strategy<Value = Spanned<Expr>> {
    prop_oneof![
        2 => prop::strategy::LazyJust::new(|| Expr::Null),
        3 => any::<bool>().prop_map(Expr::Bool),
        3 => any::<i64>().prop_map(Expr::Int),
        2 => arb_symbol().prop_map(Expr::Var),
    ]
    .prop_map(spanned)
    .prop_recursive(8, 128, 4, |expr| {
        prop_oneof![
            arb_symbol_map(expr.clone()).prop_map(Expr::Record),
            arb_func(expr.clone()).prop_map(|func| Expr::Func(Box::new(func))),
            (expr.clone(), expr.clone())
                .prop_map(|(func, arg)| Expr::Call(Box::new(CallExpr { func, arg }))),
            (arb_symbol().prop_map(spanned), expr.clone(), expr.clone())
                .prop_map(|(name, val, body)| Expr::Let(Box::new(LetExpr { name, val, body }))),
            (
                arb_symbol().prop_map(spanned),
                arb_func(expr.clone()).prop_map(spanned),
                expr.clone()
            )
                .prop_map(|(name, func, body)| Expr::Rec(Box::new(RecExpr {
                    name,
                    func,
                    body
                }))),
            (expr.clone(), expr.clone(), expr.clone())
                .prop_map(|(cond, cons, alt)| Expr::If(Box::new(IfExpr { cond, cons, alt }))),
            (expr.clone(), arb_symbol().prop_map(spanned))
                .prop_map(|(expr, field)| Expr::Proj(Box::new(ProjExpr { expr, field }))),
            (
                arb_symbol().prop_map(spanned),
                prop::option::weighted(0.8, expr.clone())
            )
                .prop_map(|(tag, expr)| Expr::Enum(Box::new(EnumExpr { tag, expr }))),
            (
                expr.clone(),
                arb_symbol_map(arb_match_case(expr.clone()).boxed())
            )
                .prop_map(|(expr, cases)| Expr::Match(Box::new(MatchExpr { expr, cases }))),
        ]
        .prop_map(spanned)
    })
}

fn arb_func(expr: BoxedStrategy<Spanned<Expr>>) -> impl Strategy<Value = FuncExpr> {
    (arb_symbol().prop_map(spanned), expr).prop_map(|(arg, body)| FuncExpr { arg, body })
}

fn arb_match_case(
    expr: BoxedStrategy<Spanned<Expr>>,
) -> impl Strategy<Value = Spanned<MatchExprCase>> {
    (prop::option::of(arb_symbol().prop_map(spanned)), expr)
        .prop_map(|(name, expr)| MatchExprCase { name, expr })
        .prop_map(spanned)
}

fn arb_symbol_map<T: Debug>(strat: BoxedStrategy<T>) -> impl Strategy<Value = SymbolMap<T>> {
    prop::collection::hash_map(arb_symbol(), strat, 0..4).prop_map(|map| map.into_iter().collect())
}

fn arb_symbol() -> impl Strategy<Value = Symbol> {
    prop_oneof![
        5 => Just(Symbol::new("a")),
        4 => Just(Symbol::new("b")),
        3 => Just(Symbol::new("l")),
        3 => Just(Symbol::new("r")),
        1 => Just(Symbol::new("__builtin_eq")),
        1 => Just(Symbol::new("__builtin_add")),
        1 => Just(Symbol::new("__builtin_sub")),
    ]
}

fn spanned<T>(val: T) -> Spanned<T> {
    Spanned {
        val,
        span: Span::initial(),
    }
}
