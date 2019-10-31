use proptest::prelude::*;

use codespan::Span;

use crate::syntax::*;

pub fn arb_expr() -> impl Strategy<Value = Spanned<Expr>> {
    arb_expr_impl().prop_map(|expr| {
        spanned(Expr::Let(Box::new(LetExpr {
            name: spanned(Symbol::new("std")),
            val: spanned(Expr::Import("std".to_owned())),
            body: expr,
        })))
    })
}

fn arb_expr_impl() -> impl Strategy<Value = Spanned<Expr>> {
    prop_oneof![
        3 => any::<bool>().prop_map(Expr::Bool),
        3 => any::<i64>().prop_map(Expr::Int),
        2 => arb_symbol().prop_map(Expr::Var),
        2 => prop_oneof![
            "eq",
            "add",
            "sub",
        ].prop_map(|item| {
            Expr::Proj(Box::new(ProjExpr {
                expr: spanned(Expr::Var(Symbol::new("std"))),
                field: spanned(Symbol::new(item)),
            }))
        }),
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
        ]
        .prop_map(spanned)
    })
}

fn arb_func(expr: BoxedStrategy<Spanned<Expr>>) -> impl Strategy<Value = FuncExpr> {
    (arb_symbol().prop_map(spanned), expr).prop_map(|(arg, body)| FuncExpr { arg, body })
}

fn arb_symbol_map(
    expr: BoxedStrategy<Spanned<Expr>>,
) -> impl Strategy<Value = SymbolMap<Spanned<Expr>>> {
    prop::collection::hash_map(arb_symbol(), expr, 0..4).prop_map(|map| map.into_iter().collect())
}

fn arb_symbol() -> impl Strategy<Value = Symbol> {
    prop_oneof![
        Just(Symbol::new("a")),
        Just(Symbol::new("b")),
        Just(Symbol::new("c")),
        Just(Symbol::new("d")),
    ]
}

fn spanned<T>(val: T) -> Spanned<T> {
    Spanned {
        val,
        span: Span::initial(),
    }
}
