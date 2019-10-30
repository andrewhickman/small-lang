use std::rc::Rc;

use proptest::prelude::*;

use crate::syntax::{Expr, Symbol, SymbolMap};

pub fn arb_expr() -> impl Strategy<Value = Expr> {
    arb_expr_impl().prop_map(|expr| {
        Expr::Let(
            Symbol::new("std"),
            Rc::new(Expr::Import("std".to_owned())),
            Rc::new(expr),
        )
    })
}

fn arb_expr_impl() -> impl Strategy<Value = Expr> {
    prop_oneof![
        3 => any::<bool>().prop_map(Expr::Bool),
        3 => any::<i64>().prop_map(Expr::Int),
        2 => arb_symbol().prop_map(Expr::Var),
        2 => prop_oneof![
            "eq",
            "add",
            "sub",
        ].prop_map(|item| {
            Expr::Proj(Rc::new(Expr::Var(Symbol::new("std"))), Symbol::new(item))
        }),
    ]
    .prop_recursive(8, 128, 4, |expr| {
        prop_oneof![
            arb_symbol_map(expr.clone()).prop_map(Expr::Record),
            arb_func(expr.clone()),
            (expr.clone(), expr.clone()).prop_map(|(f, e)| Expr::Call(Rc::new(f), Rc::new(e))),
            (arb_symbol(), expr.clone(), expr.clone()).prop_map(|(s, v, e)| Expr::Let(
                s,
                Rc::new(v),
                Rc::new(e)
            )),
            (arb_symbol(), arb_func(expr.clone()), expr.clone()).prop_map(|(s, v, e)| Expr::Rec(
                s,
                Rc::new(v),
                Rc::new(e)
            )),
            (expr.clone(), expr.clone(), expr.clone()).prop_map(|(a, b, c)| Expr::If(
                Rc::new(a),
                Rc::new(b),
                Rc::new(c)
            )),
            (expr.clone(), arb_symbol()).prop_map(|(e, s)| Expr::Proj(Rc::new(e), s)),
        ]
    })
}

fn arb_func(expr: BoxedStrategy<Expr>) -> impl Strategy<Value = Expr> {
    (arb_symbol(), expr).prop_map(|(s, e)| Expr::Func(s, Rc::new(e)))
}

fn arb_symbol_map(expr: BoxedStrategy<Expr>) -> impl Strategy<Value = SymbolMap<Expr>> {
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
