use std::rc::Rc;

use proptest::prelude::*;

use crate::syntax::{Expr, Symbol, SymbolMap};

pub fn arb_expr() -> impl Strategy<Value = Expr> {
    prop_oneof![
        Just(Expr::True),
        Just(Expr::False),
        arb_symbol().prop_map(Expr::Var),
    ].prop_recursive(8, 128, 4, |expr| prop_oneof![
        arb_symbol_map(expr.clone()).prop_map(Expr::Cons),
        arb_func(expr.clone()),
        (expr.clone(), expr.clone()).prop_map(|(f, e)| Expr::App(Rc::new(f), Rc::new(e))),
        (arb_symbol(), expr.clone(), expr.clone()).prop_map(|(s, v, e)| Expr::Let(s, Rc::new(v), Rc::new(e))),
        (arb_symbol(), arb_func(expr.clone()), expr.clone()).prop_map(|(s, v, e)| Expr::Rec(s, Rc::new(v), Rc::new(e))),
        (expr.clone(), expr.clone(), expr.clone()).prop_map(|(a, b, c)| Expr::If(Rc::new(a), Rc::new(b), Rc::new(c))),
        (expr.clone(), arb_symbol()).prop_map(|(e, s)| Expr::Proj(Rc::new(e), s)),
    ])
}

fn arb_func(expr: BoxedStrategy<Expr>) -> impl Strategy<Value = Expr> {
    (arb_symbol(), expr).prop_map(|(s, e)| Expr::Abs(s, Rc::new(e)))
}

fn arb_symbol_map(expr: BoxedStrategy<Expr>) -> impl Strategy<Value = SymbolMap<Expr>> {
    prop::collection::hash_map(arb_symbol(), expr, 0..4)
        .prop_map(|map| map.into_iter().collect())
}

fn arb_symbol() -> impl Strategy<Value = Symbol> {
    prop_oneof![
        Just(Symbol::new("a")),
        Just(Symbol::new("b")),
        Just(Symbol::new("c")),
        Just(Symbol::new("d")),
    ]
}