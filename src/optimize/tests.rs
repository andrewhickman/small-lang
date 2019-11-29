use proptest::proptest;

use crate::check::{check, ir};
use crate::generate::generate;
use crate::optimize::{optimize, Opts};
use crate::rt;
use crate::syntax::tests::{arb_expr, dummy_file_id};

const RUNTIME_OPTIONS: rt::Opts = rt::Opts {
    max_stack: 1024,
    max_ops: Some(1_048_576),
};

fn run(expr: &ir::Expr) -> Option<rt::Value> {
    let cmds = generate(expr);
    match rt::run(&cmds, RUNTIME_OPTIONS) {
        Ok(output) => Some(output.value),
        Err(_) => None,
    }
}

fn run_optimized(expr: ir::Expr) -> Option<rt::Value> {
    run(&optimize(expr, Opts { opt_level: 3 }))
}

proptest! {
    #[test]
    fn optimization_correctness(expr in arb_expr()) {
        if let Ok((_, expr)) = check(dummy_file_id(), &expr, |_| unreachable!()) {
            let original = run(&expr);
            let optimized = run_optimized(expr);
            assert!(
                structural_eq(&original, &optimized),
                "expression not equivalent to optimized expression\noriginal: {:?}\noptimized: {:?}",
                original,
                optimized
            );
        }
    }
}

fn structural_eq(lhs: &Option<rt::Value>, rhs: &Option<rt::Value>) -> bool {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => structural_eq_value(lhs, rhs),
        (Some(_), None) | (None, Some(_)) => false,
        (None, None) => true,
    }
}

fn structural_eq_value(lhs: &rt::Value, rhs: &rt::Value) -> bool {
    match (lhs, rhs) {
        (rt::Value::Func(_), rt::Value::Func(_)) => true, // may be different due to optimization,
        (rt::Value::Builtin { name: l, .. }, rt::Value::Builtin { name: r, .. }) => l == r,
        (rt::Value::Enum(lhs), rt::Value::Enum(rhs)) => {
            lhs.tag == rhs.tag && structural_eq_value(&lhs.value, &rhs.value)
        }
        (rt::Value::Record(lhs), rt::Value::Record(rhs)) => {
            itertools::zip_eq(lhs, rhs).all(|(l, r)| l.0 == r.0 && structural_eq_value(&l.1, &r.1))
        }
        _ => PartialEq::eq(lhs, rhs),
    }
}
