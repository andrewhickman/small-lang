use proptest::proptest;

use crate::check::{check, ir};
use crate::generate::generate;
use crate::optimize::{optimize, Opts};
use crate::rt;
use crate::syntax::tests::{arb_expr, dummy_file_id};
use crate::syntax::SourceMap;

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
        if let Ok(expr) = check(&mut SourceMap::new(), dummy_file_id(), &expr) {
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
        (Some(lhs), Some(rhs)) => {
            serde_json::to_string(lhs).unwrap() == serde_json::to_string(rhs).unwrap()
        }
        (Some(_), None) | (None, Some(_)) => false,
        (None, None) => true,
    }
}
