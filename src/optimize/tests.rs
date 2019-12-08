use std::rc::Rc;

use codespan::FileId;
use proptest::proptest;

use crate::check::{check, ir};
use crate::generate::generate;
use crate::optimize::{optimize, Opts};
use crate::pipeline::{Pipeline, ProcessOutput, ProcessResult, Source};
use crate::rt;
use crate::syntax::tests::{arb_expr, dummy_file_id};

pub fn generate_ir(input: impl Into<String>) -> ir::Expr {
    Pipeline::new()
        .process_root_rc(Source::Input(input.into()), generate_ir_impl)
        .into_result()
        .unwrap()
}

fn generate_ir_impl(
    _pipeline: &mut Pipeline<Rc<ir::Expr>>,
    file: FileId,
    input: String,
) -> ProcessResult<ir::Expr> {
    let ast = crate::syntax::parse(file, &input)?;
    let (_, expr, warnings) = check(file, &ast, |_| unreachable!())?;
    Ok(ProcessOutput {
        value: expr,
        warnings,
    })
}

const RUNTIME_OPTIONS: rt::Opts = rt::Opts {
    max_stack: 32,
    max_ops: Some(1_048_576),
};

fn run(expr: &ir::Expr) -> Option<rt::Value> {
    let cmds = generate(expr);
    match rt::run(&cmds, RUNTIME_OPTIONS) {
        Ok(output) => Some(output.value),
        Err(_) => None,
    }
}

fn run_optimized(mut expr: ir::Expr) -> Option<rt::Value> {
    optimize(&mut expr, Opts { opt_level: 3 });
    run(&expr)
}

proptest! {
    #[test]
    fn optimization_correctness(expr in arb_expr()) {
        if let Ok((_, expr, _)) = check(dummy_file_id(), &expr, |_| unreachable!()) {
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

impl ir::Node {
    pub fn unwrap_let(&self) -> ir::Let {
        match self {
            &ir::Node::Let(let_expr) => let_expr,
            _ => panic!("expected let"),
        }
    }
}
