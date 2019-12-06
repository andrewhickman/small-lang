use crate::check::ir;
use crate::optimize::Transform;

pub(in crate::optimize) static INSTANCE: &(dyn Transform + Send + Sync) = &InlineIife;

struct InlineIife;

impl Transform for InlineIife {
    fn transform(&self, expr: ir::Expr) -> (ir::Expr, u32) {
        let mut cost = 20u32;
        let expr = expr.map(&mut |expr| {
            let (expr, changes) = transform(expr);
            cost = 1 + cost.saturating_sub(changes);
            expr
        });
        (expr, cost)
    }
}

fn transform(expr: ir::Expr) -> (ir::Expr, u32) {
    match expr {
        ir::Expr::Call(call) if call.func.is_func() => match *call {
            ir::Call {
                func: ir::Expr::Func(func),
                arg,
            } => (
                ir::Expr::Let(Box::new(ir::Let {
                    name: func.arg,
                    val: arg,
                    body: func.body,
                })),
                4,
            ),
            _ => unreachable!(),
        },
        _ => (expr, 0),
    }
}

#[test]
fn test_transform() {
    use crate::optimize::tests::generate_ir;

    assert_eq!(
        transform(generate_ir("(func x => x) null")),
        (generate_ir("let x = null in x"), 4)
    );
    assert_eq!(
        transform(generate_ir("{ a: 5 }")),
        (generate_ir("{ a: 5 }"), 0)
    );
}
