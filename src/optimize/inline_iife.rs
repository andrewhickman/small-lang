use crate::check::ir;
use crate::optimize::Transform;

pub(in crate::optimize) static INSTANCE: &(dyn Transform + Send + Sync) = &InlineIife;

struct InlineIife;

impl Transform for InlineIife {
    fn transform(&self, expr: ir::Expr, budget: &mut u32) -> ir::Expr {
        let mut sub_budget = 20u32;
        let expr = expr.map(&mut |expr| {
            let (expr, cost) = transform(expr);
            sub_budget = sub_budget.saturating_sub(cost);
            expr
        });
        *budget = budget.saturating_sub(1 + sub_budget);
        expr
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
fn inline_iife() {
    use crate::check::vars::VarId;
    use crate::rt;

    assert_eq!(
        transform(ir::Expr::Call(Box::new(ir::Call {
            func: ir::Expr::Func(Box::new(ir::Func {
                arg: VarId::new(3),
                body: ir::Expr::Var(VarId::new(3)),
                rec_var: None,
                captured_vars: vec![VarId::new(4)],
            })),
            arg: ir::Expr::Literal(rt::Value::Null),
        }))),
        (
            ir::Expr::Let(Box::new(ir::Let {
                name: VarId::new(3),
                val: ir::Expr::Literal(rt::Value::Null),
                body: ir::Expr::Var(VarId::new(3)),
            })),
            4
        )
    );
}
