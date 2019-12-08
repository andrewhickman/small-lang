use crate::check::ir;
use crate::optimize::Transform;

pub(in crate::optimize) static INSTANCE: &(dyn Transform + Send + Sync) = &InlineIife;

struct InlineIife;

impl Transform for InlineIife {
    fn transform(&self, expr: &mut ir::Expr) -> u32 {
        let mut cost = 20u32;
        expr.nodes.iter(|nodes, id| {
            let changes = transform(nodes, id);
            cost = 1 + cost.saturating_sub(changes);
        });
        cost
    }
}

fn transform(nodes: &mut ir::Nodes, id: ir::NodeId) -> u32 {
    match &nodes[id] {
        ir::Node::Call(call) => match &nodes[call.func] {
            &ir::Node::Func(func) => {
                nodes[id] = ir::Node::Let(ir::Let {
                    name: func.arg,
                    val: call.arg,
                    body: func.body,
                });
                4
            }
            _ => 0,
        },
        _ => 0,
    }
}

#[test]
fn test_transform() {
    use crate::optimize::tests::generate_ir;

    fn transformed(mut ir: ir::Expr) -> (ir::Expr, u32) {
        let cost = transform(&mut ir.nodes, ir.id);
        (ir, cost)
    }

    assert_eq!(
        transformed(generate_ir("(func x => x) null")),
        (generate_ir("let x = null in x"), 4)
    );
    assert_eq!(
        transformed(generate_ir("{ a: 5 }")),
        (generate_ir("{ a: 5 }"), 0)
    );
    assert_eq!(
        transformed(generate_ir(
            "(func arg => let not = (func x => if x then false else true) in not (not arg)) true"
        )),
        (generate_ir(
            "let arg = true in let not = (func x => if x then false else true) in not (not arg)"
        ), 4)
    );
}

#[test]
fn test_transform_full() {
    use crate::optimize::tests::generate_ir;

    fn transformed(mut ir: ir::Expr) -> (ir::Expr, u32) {
        let cost = INSTANCE.transform(&mut ir);
        (ir, cost)
    }

    assert_eq!(
        transformed(generate_ir(
            "(func _4 => if _4 then false else true) ((func _4 => if _4 then false else true) true)"
        )),
        (
            generate_ir("let _4 = (let _4 = true in if _4 then false else true) in if _4 then false else true"),
            25
        )
    );
}
