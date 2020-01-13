use std::collections::HashMap;
use std::iter::{once, FromIterator};

use crate::check::ir;
use crate::optimize::Transform;

pub struct InlineIife;

impl<T: Clone> Transform<T> for InlineIife {
    fn transform(&self, expr: &mut ir::Expr<T>) -> u32 {
        let mut cost = 20u32;
        expr.nodes.visit_mut(expr.id, |nodes, id| {
            let changes = transform(nodes, id);
            cost = 1 + cost.saturating_sub(changes);
        });

        log::debug!("Applied inline_iife transform. Cost: {:?}", cost);
        cost
    }
}

fn transform<T: Clone>(nodes: &mut ir::Nodes<T>, id: ir::NodeId) -> u32 {
    match &nodes[id] {
        &ir::Node::Call(call) => {
            let call_func = nodes.deref_id(call.func);
            match &nodes[call_func] {
                &ir::Node::Func(func) => {
                    let cloned_func_body = nodes
                        .clone_node_with(func.body, &mut HashMap::from_iter(once((call_func, id))));

                    nodes[id] = ir::Node::Let(ir::Let {
                        val: call.arg,
                        body: cloned_func_body,
                    });
                    4
                }
                _ => 0,
            }
        }
        _ => 0,
    }
}

#[test]
fn test_transform() {
    use std::rc::Rc;

    use crate::optimize::tests::generate_ir;
    use crate::rt::Command;

    fn transformed(mut ir: ir::Expr<Rc<[Command]>>) -> (ir::Expr<Rc<[Command]>>, u32) {
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
    use std::rc::Rc;

    use crate::optimize::tests::generate_ir;
    use crate::rt::Command;

    fn transformed(mut ir: ir::Expr<Rc<[Command]>>) -> (ir::Expr<Rc<[Command]>>, u32) {
        let cost = InlineIife.transform(&mut ir);
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
