use std::collections::BTreeMap;

use crate::check::ir::{self, Visitor as _};
use crate::check::vars::VarId;
use crate::optimize::Transform;
use crate::syntax::Symbol;

pub(in crate::optimize) static INSTANCE: &(dyn Transform + Send + Sync) = &InlineLet;

struct InlineLet;

impl Transform for InlineLet {
    fn transform(&self, expr: &mut ir::Expr) -> u32 {
        let mut cost = 40u32;
        expr.nodes.visit_mut(expr.id, |nodes, id| {
            let changes = transform(nodes, id);
            cost = 1 + cost.saturating_sub(changes);
        });
        cost
    }
}

fn transform(nodes: &mut ir::Nodes, id: ir::NodeId) -> u32 {
    match &nodes[id] {
        &ir::Node::Let(let_expr) => {
            if let Some((changes, occurrences)) = should_inline(nodes, id, &let_expr) {
                for occurrence in occurrences {
                    nodes[occurrence] = ir::Node::Ref(let_expr.val);
                }
                nodes[id] = ir::Node::Ref(let_expr.body);
                changes
            } else {
                0
            }
        }
        _ => 0,
    }
}

fn should_inline(
    nodes: &ir::Nodes,
    id: ir::NodeId,
    let_expr: &ir::Let,
) -> Option<(u32, Vec<ir::NodeId>)> {
    match nodes[let_expr.val] {
        ir::Node::Func(func) if func.rec_var.is_some() => return None,
        _ => (),
    };

    let size = size(nodes, let_expr.val);
    let occurrences = occurrences(nodes, let_expr.body, id);
    if (size * occurrences.len() as u32) < (size + 40) {
        Some((size, occurrences))
    } else {
        None
    }
}

fn size(nodes: &ir::Nodes, expr: ir::NodeId) -> u32 {
    let mut visitor = SizeVisitor { nodes, size: 0 };
    visitor.visit_node(expr);
    visitor.size
}

fn occurrences(nodes: &ir::Nodes, expr: ir::NodeId, var: VarId) -> Vec<ir::NodeId> {
    let mut visitor = OccurrencesVisitor {
        nodes,
        var,
        occurrences: Vec::new(),
    };
    visitor.visit_node(expr);
    visitor.occurrences
}

struct SizeVisitor<'a> {
    nodes: &'a ir::Nodes,
    size: u32,
}

struct OccurrencesVisitor<'a> {
    nodes: &'a ir::Nodes,
    var: VarId,
    occurrences: Vec<ir::NodeId>,
}

impl<'a> ir::Visitor for SizeVisitor<'a> {
    fn visit_node(&mut self, id: ir::NodeId) {
        self.visit_expr(id, &self.nodes[id]);
    }

    fn visit_call(&mut self, _id: ir::NodeId, call_expr: &ir::Call) {
        self.visit_node(call_expr.arg);
        self.visit_node(call_expr.func);
        self.size += 20;
    }

    fn visit_let(&mut self, _id: ir::NodeId, let_expr: &ir::Let) {
        self.visit_node(let_expr.val);
        self.visit_node(let_expr.body);
        self.size += 20;
    }

    fn visit_func(&mut self, _id: ir::NodeId, func_expr: &ir::Func) {
        self.visit_node(func_expr.body);
        self.size += 1;
    }

    fn visit_if(&mut self, _id: ir::NodeId, if_expr: &ir::If) {
        self.visit_node(if_expr.cond);
        self.visit_node(if_expr.cons);
        self.visit_node(if_expr.alt);
        self.size += 3;
    }

    fn visit_proj(&mut self, _id: ir::NodeId, proj_expr: &ir::Proj) {
        self.visit_node(proj_expr.expr);
        self.size += 1;
    }

    fn visit_enum(&mut self, _id: ir::NodeId, enum_expr: &ir::Enum) {
        self.visit_node(enum_expr.expr);
        self.size += 1;
    }

    fn visit_record(&mut self, _id: ir::NodeId, record_expr: &BTreeMap<Symbol, ir::NodeId>) {
        for &expr in record_expr.values() {
            self.visit_node(expr);
        }
        self.size += 1;
    }

    fn visit_match(&mut self, _id: ir::NodeId, record_expr: &ir::Match) {
        self.visit_node(record_expr.expr);
        for &expr in record_expr.cases.values() {
            self.visit_node(expr);
        }
        self.size += 5;
    }
}

impl<'a> ir::Visitor for OccurrencesVisitor<'a> {
    fn visit_node(&mut self, id: ir::NodeId) {
        self.visit_expr(id, &self.nodes[id])
    }

    fn visit_var(&mut self, id: ir::NodeId, var: VarId) {
        if var == self.var {
            self.occurrences.push(id);
        }
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
        transformed(generate_ir("let x = null in x")),
        (generate_ir("null"), 0)
    );
    assert_eq!(
        transformed(generate_ir("let x = [some: 5] in { x, y: x }")),
        (generate_ir("{ x: [some: 5], y: [some: 5] }"), 1)
    );
    assert_eq!(
        transformed(generate_ir("match [none] with [none => null]")),
        (generate_ir("match [none] with [none => null]"), 0)
    );
    assert_eq!(
        transformed(generate_ir(
            "(
              let not = func x => if x
                then false
                else true
              in
                func args => if args.l
                  then not args.r
                  else args.r
            ) { l: true, r: false, j: true }"
        )),
        (
            generate_ir(
                "(
                  let _3 = func _3 => if _3
                    then false
                    else true in
                  func _4 => if _4.l
                    then _3 _4.r
                    else _4.r
                ) {
                  l: true,
                  r: false,
                  j: true,
                }"
            ),
            0
        )
    );
    assert_eq!(
        transformed(generate_ir(
            "let arg = true in let not = (func x => if x then false else true) in not (not arg)"
        )),
        (
            generate_ir("let not = (func x => if x then false else true) in not (not true)"),
            0
        )
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
            "let arg = true in let not = (func x => if x then false else true) in not (not arg)"
        )),
        (
            generate_ir("(func _4 => if _4 then false else true) ((func _4 => if _4 then false else true) true)"),
            49
        )
    );
    assert_eq!(
        transformed(generate_ir(
            "let _4 = (let _4 = true in if _4 then false else true) in if _4 then false else true"
        )),
        (
            generate_ir("if (if true then false else true) then false else true"),
            48
        )
    );
}

#[test]
fn test_should_inline() {
    use crate::optimize::tests::generate_ir;

    fn should_inline_expr(expr: ir::Expr) -> Option<u32> {
        should_inline(&expr.nodes, expr.id, &expr.nodes[expr.id].unwrap_let())
            .map(|(changes, _)| changes)
    }

    assert!(should_inline_expr(generate_ir("let x = null in x")).is_some());
    assert!(should_inline_expr(generate_ir("let rec f = func _ => f in f null")).is_none());
    assert!(should_inline_expr(generate_ir(
        "let f = func x => (x x x x x x x x) in\
         {\
         a: f (func x => x),\
         b: f (func x => x),\
         c: f (func x => x),\
         }"
    ))
    .is_none());
}
