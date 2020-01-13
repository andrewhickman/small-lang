use std::collections::BTreeMap;

use crate::check::ir;
use crate::check::vars::VarId;
use crate::rt;
use crate::syntax::Symbol;

pub trait Visitor<T> {
    fn visit_node(&mut self, id: ir::NodeId);

    fn visit_expr(&mut self, id: ir::NodeId, expr: &ir::Node<T>) {
        self.visit_expr_match(id, expr)
    }

    fn visit_expr_match(&mut self, id: ir::NodeId, expr: &ir::Node<T>) {
        match expr {
            ir::Node::Literal(value) => self.visit_literal(id, value),
            ir::Node::Var(var) => self.visit_var(id, *var),
            ir::Node::Call(call_expr) => self.visit_call(id, call_expr),
            ir::Node::Let(let_expr) => self.visit_let(id, let_expr),
            ir::Node::Func(func_expr) => self.visit_func(id, func_expr),
            ir::Node::If(if_expr) => self.visit_if(id, if_expr),
            ir::Node::Proj(proj_expr) => self.visit_proj(id, proj_expr),
            ir::Node::Enum(enum_expr) => self.visit_enum(id, enum_expr),
            ir::Node::Record(record_expr) => self.visit_record(id, record_expr),
            ir::Node::Match(match_expr) => self.visit_match(id, match_expr),
            ir::Node::Import(import_expr) => self.visit_import(id, import_expr),
            ir::Node::Ref(ref_id) => self.visit_ref(id, *ref_id),
        }
    }

    fn visit_literal(&mut self, _id: ir::NodeId, _value: &rt::Value) {}

    fn visit_var(&mut self, _id: ir::NodeId, _var: VarId) {}

    fn visit_call(&mut self, _id: ir::NodeId, call_expr: &ir::Call) {
        self.visit_node(call_expr.arg);
        self.visit_node(call_expr.func);
    }

    fn visit_let(&mut self, _id: ir::NodeId, let_expr: &ir::Let) {
        self.visit_node(let_expr.val);
        self.visit_node(let_expr.body);
    }

    fn visit_func(&mut self, _id: ir::NodeId, func_expr: &ir::Func) {
        self.visit_node(func_expr.body);
    }

    fn visit_if(&mut self, _id: ir::NodeId, if_expr: &ir::If) {
        self.visit_node(if_expr.cond);
        self.visit_node(if_expr.cons);
        self.visit_node(if_expr.alt);
    }

    fn visit_proj(&mut self, _id: ir::NodeId, proj_expr: &ir::Proj) {
        self.visit_node(proj_expr.expr);
    }

    fn visit_enum(&mut self, _id: ir::NodeId, enum_expr: &ir::Enum) {
        self.visit_node(enum_expr.expr);
    }

    fn visit_record(&mut self, _id: ir::NodeId, record_expr: &BTreeMap<Symbol, ir::NodeId>) {
        for &expr in record_expr.values() {
            self.visit_node(expr);
        }
    }

    fn visit_match(&mut self, _id: ir::NodeId, record_expr: &ir::Match) {
        self.visit_node(record_expr.expr);
        for &case in record_expr.cases.values() {
            self.visit_node(case);
        }
    }

    fn visit_import(&mut self, _id: ir::NodeId, _import: &T) {}

    fn visit_ref(&mut self, _id: ir::NodeId, ref_id: ir::NodeId) {
        self.visit_node(ref_id)
    }
}
