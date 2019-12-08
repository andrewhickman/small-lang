use std::collections::BTreeMap;
use std::rc::Rc;

use crate::check::ir;
use crate::check::vars::VarId;
use crate::rt;
use crate::syntax::Symbol;

pub trait Visitor<T = Rc<[rt::Command]>> {
    fn visit_node(&mut self, id: ir::NodeId);

    fn visit_expr(&mut self, expr: &ir::Node<T>) {
        self.visit_expr_match(expr)
    }

    fn visit_expr_match(&mut self, expr: &ir::Node<T>) {
        match expr {
            ir::Node::Literal(value) => self.visit_literal(value),
            ir::Node::Var(var) => self.visit_var(*var),
            ir::Node::Call(call_expr) => self.visit_call(call_expr),
            ir::Node::Let(let_expr) => self.visit_let(let_expr),
            ir::Node::Func(func_expr) => self.visit_func(func_expr),
            ir::Node::If(if_expr) => self.visit_if(if_expr),
            ir::Node::Proj(proj_expr) => self.visit_proj(proj_expr),
            ir::Node::Enum(enum_expr) => self.visit_enum(enum_expr),
            ir::Node::Record(record_expr) => self.visit_record(record_expr),
            ir::Node::Match(match_expr) => self.visit_match(match_expr),
            ir::Node::Import(import_expr) => self.visit_import(import_expr),
        }
    }

    fn visit_literal(&mut self, _value: &rt::Value) {}

    fn visit_var(&mut self, _var: VarId) {}

    fn visit_call(&mut self, call_expr: &ir::Call) {
        self.visit_node(call_expr.arg);
        self.visit_node(call_expr.func);
    }

    fn visit_let(&mut self, let_expr: &ir::Let) {
        self.visit_node(let_expr.val);
        self.visit_node(let_expr.body);
    }

    fn visit_func(&mut self, func_expr: &ir::Func) {
        self.visit_node(func_expr.body);
    }

    fn visit_if(&mut self, if_expr: &ir::If) {
        self.visit_node(if_expr.cond);
        self.visit_node(if_expr.cons);
        self.visit_node(if_expr.alt);
    }

    fn visit_proj(&mut self, proj_expr: &ir::Proj) {
        self.visit_node(proj_expr.expr);
    }

    fn visit_enum(&mut self, enum_expr: &ir::Enum) {
        self.visit_node(enum_expr.expr);
    }

    fn visit_record(&mut self, record_expr: &BTreeMap<Symbol, ir::NodeId>) {
        for &expr in record_expr.values() {
            self.visit_node(expr);
        }
    }

    fn visit_match(&mut self, record_expr: &ir::Match) {
        self.visit_node(record_expr.expr);
        for case in record_expr.cases.values() {
            self.visit_match_case(case);
        }
    }

    fn visit_match_case(&mut self, case: &ir::MatchCase) {
        self.visit_node(case.expr);
    }

    fn visit_import(&mut self, _import: &T) {}
}
