use std::collections::BTreeMap;
use std::rc::Rc;

use parity_wasm::elements::Module;

use crate::check::ir::{self, Visitor as _};
use crate::check::vars::VarId;
use crate::rt;
use crate::syntax::Symbol;

pub fn generate(expr: &ir::Expr<Rc<Module>>) -> Module {
    let mut visitor = GenerateVisitor::new(&expr.nodes);
    visitor.visit_node(expr.id);
    visitor.module
}

struct GenerateVisitor<'a> {
    ir: &'a ir::Nodes<Rc<Module>>,
    module: Module,
}

impl<'a> GenerateVisitor<'a> {
    fn new(ir: &'a ir::Nodes<Rc<Module>>) -> Self {
        GenerateVisitor {
            ir,
            module: Module::default(),
        }
    }
}

impl<'a> ir::Visitor<Rc<Module>> for GenerateVisitor<'a> {
    fn visit_literal(&mut self, _id: ir::NodeId, value: &rt::Value) {
        todo!()
    }

    fn visit_var(&mut self, _id: ir::NodeId, var: VarId) {
        todo!()
    }

    fn visit_call(&mut self, _id: ir::NodeId, call_expr: &ir::Call) {
        todo!()
    }

    fn visit_let(&mut self, var: VarId, let_expr: &ir::Let) {
        todo!()
    }

    fn visit_func(&mut self, var: VarId, func_expr: &ir::Func) {
        todo!()
    }

    fn visit_if(&mut self, _id: ir::NodeId, if_expr: &ir::If) {
        todo!()
    }

    fn visit_proj(&mut self, _id: ir::NodeId, proj_expr: &ir::Proj) {
        todo!()
    }

    fn visit_enum(&mut self, _id: ir::NodeId, enum_expr: &ir::Enum) {
        todo!()
    }

    fn visit_record(&mut self, _id: ir::NodeId, record_expr: &BTreeMap<Symbol, ir::NodeId>) {
        todo!()
    }

    fn visit_match(&mut self, var: VarId, match_expr: &ir::Match) {
        todo!()
    }

    fn visit_import(&mut self, _id: ir::NodeId, cmds: &Rc<Module>) {
        todo!()
    }

    fn visit_node(&mut self, node: ir::NodeId) {
        self.visit_expr(node, &self.ir[node]);
    }
}
