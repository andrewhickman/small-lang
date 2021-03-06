use std::collections::{BTreeMap, HashMap};
use std::iter::FromIterator;
use std::mem::replace;
use std::rc::Rc;

use small_ord_set::SmallOrdSet;

use crate::check::ir::{self, Visitor as _};
use crate::check::vars::VarId;
use crate::rt;
use crate::syntax::{ImSymbolMap, Symbol};

pub fn generate(expr: &ir::Expr) -> Vec<rt::Command> {
    let mut visitor = GenerateVisitor::new(&expr.nodes);
    visitor.visit_node(expr.id);
    visitor.cmds
}

struct GenerateVisitor<'a> {
    ir: &'a ir::Nodes,
    cmds: Vec<rt::Command>,
    cache: HashMap<ir::NodeId, Vec<rt::Command>>,
    rec_vars: Vec<Option<VarId>>,
    is_tail: bool,
}

type CapturedVars = SmallOrdSet<[VarId; 8]>;

struct CaptureVisitor<'a> {
    ir: &'a ir::Nodes,
    local_vars: SmallOrdSet<[VarId; 16]>,
    captured_vars: CapturedVars,
}

impl<'a> GenerateVisitor<'a> {
    fn new(ir: &'a ir::Nodes) -> Self {
        GenerateVisitor {
            cmds: Vec::with_capacity(16),
            cache: HashMap::new(),
            rec_vars: Vec::new(),
            is_tail: true,
            ir,
        }
    }
}

impl<'a> ir::Visitor for GenerateVisitor<'a> {
    fn visit_literal(&mut self, _id: ir::NodeId, value: &rt::Value) {
        self.cmds.push(rt::Command::Push {
            value: value.clone(),
        })
    }

    fn visit_var(&mut self, _id: ir::NodeId, var: VarId) {
        self.cmds.push(rt::Command::Load { var })
    }

    fn visit_call(&mut self, _id: ir::NodeId, call_expr: &ir::Call) {
        self.visit_init_node(call_expr.arg);

        if self.is_tail_call(call_expr) {
            self.cmds.push(rt::Command::Become);
        } else {
            self.visit_init_node(call_expr.func);
            self.cmds.push(rt::Command::Call);
        }
    }

    fn visit_let(&mut self, var: VarId, let_expr: &ir::Let) {
        self.visit_init_node(let_expr.val);
        self.cmds.push(rt::Command::Store { var });
        self.visit_node(let_expr.body);
    }

    fn visit_func(&mut self, var: VarId, func_expr: &ir::Func) {
        let start = self.cmds.len();

        let captured_vars = CaptureVisitor::get_captures(self.ir, var, func_expr);

        self.cmds.push(rt::Command::Store { var });
        self.visit_func_body(func_expr);

        let capture = rt::Command::Capture {
            span: func_expr.span,
            rec_var: func_expr.rec_var,
            cmds: self.cmds.drain(start..).collect(),
            vars: Vec::from_iter(captured_vars),
        };
        self.cmds.push(capture);
    }

    fn visit_if(&mut self, _id: ir::NodeId, if_expr: &ir::If) {
        self.visit_init_node(if_expr.cond);

        self.cmds.push(rt::Command::Trap);
        let alt_pos = self.cmds.len();
        self.visit_node(if_expr.alt);

        self.cmds.push(rt::Command::Trap);
        let cons_pos = self.cmds.len();
        self.visit_node(if_expr.cons);

        self.cmds[alt_pos - 1] = rt::Command::Test {
            jump_offset: cons_pos - alt_pos,
        };
        self.cmds[cons_pos - 1] = rt::Command::Jump {
            jump_offset: self.cmds.len() - cons_pos,
        };
    }

    fn visit_proj(&mut self, _id: ir::NodeId, proj_expr: &ir::Proj) {
        self.visit_init_node(proj_expr.expr);
        self.cmds.push(rt::Command::Get {
            field: proj_expr.field,
        });
    }

    fn visit_enum(&mut self, _id: ir::NodeId, enum_expr: &ir::Enum) {
        self.visit_init_node(enum_expr.expr);
        self.cmds.push(rt::Command::WrapEnum { tag: enum_expr.tag });
    }

    fn visit_record(&mut self, _id: ir::NodeId, record_expr: &BTreeMap<Symbol, ir::NodeId>) {
        self.cmds.push(rt::Command::Push {
            value: rt::Value::Record(Default::default()),
        });
        for (&field, &val) in record_expr {
            self.visit_init_node(val);
            self.cmds.push(rt::Command::Set { field });
        }
    }

    fn visit_match(&mut self, var: VarId, match_expr: &ir::Match) {
        self.visit_init_node(match_expr.expr);

        let mut jump_offsets = ImSymbolMap::default();

        self.cmds.push(rt::Command::Trap);
        let cases_pos = self.cmds.len();
        for (&tag, &case) in &match_expr.cases {
            self.cmds.push(rt::Command::Trap);
            let case_pos = self.cmds.len();
            self.cmds.push(rt::Command::Store { var });
            self.visit_node(case);

            jump_offsets.insert(tag, case_pos - cases_pos);
        }

        for (_, offset) in &jump_offsets {
            let case_pos = cases_pos + offset;

            self.cmds[case_pos - 1] = rt::Command::Jump {
                jump_offset: self.cmds.len() - case_pos,
            };
        }
        self.cmds[cases_pos - 1] = rt::Command::Match { jump_offsets };
    }

    fn visit_import(&mut self, _id: ir::NodeId, cmds: &Rc<[rt::Command]>) {
        self.cmds.push(rt::Command::Import { cmds: cmds.clone() });
    }

    fn visit_node(&mut self, node: ir::NodeId) {
        let node = self.ir.deref_id(node);
        if let Some(cached) = self.cache.get(&node) {
            self.cmds.extend(cached.iter().cloned());
        } else {
            let start = self.cmds.len();
            self.visit_expr(node, &self.ir[node]);
            let cmds = Vec::from(&self.cmds[start..]);
            self.cache.insert(node, cmds);
        }
    }
}

impl<'a> GenerateVisitor<'a> {
    fn visit_init_node(&mut self, node: ir::NodeId) {
        let was_tail = replace(&mut self.is_tail, false);
        self.visit_node(node);
        self.is_tail = was_tail;
    }

    fn visit_func_body(&mut self, func: &ir::Func) {
        self.rec_vars.push(func.rec_var);

        let was_tail = replace(&mut self.is_tail, true);
        self.visit_node(func.body);
        self.is_tail = was_tail;

        self.rec_vars.pop();
    }

    fn is_tail_call(&mut self, call: &ir::Call) -> bool {
        if let Some(&Some(rec_func)) = self.rec_vars.last() {
            if let ir::Node::Var(func) = self.ir[call.func] {
                self.is_tail && rec_func == func
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl<'a> ir::Visitor for CaptureVisitor<'a> {
    fn visit_node(&mut self, node: ir::NodeId) {
        self.visit_expr(node, &self.ir[node]);
    }

    fn visit_var(&mut self, _id: ir::NodeId, var: VarId) {
        if !var.is_builtin() && !self.local_vars.contains(&var) {
            self.captured_vars.insert(var);
        }
    }

    fn visit_let(&mut self, var: VarId, let_expr: &ir::Let) {
        self.visit_node(let_expr.val);
        self.local_vars.insert(var);
        self.visit_node(let_expr.body);
        self.local_vars.remove(&var);
    }

    fn visit_func(&mut self, var: VarId, func_expr: &ir::Func) {
        if let Some(var) = func_expr.rec_var {
            self.local_vars.insert(var);
        }
        self.local_vars.insert(var);
        self.visit_node(func_expr.body);
        self.local_vars.remove(&var);
        if let Some(var) = func_expr.rec_var {
            self.local_vars.remove(&var);
        }
    }

    fn visit_match(&mut self, var: VarId, record_expr: &ir::Match) {
        self.visit_node(record_expr.expr);
        self.local_vars.insert(var);
        for &case in record_expr.cases.values() {
            self.visit_node(case);
        }
        self.local_vars.remove(&var);
    }
}

impl<'a> CaptureVisitor<'a> {
    fn get_captures(ir: &ir::Nodes, id: ir::NodeId, func_expr: &ir::Func) -> CapturedVars {
        let mut visitor = CaptureVisitor {
            ir,
            local_vars: SmallOrdSet::new(),
            captured_vars: SmallOrdSet::new(),
        };
        visitor.visit_func(id, &func_expr);
        visitor.captured_vars
    }
}
