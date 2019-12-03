use std::iter::FromIterator;
use std::rc::Rc;

use small_ord_set::SmallOrdSet;

use crate::check::ir;
use crate::check::vars::VarId;
use crate::rt;
use crate::syntax::{ImSymbolMap, SymbolMap};

pub fn generate(expr: &ir::Expr) -> Vec<rt::Command> {
    let mut visitor = GenerateVisitor::new();
    expr.visit(&mut visitor);
    visitor.cmds
}

struct GenerateVisitor {
    cmds: Vec<rt::Command>,
    scopes: Vec<Scope>,
}

type CapturedVars = SmallOrdSet<[VarId; 8]>;

struct Scope {
    depth: u32,
}

struct CaptureVisitor {
    max_var: VarId,
    captured_vars: CapturedVars,
}

impl GenerateVisitor {
    fn new() -> Self {
        GenerateVisitor {
            cmds: Vec::with_capacity(16),
            scopes: Vec::new(),
        }
    }
}

impl ir::Visitor for GenerateVisitor {
    fn visit_literal(&mut self, value: &rt::Value) {
        self.cmds.push(rt::Command::Push {
            value: value.clone(),
        })
    }

    fn visit_var(&mut self, var: VarId) {
        self.cmds.push(rt::Command::Load { var })
    }

    fn visit_call(&mut self, call_expr: &ir::Call) {
        self.visit_expr(&call_expr.arg);
        self.visit_expr(&call_expr.func);
        self.cmds.push(rt::Command::Call);
    }

    fn visit_let(&mut self, let_expr: &ir::Let) {
        self.visit_expr(&let_expr.val);
        self.cmds.push(rt::Command::Store { var: let_expr.name });
        self.visit_expr(&let_expr.body);
    }

    fn visit_func(&mut self, func_expr: &ir::Func) {
        let start = self.cmds.len();

        let captured_vars = CaptureVisitor::get_captures(func_expr);
        self.push_scope(captured_vars.len() as u32);

        self.cmds.push(rt::Command::Store { var: func_expr.arg });
        self.visit_expr(&func_expr.body);

        assert_eq!(self.pop_scope(), captured_vars.len() as u32);

        let capture = rt::Command::Capture {
            rec_var: func_expr.rec_var,
            cmds: self.cmds.drain(start..).collect(),
            vars: Vec::from_iter(captured_vars),
        };
        self.cmds.push(capture);
    }

    fn visit_if(&mut self, if_expr: &ir::If) {
        self.visit_expr(&if_expr.cond);

        self.cmds.push(rt::Command::Trap);
        let alt_pos = self.cmds.len();
        self.visit_expr(&if_expr.alt);

        self.cmds.push(rt::Command::Trap);
        let cons_pos = self.cmds.len();
        self.visit_expr(&if_expr.cons);

        self.cmds[alt_pos - 1] = rt::Command::Test {
            jump_offset: cons_pos - alt_pos,
        };
        self.cmds[cons_pos - 1] = rt::Command::Jump {
            jump_offset: self.cmds.len() - cons_pos,
        };
    }

    fn visit_proj(&mut self, proj_expr: &ir::Proj) {
        self.visit_expr(&proj_expr.expr);
        self.cmds.push(rt::Command::Get {
            field: proj_expr.field,
        });
    }

    fn visit_enum(&mut self, enum_expr: &ir::Enum) {
        self.visit_expr(&enum_expr.expr);
        self.cmds.push(rt::Command::WrapEnum { tag: enum_expr.tag });
    }

    fn visit_record(&mut self, record_expr: &SymbolMap<ir::Expr>) {
        self.cmds.push(rt::Command::Push {
            value: rt::Value::Record(Default::default()),
        });
        for (&field, val) in record_expr {
            self.visit_expr(&val);
            self.cmds.push(rt::Command::Set { field });
        }
    }

    fn visit_match(&mut self, match_expr: &ir::Match) {
        self.visit_expr(&match_expr.expr);

        let mut jump_offsets = ImSymbolMap::default();

        self.cmds.push(rt::Command::Trap);
        let cases_pos = self.cmds.len();
        for (&tag, case) in &match_expr.cases {
            self.cmds.push(rt::Command::Trap);
            let case_pos = self.cmds.len();
            if let Some(name) = case.name {
                self.cmds.push(rt::Command::Store { var: name });
            } else {
                self.cmds.push(rt::Command::Pop);
            }
            self.visit_expr(&case.expr);

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

    fn visit_import(&mut self, cmds: &Rc<[rt::Command]>) {
        self.cmds.push(rt::Command::Import { cmds: cmds.clone() });
    }
}

impl GenerateVisitor {
    fn push_scope(&mut self, depth: u32) {
        self.scopes.push(Scope { depth });
    }

    fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn pop_scope(&mut self) -> u32 {
        let scope = self.scopes.pop().unwrap();
        scope.depth
    }
}

impl ir::Visitor for CaptureVisitor {
    fn visit_var(&mut self, var: VarId) {
        if var < self.max_var {
            self.captured_vars.insert(var);
        }
    }
}

impl CaptureVisitor {
    fn get_captures(func_expr: &ir::Func) -> CapturedVars {
        let mut visitor = CaptureVisitor {
            max_var: func_expr.arg,
            captured_vars: SmallOrdSet::new(),
        };
        func_expr.body.visit(&mut visitor);
        visitor.captured_vars
    }
}
