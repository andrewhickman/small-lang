use std::collections::HashMap;
use std::rc::Rc;

use crate::check::ir;
use crate::rt;
use crate::syntax::{ImSymbolMap, Symbol, SymbolMap};

pub fn generate(expr: &ir::Expr) -> Vec<rt::Command> {
    let mut ctx = Context::new();
    ctx.generate_expr(expr);
    ctx.cmds
}

struct Context {
    cmds: Vec<rt::Command>,
    cache: HashMap<*const ir::Expr, Rc<[rt::Command]>>,
}

impl Context {
    fn new() -> Self {
        Context {
            cmds: Vec::with_capacity(16),
            cache: HashMap::with_capacity(8),
        }
    }

    fn generate_expr(&mut self, expr: &ir::Expr) {
        match &expr.kind {
            ir::ExprKind::Literal(value) => self.generate_literal(value.clone()),
            ir::ExprKind::Var(var) => self.generate_var(*var),
            ir::ExprKind::Call(call_expr) => self.generate_call(&*call_expr),
            ir::ExprKind::Let(let_expr) => self.generate_let(&*let_expr),
            ir::ExprKind::Func(func_expr) => self.generate_func(&*func_expr),
            ir::ExprKind::If(if_expr) => self.generate_if(&*if_expr),
            ir::ExprKind::Proj(proj_expr) => self.generate_proj(&*proj_expr),
            ir::ExprKind::Enum(enum_expr) => self.generate_enum(&*enum_expr),
            ir::ExprKind::Record(record_expr) => self.generate_record(record_expr),
            ir::ExprKind::Match(match_expr) => self.generate_match(&*match_expr),
            ir::ExprKind::Import(import_expr) => self.generate_import(import_expr),
        }
    }

    fn generate_literal(&mut self, value: rt::Value) {
        self.cmds.push(rt::Command::Push { value })
    }

    fn generate_var(&mut self, var: Symbol) {
        self.cmds.push(rt::Command::Load { var })
    }

    fn generate_call(&mut self, call_expr: &ir::Call) {
        self.generate_expr(&call_expr.arg);
        self.generate_expr(&call_expr.func);
        self.cmds.push(rt::Command::Call);
    }

    fn generate_let(&mut self, let_expr: &ir::Let) {
        self.generate_expr(&let_expr.val);
        self.cmds.push(rt::Command::Store { var: let_expr.name });
        self.generate_expr(&let_expr.body);
        self.cmds.push(rt::Command::End);
    }

    fn generate_func(&mut self, func_expr: &ir::Func) {
        let start = self.cmds.len();

        self.cmds.push(rt::Command::Store { var: func_expr.arg });
        self.generate_expr(&func_expr.body);
        self.cmds.push(rt::Command::End);

        let capture = rt::Command::Capture {
            rec_name: func_expr.rec_name,
            cmds: self.cmds.drain(start..).collect(),
        };
        self.cmds.push(capture);
    }

    fn generate_if(&mut self, if_expr: &ir::If) {
        self.generate_expr(&if_expr.cond);

        self.cmds.push(rt::Command::Trap);
        let alt_pos = self.cmds.len();
        self.generate_expr(&if_expr.alt);

        self.cmds.push(rt::Command::Trap);
        let cons_pos = self.cmds.len();
        self.generate_expr(&if_expr.cons);

        self.cmds[alt_pos - 1] = rt::Command::Test {
            jump_offset: cons_pos - alt_pos,
        };
        self.cmds[cons_pos - 1] = rt::Command::Jump {
            jump_offset: self.cmds.len() - cons_pos,
        };
    }

    fn generate_proj(&mut self, proj_expr: &ir::Proj) {
        self.generate_expr(&proj_expr.expr);
        self.cmds.push(rt::Command::Get {
            field: proj_expr.field,
        });
    }

    fn generate_enum(&mut self, enum_expr: &ir::Enum) {
        self.generate_expr(&enum_expr.expr);
        self.cmds.push(rt::Command::WrapEnum { tag: enum_expr.tag });
    }

    fn generate_record(&mut self, record_expr: &SymbolMap<ir::RecordEntry>) {
        self.cmds.push(rt::Command::Push {
            value: rt::Value::Record(Default::default()),
        });
        for (&field, entry) in record_expr {
            self.generate_expr(&entry.expr);
            self.cmds.push(rt::Command::Set { field });
        }
    }

    fn generate_match(&mut self, match_expr: &ir::Match) {
        self.generate_expr(&match_expr.expr);

        let mut jump_offsets = ImSymbolMap::default();

        self.cmds.push(rt::Command::Trap);
        let cases_pos = self.cmds.len();
        for (&tag, case) in &match_expr.cases {
            self.cmds.push(rt::Command::Trap);
            let case_pos = self.cmds.len();
            if let Some(name) = case.name {
                self.cmds.push(rt::Command::Store { var: name });
                self.generate_expr(&case.expr);
                self.cmds.push(rt::Command::End);
            } else {
                self.cmds.push(rt::Command::Pop);
                self.generate_expr(&case.expr);
            }

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

    fn generate_import(&mut self, import_expr: &Rc<ir::Expr>) {
        let key = &**import_expr as *const ir::Expr;
        if !self.cache.contains_key(&key) {
            let start = self.cmds.len();
            self.generate_expr(import_expr);
            self.cache.insert(key, self.cmds.drain(start..).collect());
        }
        self.cmds.push(rt::Command::Import {
            cmds: self.cache[&key].clone(),
        });
    }
}