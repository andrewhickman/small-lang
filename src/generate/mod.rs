use std::iter::FromIterator;
use std::rc::Rc;

use small_ord_set::SmallOrdSet;

use crate::check::ir;
use crate::check::vars::VarId;
use crate::rt;
use crate::syntax::{ImSymbolMap, SymbolMap};

pub fn generate(expr: &ir::Expr) -> Vec<rt::Command> {
    let mut ctx = Context::new();
    ctx.generate_expr(expr);
    ctx.cmds
}

struct Context {
    cmds: Vec<rt::Command>,
    captures: Vec<(VarId, SmallOrdSet<[VarId; 8]>)>,
}

impl Context {
    fn new() -> Self {
        Context {
            cmds: Vec::with_capacity(16),
            captures: Vec::new(),
        }
    }

    fn generate_expr(&mut self, expr: &ir::Expr) {
        match expr {
            ir::Expr::Literal(value) => self.generate_literal(value.clone()),
            ir::Expr::Var(var) => self.generate_var(*var),
            ir::Expr::Call(call_expr) => self.generate_call(&*call_expr),
            ir::Expr::Let(let_expr) => self.generate_let(&*let_expr),
            ir::Expr::Func(func_expr) => self.generate_func(&*func_expr),
            ir::Expr::If(if_expr) => self.generate_if(&*if_expr),
            ir::Expr::Proj(proj_expr) => self.generate_proj(&*proj_expr),
            ir::Expr::Enum(enum_expr) => self.generate_enum(&*enum_expr),
            ir::Expr::Record(record_expr) => self.generate_record(record_expr),
            ir::Expr::Match(match_expr) => self.generate_match(&*match_expr),
            ir::Expr::Import(import_expr) => self.generate_import(import_expr.clone()),
        }
    }

    fn generate_literal(&mut self, value: rt::Value) {
        self.cmds.push(rt::Command::Push { value })
    }

    fn generate_var(&mut self, var: VarId) {
        self.captures
            .iter_mut()
            .rev()
            .take_while(|&&mut (start, _)| var < start)
            .for_each(|(_, captures)| {
                captures.insert(var);
            });

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
    }

    fn generate_func(&mut self, func_expr: &ir::Func) {
        let start = self.cmds.len();
        // NOTE: this relies on the id `func_expr.arg` being greater than all
        // variables in enclosing scopes.
        self.captures.push((func_expr.arg, SmallOrdSet::new()));

        self.cmds.push(rt::Command::Store { var: func_expr.arg });
        self.generate_expr(&func_expr.body);

        let (_, vars) = self.captures.pop().unwrap();
        let capture = rt::Command::Capture {
            rec_var: func_expr.rec_var,
            cmds: self.cmds.drain(start..).collect(),
            vars: Vec::from_iter(vars),
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

    fn generate_record(&mut self, record_expr: &SymbolMap<ir::Expr>) {
        self.cmds.push(rt::Command::Push {
            value: rt::Value::Record(Default::default()),
        });
        for (&field, val) in record_expr {
            self.generate_expr(&val);
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
            } else {
                self.cmds.push(rt::Command::Pop);
            }
            self.generate_expr(&case.expr);

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

    fn generate_import(&mut self, cmds: Rc<[rt::Command]>) {
        self.cmds.push(rt::Command::Import { cmds });
    }
}
