#[cfg(test)]
pub mod tests;

mod inline_iife;
mod inline_let;

use std::collections::HashMap;

use crate::check::ir;
use crate::check::vars::VarId;

pub fn optimize(expr: &mut ir::Expr, opts: Opts) {
    let pass = Pass {
        transforms: vec![inline_iife::INSTANCE, inline_let::INSTANCE],
    };

    let mut budget: u32 = match opts.opt_level {
        0 => 0,
        1 => 100,
        2 => 200,
        _ => 400,
    };

    while budget != 0 {
        let cost = pass.transform(expr);
        budget = budget.saturating_sub(cost);
    }
}

trait Transform {
    #[must_use]
    fn transform(&self, nodes: &mut ir::Expr) -> u32;
}

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "structopt", derive(structopt::StructOpt))]
pub struct Opts {
    #[cfg_attr(
        feature = "structopt",
        structopt(long, short = "O", default_value = "0")
    )]
    pub opt_level: u32,
}

impl Default for Opts {
    fn default() -> Self {
        Opts { opt_level: 0 }
    }
}

struct Pass {
    transforms: Vec<&'static dyn Transform>,
}

impl Transform for Pass {
    fn transform(&self, expr: &mut ir::Expr) -> u32 {
        self.transforms
            .iter()
            .map(|transform| transform.transform(expr))
            .sum()
    }
}

impl ir::Nodes {
    fn clone_node_with(
        &mut self,
        id: ir::NodeId,
        var_map: &mut HashMap<VarId, VarId>,
    ) -> ir::NodeId {
        let src = self.deref_id(id);
        let dst = self.next();
        if self[src].is_decl() {
            var_map.insert(src, dst);
        }

        let cloned = match &self[src] {
            &ir::Node::Literal(ref val) => {
                let cloned = val.clone();
                ir::Node::Literal(cloned)
            }
            &ir::Node::Import(ref import) => {
                let cloned = import.clone();
                ir::Node::Import(cloned)
            }
            &ir::Node::Var(var) => {
                let cloned = *var_map.get(&var).unwrap_or(&var);
                ir::Node::Var(cloned)
            }
            &ir::Node::Call(call_expr) => {
                let cloned = ir::Call {
                    arg: self.clone_node_with(call_expr.arg, var_map),
                    func: self.clone_node_with(call_expr.func, var_map),
                };
                ir::Node::Call(cloned)
            }
            &ir::Node::Let(let_expr) => {
                let cloned = ir::Let {
                    val: self.clone_node_with(let_expr.val, var_map),
                    body: self.clone_node_with(let_expr.body, var_map),
                };
                ir::Node::Let(cloned)
            }
            &ir::Node::Func(func_expr) => {
                let cloned_rec_var = func_expr
                    .rec_var
                    .map(|var| *var_map.get(&var).unwrap_or(&var));
                let cloned = ir::Func {
                    body: self.clone_node_with(func_expr.body, var_map),
                    rec_var: cloned_rec_var,
                    span: func_expr.span,
                };
                ir::Node::Func(cloned)
            }
            &ir::Node::If(if_expr) => {
                let cloned = ir::If {
                    cond: self.clone_node_with(if_expr.cond, var_map),
                    cons: self.clone_node_with(if_expr.cons, var_map),
                    alt: self.clone_node_with(if_expr.alt, var_map),
                };
                ir::Node::If(cloned)
            }
            &ir::Node::Proj(proj_expr) => {
                let cloned = ir::Proj {
                    expr: self.clone_node_with(proj_expr.expr, var_map),
                    field: proj_expr.field,
                };
                ir::Node::Proj(cloned)
            }
            &ir::Node::Enum(enum_expr) => {
                let cloned = ir::Enum {
                    expr: self.clone_node_with(enum_expr.expr, var_map),
                    tag: enum_expr.tag,
                };
                ir::Node::Enum(cloned)
            }
            &ir::Node::Record(ref fields) => {
                let mut cloned = fields.clone();
                for expr in cloned.values_mut() {
                    *expr = self.clone_node_with(*expr, var_map)
                }
                ir::Node::Record(cloned)
            }
            &ir::Node::Match(ref match_expr) => {
                let mut cloned = match_expr.clone();
                cloned.expr = self.clone_node_with(cloned.expr, var_map);

                for case in cloned.cases.values_mut() {
                    *case = self.clone_node_with(*case, var_map);
                }
                ir::Node::Match(cloned)
            }
            &ir::Node::Ref(_) => unreachable!(),
        };

        self.add_at(dst, cloned)
    }
}
