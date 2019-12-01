mod inline_iife;
#[cfg(test)]
mod tests;

use crate::check::ir;

pub fn optimize(mut expr: ir::Expr, opts: Opts) -> ir::Expr {
    let pass = Pass {
        transforms: vec![inline_iife::INSTANCE],
    };

    let mut budget = match opts.opt_level {
        0 => 0,
        1 => 100,
        2 => 200,
        _ => 400,
    };

    while budget != 0 {
        expr = pass.transform(expr, &mut budget);
    }
    expr
}

trait Transform {
    fn transform(&self, expr: ir::Expr, budget: &mut u32) -> ir::Expr;
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
    fn transform(&self, mut expr: ir::Expr, budget: &mut u32) -> ir::Expr {
        for transform in &self.transforms {
            if *budget == 0 {
                return expr;
            }
            expr = transform.transform(expr, budget);
        }
        expr
    }
}

impl ir::Expr {
    fn is_func(&self) -> bool {
        match self {
            ir::Expr::Func(_) => true,
            _ => false,
        }
    }

    fn map<F>(self, transform: &mut F) -> Self
    where
        F: FnMut(ir::Expr) -> ir::Expr,
    {
        let expr = match self {
            ir::Expr::Literal(_) | ir::Expr::Var(_) | ir::Expr::Import(_) => self,
            ir::Expr::Func(func_expr) => ir::Expr::Func(Box::new(ir::Func {
                arg: func_expr.arg,
                rec_var: func_expr.rec_var,
                body: func_expr.body.map(transform),
                captured_vars: func_expr.captured_vars,
            })),
            ir::Expr::Call(call_expr) => ir::Expr::Call(Box::new(ir::Call {
                arg: call_expr.arg.map(transform),
                func: call_expr.func.map(transform),
            })),
            ir::Expr::Let(let_expr) => ir::Expr::Let(Box::new(ir::Let {
                name: let_expr.name,
                val: let_expr.val.map(transform),
                body: let_expr.body.map(transform),
            })),
            ir::Expr::If(if_expr) => ir::Expr::If(Box::new(ir::If {
                cond: if_expr.cond.map(transform),
                cons: if_expr.cons.map(transform),
                alt: if_expr.alt.map(transform),
            })),
            ir::Expr::Proj(proj_expr) => ir::Expr::Proj(Box::new(ir::Proj {
                expr: proj_expr.expr.map(transform),
                field: proj_expr.field,
            })),
            ir::Expr::Record(record_expr) => ir::Expr::Record(
                record_expr
                    .into_iter()
                    .map(|(field, expr)| (field, expr.map(transform)))
                    .collect(),
            ),
            ir::Expr::Enum(enum_expr) => ir::Expr::Enum(Box::new(ir::Enum {
                tag: enum_expr.tag,
                expr: enum_expr.expr.map(transform),
            })),
            ir::Expr::Match(match_expr) => ir::Expr::Match(Box::new(ir::Match {
                expr: match_expr.expr.map(transform),
                cases: match_expr
                    .cases
                    .into_iter()
                    .map(|(tag, val)| {
                        (
                            tag,
                            ir::MatchCase {
                                expr: val.expr.map(transform),
                                name: val.name,
                            },
                        )
                    })
                    .collect(),
            })),
        };
        transform(expr)
    }
}
