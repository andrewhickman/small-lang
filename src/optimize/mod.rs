#[cfg(test)]
pub mod tests;

mod inline_iife;
mod inline_let;

use crate::check::ir::{self};

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
