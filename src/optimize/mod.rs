#[cfg(test)]
mod tests;

use crate::check::ir;

pub fn optimize(expr: ir::Expr, _opts: Opts) -> ir::Expr {
    expr
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
