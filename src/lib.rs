pub mod rt;

mod check;
mod syntax;
#[cfg(test)]
mod tests;

use std::error::Error;
use std::str::FromStr;

use crate::check::check;
use crate::syntax::Expr;

pub fn run(input: &str, opts: rt::Opts) -> Result<rt::Value, Box<dyn Error>> {
    let expr = Expr::from_str(input)?;
    let func = check(&expr)?;
    let result = rt::run(func, opts)?;
    Ok(result)
}
