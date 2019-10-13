pub mod rt;

mod check;
mod syntax;
#[cfg(test)]
mod tests;

use std::error::Error;
use std::str::FromStr;

use crate::check::check;
use crate::rt::{Command, Value};
use crate::syntax::Expr;

pub fn run(input: &str) -> Result<Value, Box<dyn Error>> {
    let expr = Expr::from_str(input)?;
    let func = check(&expr)?;
    let mut ctx = vec![func].into();
    Command::App.exec(&mut ctx);
    assert_eq!(ctx.stack.len(), 1);
    Ok(ctx.stack.into_iter().next().unwrap())
}
