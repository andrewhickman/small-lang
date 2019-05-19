pub mod rt;

mod check;
mod syntax;
#[cfg(test)]
mod tests;

use std::error::Error;
use std::str::FromStr;

use crate::check::check;
use crate::rt::{Command, Value};
use crate::syntax::LocExpr;

pub fn run(input: &str) -> Result<Vec<Value>, Box<dyn Error>> {
    let expr = LocExpr::from_str(input)?;
    let func = check(&expr)?;
    let mut ctx = vec![func].into();
    Command::App.exec(&mut ctx);
    Ok(ctx.stack)
}
