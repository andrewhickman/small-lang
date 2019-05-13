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

pub struct Args {
    pub input: String,
    pub value: Value,
}

pub fn run(args: Args) -> Result<Vec<Value>, Box<dyn Error>> {
    let expr = LocExpr::from_str(&args.input)?;
    let func = check(&expr)?;
    let mut ctx = vec![args.value, func].into();
    Command::App.exec(&mut ctx);
    Command::App.exec(&mut ctx);
    Ok(ctx.stack)
}
