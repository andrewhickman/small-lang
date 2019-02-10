mod check;
mod rt;
mod syntax;
#[cfg(test)]
mod tests;

use std::error::Error;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process;
use std::str::FromStr;

use structopt::StructOpt;

use crate::check::check;
use crate::rt::{Command, Value};
use crate::syntax::Expr;

#[derive(StructOpt)]
struct Args {
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
    #[structopt(name = "VALUE", parse(try_from_str))]
    value: bool,
}

fn main() {
    if let Err(e) = run(&Args::from_args()) {
        eprintln!("Error: {}.", e);
        process::exit(1);
    }

    println!("ok");
}

fn run(args: &Args) -> Result<Vec<Value>, Box<dyn Error>> {
    let input = read_to_string(&args.file)?;
    let expr = Expr::from_str(&input)?;
    let func = check(&expr)?;
    let mut ctx = vec![Value::Bool(args.value), func].into();
    Command::App.exec(&mut ctx);
    Command::App.exec(&mut ctx);
    println!("{:#?}", ctx);
    Ok(ctx.stack)
}
