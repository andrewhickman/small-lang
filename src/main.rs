mod check;
mod rt;
mod syntax;

use std::error::Error;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process;
use std::str::FromStr;

use structopt::StructOpt;

use crate::check::check;
use crate::syntax::Expr;

#[derive(StructOpt)]
struct Args {
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
    #[structopt(name = "VALUE", parse(try_from_str))]
    value: bool,
}

fn main() {
    if let Err(e) = run() {
        println!("Error: {}.", e);
        process::exit(1);
    }

    println!("ok");
}

fn run() -> Result<(), Box<dyn Error>> {
    let args = Args::from_args();
    let input = read_to_string(&args.file)?;
    let expr = Expr::from_str(&input)?;
    let func = check(&expr).map_err(|()| "type error")?;
    // let mut stack = vec![Value::Bool(args.value), func].into();
    // Command::App.exec(&mut stack);
    // println!("{:#?}", stack);
    Ok(())
}
