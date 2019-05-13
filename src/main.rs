use std::fs::read_to_string;
use std::process;
use std::error::Error;
use std::path::PathBuf;

use structopt::StructOpt;

use small_lang::rt::Value;

#[derive(StructOpt)]
struct Args {
    #[structopt(value_name = "FILE", parse(from_os_str))]
    file: PathBuf,
    #[structopt(value_name = "VALUE", parse(try_from_str))]
    value: bool,
}

fn run(args: &Args) -> Result<Vec<Value>, Box<dyn Error>> {
    small_lang::run(small_lang::Args {
        input: read_to_string(&args.file)?,
        value: Value::Bool(args.value),
    })
}

fn main() {
    match run(&Args::from_args()) {
        Ok(stack) => println!("{:#?}", stack),
        Err(err) => {
            eprintln!("Error: {}.", err);
            process::exit(1);
        }
    }
}
