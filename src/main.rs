use std::error::Error;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process;

use structopt::StructOpt;

use small_lang::rt::Value;

#[derive(StructOpt)]
struct Args {
    #[structopt(value_name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

fn run(args: &Args) -> Result<Vec<Value>, Box<dyn Error>> {
    small_lang::run(&read_to_string(&args.file)?)
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
