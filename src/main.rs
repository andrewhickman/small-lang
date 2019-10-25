use std::error::Error;
use std::fs::read_to_string;
use std::io;
use std::path::PathBuf;
use std::process;

use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    #[structopt(value_name = "FILE", parse(from_os_str))]
    file: PathBuf,
    #[structopt(flatten)]
    rt_opts: small_lang::rt::Opts,
}

fn run(args: &Args) -> Result<(), Box<dyn Error>> {
    let value = small_lang::run(&read_to_string(&args.file)?, args.rt_opts)?;
    serde_json::to_writer_pretty(io::stdout().lock(), &value)?;
    Ok(())
}

fn main() {
    if let Err(err) = run(&Args::from_args()) {
        eprintln!("Error: {}.", err);
        process::exit(1);
    }
}
