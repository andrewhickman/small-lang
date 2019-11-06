use std::io;
use std::path::PathBuf;
use std::process;

use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::ColorArg;

use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    #[structopt(value_name = "FILE", parse(from_os_str))]
    file: PathBuf,
    #[structopt(
        long = "color",
        parse(try_from_str),
        default_value = "auto",
        possible_values = ColorArg::VARIANTS,
        case_insensitive = true
    )]
    color: ColorArg,
    #[structopt(flatten)]
    rt_opts: small_lang::rt::Opts,
}

fn run(args: &Args) -> Result<(), Box<small_lang::Error>> {
    let output = small_lang::run_file(&args.file, args.rt_opts)?;
    eprintln!("Finished in {} operations", output.op_count);
    serde_json::to_writer_pretty(io::stdout().lock(), &output.value)
        .map_err(|err| small_lang::Error::basic(err.into()))?;
    Ok(())
}

#[paw::main]
fn main(args: Args) {
    #[cfg(feature = "env_logger")]
    env_logger::init();
    if let Err(err) = run(&args) {
        err.emit(&mut StandardStream::stderr(args.color.into()).lock())
            .expect("failed to print to stderr");
        process::exit(1);
    }
}
