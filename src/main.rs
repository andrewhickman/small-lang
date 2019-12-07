use std::io;
use std::path::PathBuf;
use std::process;

use codespan_reporting::term::termcolor::{StandardStream, WriteColor};
use codespan_reporting::term::ColorArg;
use small_lang::Source;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Args {
    #[structopt(
        long = "color",
        parse(try_from_str),
        default_value = "auto",
        possible_values = ColorArg::VARIANTS,
        case_insensitive = true
    )]
    color: ColorArg,
    #[structopt(subcommand)]
    command: Command,
}

#[derive(StructOpt)]
enum Command {
    Check {
        #[structopt(value_name = "FILE", parse(from_os_str))]
        file: PathBuf,
    },
    Generate {
        #[structopt(value_name = "FILE", parse(from_os_str))]
        file: PathBuf,
        #[structopt(flatten)]
        optimize_opts: small_lang::optimize::Opts,
    },
    Run {
        #[structopt(value_name = "FILE", parse(from_os_str))]
        file: PathBuf,
        #[structopt(flatten)]
        rt_opts: small_lang::rt::Opts,
        #[structopt(flatten)]
        optimize_opts: small_lang::optimize::Opts,
    },
}

fn run(args: &Args, stderr: &mut impl WriteColor) -> Result<(), small_lang::Error> {
    match args.command {
        Command::Check { ref file } => {
            let result = small_lang::check(Source::File(file));
            result.emit_warnings(stderr)?;
            let scheme_graph = result.into_result()?.to_graph();

            serde_json::to_writer_pretty(io::stdout().lock(), &scheme_graph)
                .map_err(small_lang::Error::basic)?;
            println!();
            Ok(())
        }
        Command::Generate {
            ref file,
            optimize_opts,
        } => {
            let result = small_lang::generate(Source::File(file), optimize_opts);
            result.emit_warnings(stderr)?;
            let commands = result.into_result()?;

            serde_json::to_writer_pretty(io::stdout().lock(), &commands)
                .map_err(small_lang::Error::basic)?;
            println!();
            Ok(())
        }
        Command::Run {
            ref file,
            rt_opts,
            optimize_opts,
        } => {
            let result = small_lang::run(Source::File(file), optimize_opts, rt_opts);
            result.emit_warnings(stderr)?;

            let output = result.into_result()?;
            eprintln!("Finished in {} operations", output.op_count);
            serde_json::to_writer_pretty(io::stdout().lock(), &output.value)
                .map_err(small_lang::Error::basic)?;
            println!();
            Ok(())
        }
    }
}

#[paw::main]
fn main(args: Args) {
    #[cfg(feature = "env_logger")]
    env_logger::init();
    let stderr = StandardStream::stderr(args.color.into());
    let mut stderr = stderr.lock();
    if let Err(err) = run(&args, &mut stderr) {
        err.emit(&mut stderr).expect("failed to print to stderr");
        process::exit(1);
    }
}
