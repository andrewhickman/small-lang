pub mod rt;

mod check;
mod syntax;
#[cfg(test)]
mod tests;

use std::error::Error;
use std::path::Path;

use codespan::FileId;

use crate::check::check;
use crate::syntax::{Expr, SourceMap, Spanned};

pub fn run_input(input: String, opts: rt::Opts) -> Result<rt::Value, Box<dyn Error>> {
    run(opts, |source| source.parse_source(input))
}

pub fn run_file(path: &Path, opts: rt::Opts) -> Result<rt::Value, Box<dyn Error>> {
    run(opts, |source| source.parse_file(path))
}

fn run<F>(opts: rt::Opts, parse: F) -> Result<rt::Value, Box<dyn Error>>
where
    F: FnOnce(&mut SourceMap) -> Result<(FileId, Spanned<Expr>), Box<dyn Error>>,
{
    let mut source = SourceMap::new();
    let (_, expr) = parse(&mut source)?;
    let func = check(source, &expr)?;
    let result = rt::run(func, opts)?;
    Ok(result)
}
