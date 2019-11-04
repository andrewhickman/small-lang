pub mod check;
pub mod rt;
pub mod syntax;

#[cfg(test)]
mod tests;

use std::error::Error;
use std::path::Path;

use crate::check::check;

use crate::syntax::{SourceCacheResult, SourceMap};

pub fn run_input(input: String, opts: rt::Opts) -> Result<rt::Output, Box<dyn Error>> {
    run(opts, |source| source.parse_input("root", input))
}

pub fn run_file(path: &Path, opts: rt::Opts) -> Result<rt::Output, Box<dyn Error>> {
    run(opts, |source| source.parse_file(path))
}

fn run<F>(opts: rt::Opts, parse: F) -> Result<rt::Output, Box<dyn Error>>
where
    F: FnOnce(&mut SourceMap) -> Result<SourceCacheResult, Box<dyn Error>>,
{
    let mut source = SourceMap::new();
    let (_, expr) = parse(&mut source)?.unwrap_miss();
    let func = check(source, &expr)?;
    let result = rt::run(func, opts)?;
    Ok(result)
}
