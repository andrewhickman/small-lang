pub mod rt;

mod check;
mod syntax;
#[cfg(test)]
mod tests;

use std::error::Error;
use std::path::Path;

use crate::check::check;
use crate::syntax::SourceMap;

pub fn run_source(input: String, opts: rt::Opts) -> Result<rt::Value, Box<dyn Error>> {
    run(SourceMap::from_source(input), opts)
}

pub fn run_file(path: &Path, opts: rt::Opts) -> Result<rt::Value, Box<dyn Error>> {
    run(SourceMap::new(path)?, opts)
}

fn run(source: SourceMap, opts: rt::Opts) -> Result<rt::Value, Box<dyn Error>> {
    let func = check(source)?;
    let result = rt::run(func, opts)?;
    Ok(result)
}
