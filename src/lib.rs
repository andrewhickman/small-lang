pub mod check;
pub mod rt;
pub mod syntax;

mod error;
#[cfg(test)]
mod tests;
#[cfg(test)]
mod ui_tests;

pub use crate::error::Error;

pub(crate) use crate::error::ErrorData;

use std::path::Path;

use crate::check::check;

use crate::syntax::{SourceCacheResult, SourceMap};

pub fn run_input(input: String, opts: rt::Opts) -> Result<rt::Output, Error> {
    run(opts, |source| source.parse_input("root", input))
}

pub fn run_file(path: &Path, opts: rt::Opts) -> Result<rt::Output, Error> {
    run(opts, |source| source.parse_file(path))
}

fn run<F>(opts: rt::Opts, parse: F) -> Result<rt::Output, Error>
where
    F: FnOnce(&mut SourceMap) -> Result<SourceCacheResult, ErrorData>,
{
    let mut source = SourceMap::new();
    let (file, expr) = match parse(&mut source) {
        Ok(result) => result.unwrap_miss(),
        Err(err) => return Err(Error::new(source, err)),
    };
    let func = match check(&mut source, file, &expr) {
        Ok(func) => func,
        Err(err) => return Err(Error::diagnostics(source, err)),
    };
    match rt::run(func, opts) {
        Ok(result) => Ok(result),
        Err(err) => Err(Error::basic(Box::new(err))),
    }
}
