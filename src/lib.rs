pub mod check;
pub mod generate;
pub mod rt;
pub mod syntax;

mod error;
#[cfg(test)]
mod tests;
#[cfg(test)]
mod ui_tests;

pub use crate::error::Error;
pub use crate::syntax::Source;

pub(crate) use crate::error::ErrorData;

use crate::syntax::SourceMap;

pub fn check(root: Source) -> Result<check::ir::Expr, Error> {
    let mut source = SourceMap::new();
    let (file, expr) = match source.parse_root(root) {
        Ok(result) => result.unwrap_miss(),
        Err(err) => return Err(Error::new(source, err)),
    };
    match check::check(&mut source, file, &expr) {
        Ok(expr) => Ok(expr),
        Err(err) => return Err(Error::diagnostics(source, err)),
    }
}

pub fn run(root: Source, opts: rt::Opts) -> Result<rt::Output, Error> {
    let expr = check(root)?;
    let cmds = generate::generate(&expr);
    match rt::run(&cmds, opts) {
        Ok(result) => Ok(result),
        Err(err) => Err(Error::basic(Box::new(err))),
    }
}
