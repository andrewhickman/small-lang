pub mod check;
pub mod generate;
pub mod optimize;
pub mod rt;
pub mod syntax;

#[cfg(test)]
mod codegen_tests;
mod error;
#[cfg(test)]
mod tests;
#[cfg(test)]
mod ui_tests;

pub use crate::error::Error;
pub use crate::syntax::Source;

pub(crate) use crate::error::ErrorData;

use std::rc::Rc;

use codespan::FileId;

use crate::check::scheme::ReducedScheme;
use crate::syntax::{ast, SourceMap};

pub fn parse(root: Source) -> Result<ast::Spanned<ast::Expr>, Error> {
    SourceMap::new().parse_root_rc(root, |_, file, input| syntax::parse(file, &input))
}

pub fn check(root: Source) -> Result<ReducedScheme, Error> {
    let mut source_map = SourceMap::new();
    source_map
        .parse_root(root, check_impl)
        .map_err(|err| Error::new(source_map, err))
}

pub fn generate(root: Source, optimize_opts: optimize::Opts) -> Result<Rc<[rt::Command]>, Error> {
    let mut source_map = SourceMap::new();
    let (_, cmds) = source_map
        .parse_root(root, |source_map, file, input| {
            run_impl(source_map, file, input, optimize_opts)
        })
        .map_err(|err| Error::new(source_map, err))?;
    Ok(cmds)
}

pub fn run(
    root: Source,
    optimize_opts: optimize::Opts,
    rt_opts: rt::Opts,
) -> Result<rt::Output, Error> {
    let cmds = generate(root, optimize_opts)?;
    rt::run(&cmds, rt_opts).map_err(Error::basic)
}

fn check_impl(
    source_map: &mut SourceMap<ReducedScheme>,
    file: FileId,
    input: String,
) -> Result<ReducedScheme, ErrorData> {
    let ast = syntax::parse(file, &input)?;
    let (scheme, _) = check::check(file, &ast, |path| {
        Ok((source_map.parse_import(path, check_impl)?, ()))
    })?;
    Ok(scheme)
}

fn run_impl(
    source_map: &mut SourceMap<(ReducedScheme, Rc<[rt::Command]>)>,
    file: FileId,
    input: String,
    optimize_opts: optimize::Opts,
) -> Result<(ReducedScheme, Rc<[rt::Command]>), ErrorData> {
    let ast = syntax::parse(file, &input)?;
    let (scheme, expr) = check::check(file, &ast, |path| {
        source_map.parse_import(path, |source_map, file, input| {
            run_impl(source_map, file, input, optimize_opts)
        })
    })?;
    let expr = optimize::optimize(expr, optimize_opts);
    let cmds = generate::generate(&expr);
    Ok((scheme, cmds.into()))
}
