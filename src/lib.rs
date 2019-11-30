pub mod check;
pub mod generate;
pub mod optimize;
pub mod pipeline;
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
pub use crate::pipeline::Source;

pub(crate) use crate::error::ErrorData;

use std::rc::Rc;

use codespan::FileId;

use crate::check::scheme::ReducedScheme;
use crate::pipeline::Pipeline;
use crate::syntax::ast;

pub fn parse(root: Source) -> Result<ast::Spanned<ast::Expr>, Error> {
    Pipeline::new().process_root_rc(root, |_, file, input| syntax::parse(file, &input))
}

pub fn check(root: Source) -> Result<ReducedScheme, Error> {
    let mut pipeline = Pipeline::new();
    pipeline
        .process_root(root, check_impl)
        .map_err(|err| Error::new(pipeline, err))
}

pub fn generate(root: Source, optimize_opts: optimize::Opts) -> Result<Rc<[rt::Command]>, Error> {
    let mut pipeline = Pipeline::new();
    let (_, cmds) = pipeline
        .process_root(root, |pipeline, file, input| {
            run_impl(pipeline, file, input, optimize_opts)
        })
        .map_err(|err| Error::new(pipeline, err))?;
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
    pipeline: &mut Pipeline<ReducedScheme>,
    file: FileId,
    input: String,
) -> Result<ReducedScheme, ErrorData> {
    let ast = syntax::parse(file, &input)?;
    let (scheme, _) = check::check(file, &ast, |path| {
        Ok((pipeline.process_import(path, check_impl)?, ()))
    })?;
    Ok(scheme)
}

fn run_impl(
    pipeline: &mut Pipeline<(ReducedScheme, Rc<[rt::Command]>)>,
    file: FileId,
    input: String,
    optimize_opts: optimize::Opts,
) -> Result<(ReducedScheme, Rc<[rt::Command]>), ErrorData> {
    let ast = syntax::parse(file, &input)?;
    let (scheme, expr) = check::check(file, &ast, |path| {
        pipeline.process_import(path, |pipeline, file, input| {
            run_impl(pipeline, file, input, optimize_opts)
        })
    })?;
    let expr = optimize::optimize(expr, optimize_opts);
    let cmds = generate::generate(&expr);
    Ok((scheme, cmds.into()))
}
