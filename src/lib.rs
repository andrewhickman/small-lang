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

use codespan::{FileId, Span};

use crate::check::scheme::ReducedScheme;
use crate::pipeline::{Pipeline, PipelineResult, ProcessOutput, ProcessResult};
use crate::syntax::ast;

pub type FileSpan = (FileId, Span);

pub fn parse(root: Source) -> PipelineResult<ast::Spanned<ast::Expr>> {
    Pipeline::new().process_root_rc(root, |_, file, input| {
        Ok(ProcessOutput {
            value: syntax::parse(file, &input)?,
            warnings: vec![],
        })
    })
}

pub fn check(root: Source) -> PipelineResult<ReducedScheme> {
    let mut pipeline = Pipeline::new();
    let result = pipeline.process_root(root, check_impl);
    pipeline.finish(result)
}

pub fn generate(root: Source, optimize_opts: optimize::Opts) -> PipelineResult<Rc<[rt::Command]>> {
    let mut pipeline = Pipeline::new();
    let result = pipeline
        .process_root(root, |pipeline, file, input| {
            generate_impl(pipeline, file, input, optimize_opts)
        })
        .map(|(_, cmds)| cmds);
    pipeline.finish(result)
}

pub fn run(
    root: Source,
    optimize_opts: optimize::Opts,
    rt_opts: rt::Opts,
) -> PipelineResult<rt::Output> {
    generate(root, optimize_opts)
        .and_then(|cmds| rt::run(&cmds, rt_opts).map_err(|err| ErrorData::Basic(err.into())))
}

fn check_impl(
    pipeline: &mut Pipeline<ReducedScheme>,
    file: FileId,
    input: String,
) -> ProcessResult<ReducedScheme> {
    let ast = syntax::parse(file, &input)?;
    let (scheme, _, warnings) = check::check(file, &ast, |path| {
        Ok((pipeline.process_import(path, check_impl)?, ()))
    })?;
    Ok(ProcessOutput {
        value: scheme,
        warnings,
    })
}

fn generate_impl(
    pipeline: &mut Pipeline<(ReducedScheme, Rc<[rt::Command]>)>,
    file: FileId,
    input: String,
    optimize_opts: optimize::Opts,
) -> ProcessResult<(ReducedScheme, Rc<[rt::Command]>)> {
    let ast = syntax::parse(file, &input)?;
    let (scheme, mut expr, warnings) = check::check(file, &ast, |path| {
        pipeline.process_import(path, |pipeline, file, input| {
            generate_impl(pipeline, file, input, optimize_opts)
        })
    })?;
    optimize::optimize(&mut expr, optimize_opts);
    let cmds = generate::generate(&expr);
    Ok(ProcessOutput {
        value: (scheme, cmds.into()),
        warnings,
    })
}
