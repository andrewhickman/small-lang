#![allow(unused)]

use codespan::FileId;

use small_lang::check::ir;
use small_lang::rt::{Command, Opts, Output};
use small_lang::syntax::{ast, Source};

pub fn parse(input: impl Into<String>) -> (FileId, ast::Spanned<ast::Expr>) {
    (
        dummy_file_id(),
        small_lang::parse(Source::Input(input.into())).unwrap(),
    )
}

pub fn check((file, expr): (FileId, ast::Spanned<ast::Expr>)) -> ir::Expr {
    small_lang::check::check(file, &expr, |_| unreachable!())
        .unwrap()
        .1
}

pub fn generate(expr: ir::Expr) -> Vec<Command> {
    small_lang::generate::generate(&expr)
}

pub fn run(cmds: Vec<Command>) -> Output {
    small_lang::rt::run(&cmds, Opts::default()).unwrap()
}

pub fn dummy_file_id() -> FileId {
    codespan::Files::new().add("", "")
}
