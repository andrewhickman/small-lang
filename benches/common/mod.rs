#![allow(unused)]

use codespan::FileId;

use small_lang::check::ir;
use small_lang::rt::{Command, Opts, Output};
use small_lang::syntax::{ast, SourceMap};

pub fn parse(input: &'static str) -> (SourceMap, FileId, ast::Spanned<ast::Expr>) {
    let mut source = SourceMap::new();
    let (file, expr) = source.parse_input("root", input).unwrap().unwrap_miss();
    (source, file, expr)
}

pub fn check((mut source, file, expr): (SourceMap, FileId, ast::Spanned<ast::Expr>)) -> ir::Expr {
    small_lang::check::check(&mut source, file, &expr).unwrap()
}

pub fn generate(expr: ir::Expr) -> Vec<Command> {
    small_lang::generate::generate(&expr)
}

pub fn run(cmds: Vec<Command>) -> Output {
    small_lang::rt::run(&cmds, Opts::default()).unwrap()
}
