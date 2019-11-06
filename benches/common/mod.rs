#![allow(unused)]

use codespan::FileId;

use small_lang::rt::{FuncValue, Opts, Output};
use small_lang::syntax::{Expr, SourceMap, Spanned};

pub fn parse(input: &'static str) -> (SourceMap, FileId, Spanned<Expr>) {
    let mut source = SourceMap::new();
    let (file, expr) = source.parse_input("root", input).unwrap().unwrap_miss();
    (source, file, expr)
}

pub fn check((mut source, file, expr): (SourceMap, FileId, Spanned<Expr>)) -> FuncValue {
    small_lang::check::check(&mut source, file, &expr).unwrap()
}

pub fn run(func: FuncValue) -> Output {
    small_lang::rt::run(func, Opts::default()).unwrap()
}
