#![allow(unused)]

use small_lang::rt::{FuncValue, Opts, Output};
use small_lang::syntax::{Expr, SourceMap, Spanned};

pub fn parse(input: &'static str) -> (SourceMap, Spanned<Expr>) {
    let mut source = SourceMap::new();
    let (_, expr) = source.parse_input("root", input).unwrap().unwrap_miss();
    (source, expr)
}

pub fn check((source, expr): (SourceMap, Spanned<Expr>)) -> FuncValue {
    small_lang::check::check(source, &expr).unwrap()
}

pub fn run(func: FuncValue) -> Output {
    small_lang::rt::run(func, Opts::default()).unwrap()
}
