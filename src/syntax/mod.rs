pub mod ast;
pub mod symbol;
#[cfg(test)]
pub mod tests;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    parser,
    "/syntax/parser.rs"
);
mod fmt;

pub use symbol::{ImSymbolMap, Symbol, SymbolMap};

pub(crate) use parser::Token;

use std::fmt::Write;

use codespan::{ByteIndex, FileId, RawIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::{lalrpop_mod, ParseError};
use once_cell::sync::Lazy;

use crate::error::ErrorData;
use crate::syntax::parser::SpannedExprParser;

#[derive(Debug)]
#[doc(hidden)]
pub struct Error {
    span: Span,
    message: String,
}

pub fn parse(file: FileId, input: &str) -> Result<ast::Spanned<ast::Expr>, ErrorData> {
    static PARSER: Lazy<SpannedExprParser> = Lazy::new(SpannedExprParser::new);

    PARSER
        .parse(&mut symbol::Interner::write(), input)
        .map_err(|err| {
            let err = err.map_location(|idx| ByteIndex(idx as RawIndex));
            let diag = build_diagnostic(file, err);
            ErrorData::Diagnostics(vec![diag])
        })
}

impl Error {
    fn new(
        start: impl Into<ByteIndex>,
        end: impl Into<ByteIndex>,
        message: impl Into<String>,
    ) -> Self {
        Error {
            span: Span::new(start.into(), end.into()),
            message: message.into(),
        }
    }
}

fn build_diagnostic(
    file: FileId,
    err: ParseError<ByteIndex, Token<'_>, Error>,
) -> Diagnostic<FileId> {
    match err {
        ParseError::InvalidToken { location: start } => Diagnostic::error()
            .with_message("invalid token found")
            .with_labels(vec![
                Label::primary(file, Span::new(start, start)).with_message("invalid token here")
            ]),
        ParseError::UnrecognizedEOF {
            location: end,
            expected,
        } => Diagnostic::error()
            .with_message(format!(
                "expected {}, found end of file",
                fmt_expected(&expected)
            ))
            .with_labels(vec![
                Label::primary(file, Span::new(end, end)).with_message("unexpected EOF here")
            ]),
        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => Diagnostic::error()
            .with_message(format!(
                "expected {}, found `{}`",
                fmt_expected(&expected),
                token
            ))
            .with_labels(vec![
                Label::primary(file, Span::new(start, end)).with_message("unexpected token here")
            ]),
        ParseError::ExtraToken {
            token: (start, token, end),
        } => Diagnostic::error()
            .with_message(format!("extra token found `{}`", token))
            .with_labels(vec![
                Label::primary(file, Span::new(start, end)).with_message("extra token here")
            ]),
        ParseError::User { error } => Diagnostic::error()
            .with_message(error.message)
            .with_labels(vec![Label::primary(file, error.span).with_message("here")]),
    }
}

/// Format a list of expected tokens.
fn fmt_expected(expected: &[String]) -> String {
    let mut s = String::new();
    let len = expected.len();
    if len > 0 {
        if len == 1 {
            write!(s, "{}", expected[0]).unwrap();
        } else {
            write!(s, "one of {}", expected[0]).unwrap();
            for token in &expected[1..(len - 1)] {
                write!(s, ", {}", token).unwrap();
            }
            write!(s, " or {}", expected[len - 1]).unwrap();
        }
    }
    s
}
