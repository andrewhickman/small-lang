pub mod ast;
pub mod symbol;
#[cfg(test)]
pub mod tests;

lalrpop_mod!(
    #[allow(clippy::all)]
    parser,
    "/syntax/parser.rs"
);
mod fmt;
mod source;

pub use source::{Source, SourceCacheResult, SourceMap};
pub use symbol::{ImSymbolMap, Symbol, SymbolMap};

pub(crate) use parser::Token;

use codespan::{ByteIndex, RawIndex, Span};
use lalrpop_util::{lalrpop_mod, ParseError};
use once_cell::sync::Lazy;

use parser::SpannedExprParser;

#[derive(Debug)]
#[doc(hidden)]
pub struct Error {
    span: Span,
    message: String,
}

impl ast::Expr {
    pub(crate) fn parse<'a>(
        input: &'a str,
    ) -> Result<ast::Spanned<ast::Expr>, ParseError<ByteIndex, Token<'a>, Error>> {
        static PARSER: Lazy<SpannedExprParser> = Lazy::new(SpannedExprParser::new);

        PARSER
            .parse(&mut symbol::Interner::write(), input)
            .map_err(|err| err.map_location(|idx| ByteIndex(idx as RawIndex)))
    }
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
