use std::collections::hash_map::{self, HashMap};
use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};

use codespan::{ByteIndex, FileId, Files, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;

use crate::syntax::{Expr, Spanned, Token};
use crate::ErrorData;

#[derive(Debug)]
pub struct SourceMap {
    files: Files,
    dir: Vec<PathBuf>,
    cache: HashMap<String, FileId>,
}

pub enum SourceCacheResult {
    Miss(FileId, Spanned<Expr>),
    Hit(FileId),
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: Files::new(),
            dir: vec![],
            cache: HashMap::new(),
        }
    }

    pub fn files(&self) -> &Files {
        &self.files
    }

    pub fn parse_file(&mut self, path: impl AsRef<Path>) -> Result<SourceCacheResult, ErrorData> {
        let path = match self.dir.last() {
            Some(dir) => dir.join(path),
            None => path.as_ref().to_owned(),
        };
        match path.parent() {
            Some(dir) => self.dir.push(dir.to_owned()),
            None => return Err(ErrorData::Basic("invalid path".into())),
        }
        let source = fs::read_to_string(&path).map_err(ErrorData::io)?;
        self.add_file(path.to_string_lossy(), source)
    }

    pub fn parse_input(
        &mut self,
        name: impl Into<String>,
        input: impl Into<String>,
    ) -> Result<SourceCacheResult, ErrorData> {
        self.dir.push(PathBuf::default());
        self.add_file(name, input)
    }

    fn add_file(
        &mut self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Result<SourceCacheResult, ErrorData> {
        let name = name.into();

        match self.cache.entry(name.clone()) {
            hash_map::Entry::Occupied(entry) => Ok(SourceCacheResult::Hit(*entry.get())),
            hash_map::Entry::Vacant(entry) => {
                let file = self.files.add(name, source);

                let expr = match Expr::parse(self.files.source(file)) {
                    Ok(expr) => expr,
                    Err(err) => {
                        return Err(ErrorData::Diagnostics(vec![
                            self.build_diagnostic(file, err)
                        ]))
                    }
                };

                entry.insert(file);
                Ok(SourceCacheResult::Miss(file, expr))
            }
        }
    }

    pub fn end_file(&mut self) {
        self.dir.pop();
    }

    fn build_diagnostic(
        &self,
        file: FileId,
        err: ParseError<ByteIndex, Token<'_>, &'static str>,
    ) -> Diagnostic {
        match err {
            ParseError::InvalidToken { location: start } => Diagnostic::new_error(
                "invalid token found",
                Label::new(file, Span::new(start, start), "invalid token here"),
            ),
            ParseError::UnrecognizedEOF {
                location: end,
                expected,
            } => Diagnostic::new_error(
                format!("expected {}, found end of file", fmt_expected(&expected)),
                Label::new(file, Span::new(end, end), "unexpected EOF here"),
            ),
            ParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Diagnostic::new_error(
                format!("expected {}, found `{}`", fmt_expected(&expected), token),
                Label::new(file, Span::new(start, end), "unexpected token here"),
            ),
            ParseError::ExtraToken {
                token: (start, token, end),
            } => Diagnostic::new_error(
                format!("extra token found `{}`", token),
                Label::new(file, Span::new(start, end), "extra token here"),
            ),
            ParseError::User { .. } => unreachable!(),
        }
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        SourceMap::new()
    }
}

impl SourceCacheResult {
    pub fn unwrap_miss(self) -> (FileId, Spanned<Expr>) {
        match self {
            SourceCacheResult::Miss(file, expr) => (file, expr),
            _ => panic!("expected source cache miss"),
        }
    }
}

/// Format a list of expected tokens.
fn fmt_expected(expected: &[String]) -> String {
    let mut s = String::new();
    let len = expected.len();
    if len > 0 {
        if len == 1 {
            write!(s, "{}", fmt_token(&expected[0])).unwrap();
        } else {
            write!(s, "one of {}", fmt_token(&expected[0])).unwrap();
            for token in &expected[1..(len - 1)] {
                write!(s, ", {}", fmt_token(token)).unwrap();
            }
            write!(s, " or {}", fmt_token(&expected[len - 1])).unwrap();
        }
    }
    s
}

fn fmt_token(token: &str) -> &str {
    match token {
        // ಠ_ಠ
        r##"r#"\"(?:[^\\\\\"]|(?:\\\\\")|(?:\\\\\\\\))*\""#"## => "<string>",
        r##"r#"[[:alpha:]_]+"#"## => "<identifier>",
        r##"r#"-?\\d+"#"## => "<integer>",
        _ => token,
    }
}
