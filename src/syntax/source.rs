use std::path::{Path, PathBuf};
use std::{fmt, fs};

use codespan::{FileId, Files, Location};

use crate::syntax::{Expr, Spanned};

#[derive(Debug)]
pub struct SourceMap {
    files: Files,
    dir: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct SourceLocation(Location);

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: Files::new(),
            dir: vec![],
        }
    }

    pub fn parse_file(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<(FileId, Spanned<Expr>), Box<dyn std::error::Error>> {
        let path = match self.dir.last() {
            Some(dir) => dir.join(path),
            None => path.as_ref().to_owned(),
        };
        match path.parent() {
            Some(dir) => self.dir.push(dir.to_owned()),
            None => return Err("invalid path".into()),
        }
        let source = fs::read_to_string(&path)?;
        self.add_file(path.to_string_lossy(), source)
    }

    pub fn parse_source(
        &mut self,
        source: String,
    ) -> Result<(FileId, Spanned<Expr>), Box<dyn std::error::Error>> {
        self.add_file("root", source)
    }

    fn add_file(
        &mut self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Result<(FileId, Spanned<Expr>), Box<dyn std::error::Error>> {
        let file = self.files.add(name, source);
        let expr = Expr::parse(self.files.source(file)).map_err(|err| {
            err.map_location(|idx| SourceLocation(self.files.location(file, idx).unwrap()))
                .map_token(|tok| tok.to_string())
        })?;
        Ok((file, expr))
    }

    pub fn end_file(&mut self) {
        self.dir.pop();
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0.line.number(), self.0.column.number())
    }
}
