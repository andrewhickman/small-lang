use std::path::{Path, PathBuf};
use std::{fmt, fs};

use codespan::{FileId, Files, Location};
use lalrpop_util::ParseError;

use crate::syntax::{Expr, Spanned};

#[derive(Debug)]
pub struct SourceMap {
    files: Files,
    dir: Vec<PathBuf>,
    root: FileId,
}

#[derive(Debug)]
pub struct SourceLocation(Location);

impl SourceMap {
    #[cfg(test)]
    pub fn empty() -> Self {
        SourceMap::from_source(String::new())
    }

    pub fn new(root: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        let (name, dir) = match (root.file_name(), root.parent()) {
            (Some(name), Some(dir)) => (name.to_string_lossy(), dir.to_owned()),
            _ => return Err("invalid path".into()),
        };
        let source = fs::read_to_string(root)?;

        let mut files = Files::new();
        let root = files.add(name, source);
        Ok(SourceMap {
            files,
            dir: vec![dir],
            root,
        })
    }

    pub fn from_source(source: String) -> Self {
        let mut files = Files::new();
        let root = files.add("root", source);
        SourceMap {
            files,
            dir: vec![],
            root,
        }
    }

    pub fn parse_root(
        &self,
    ) -> Result<Spanned<Expr>, ParseError<SourceLocation, String, &'static str>> {
        Expr::parse(self.files.source(self.root)).map_err(|err| {
            err.map_location(|idx| SourceLocation(self.files.location(self.root, idx).unwrap()))
                .map_token(|tok| tok.to_string())
        })
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0.line, self.0.column)
    }
}
