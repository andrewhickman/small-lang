use std::collections::hash_map::{self, HashMap};
use std::path::{Path, PathBuf};
use std::{fmt, fs};

use codespan::{FileId, Files, Location};

use crate::syntax::{Expr, Spanned};

#[derive(Debug)]
pub struct SourceMap {
    files: Files,
    dir: Vec<PathBuf>,
    cache: HashMap<String, FileId>,
}

#[derive(Debug)]
pub struct SourceLocation(Location);

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

    pub fn parse_file(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<SourceCacheResult, Box<dyn std::error::Error>> {
        let path = match self.dir.last() {
            Some(dir) => dir.join(path),
            None => path.as_ref().to_owned(),
        };
        match path.parent() {
            Some(dir) => self.dir.push(dir.to_owned()),
            None => return Err("invalid path".into()),
        }
        let source = fs::read_to_string(&path)?;
        self.add_file(path.canonicalize()?.to_string_lossy(), source)
    }

    pub fn parse_source(
        &mut self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Result<SourceCacheResult, Box<dyn std::error::Error>> {
        self.dir.push(PathBuf::default());
        self.add_file(name, source)
    }

    pub fn add_file(
        &mut self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Result<SourceCacheResult, Box<dyn std::error::Error>> {
        let name = name.into();

        match self.cache.entry(name.clone()) {
            hash_map::Entry::Occupied(entry) => return Ok(SourceCacheResult::Hit(*entry.get())),
            hash_map::Entry::Vacant(entry) => {
                let file = self.files.add(name, source);

                let files = &mut self.files;
                let expr = Expr::parse(files.source(file)).map_err(|err| {
                    err.map_location(|idx| SourceLocation(files.location(file, idx).unwrap()))
                        .map_token(|tok| tok.to_string())
                })?;

                entry.insert(file);
                Ok(SourceCacheResult::Miss(file, expr))
            }
        }
    }

    pub fn end_file(&mut self) {
        self.dir.pop();
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

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0.line.number(), self.0.column.number())
    }
}
