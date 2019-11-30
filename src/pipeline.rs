use std::collections::hash_map::{self, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use codespan::{FileId, Files};

use crate::{Error, ErrorData};

#[derive(Debug)]
pub struct Pipeline<T> {
    files: Files,
    dir: Vec<PathBuf>,
    cache: HashMap<String, Option<T>>,
}

pub enum Source<'a> {
    Input(String),
    File(&'a Path),
}

impl<T> Pipeline<T> {
    pub fn new() -> Self {
        Pipeline {
            files: Files::new(),
            dir: vec![],
            cache: HashMap::new(),
        }
    }
}

impl<T> Pipeline<T>
where
    T: Clone,
{
    pub fn process_root(
        &mut self,
        source: Source<'_>,
        import: impl FnMut(&mut Self, FileId, String) -> Result<T, ErrorData>,
    ) -> Result<T, ErrorData> {
        match source {
            Source::File(path) => self.process_file(path, import),
            Source::Input(input) => self.process_input("root", input, import),
        }
    }

    pub fn process_import(
        &mut self,
        path: &str,
        import: impl FnMut(&mut Self, FileId, String) -> Result<T, ErrorData>,
    ) -> Result<T, ErrorData> {
        match path {
            "cmp" => self.process_input("cmp", include_str!("../std/cmp.sl"), import),
            "iter" => self.process_input("iter", include_str!("../std/iter.sl"), import),
            "math" => self.process_input("math", include_str!("../std/math.sl"), import),
            "list" => self.process_input("list", include_str!("../std/list.sl"), import),
            path => self.process_file(path, import),
        }
    }

    pub fn process_file(
        &mut self,
        path: impl AsRef<Path>,
        import: impl FnMut(&mut Self, FileId, String) -> Result<T, ErrorData>,
    ) -> Result<T, ErrorData> {
        let path = match self.dir.last() {
            Some(dir) => dir.join(path),
            None => path.as_ref().to_owned(),
        };
        match path.parent() {
            Some(dir) => self.dir.push(dir.to_owned()),
            None => return Err(ErrorData::Basic("invalid path".into())),
        }
        let source = fs::read_to_string(&path).map_err(ErrorData::io)?;
        let result = self.add_file(path.to_string_lossy(), source, import);
        self.dir.pop();
        result
    }

    pub fn process_input(
        &mut self,
        name: impl Into<String>,
        input: impl Into<String>,
        import: impl FnMut(&mut Self, FileId, String) -> Result<T, ErrorData>,
    ) -> Result<T, ErrorData> {
        self.add_file(name, input, import)
    }

    fn add_file(
        &mut self,
        name: impl Into<String>,
        source: impl Into<String>,
        mut import: impl FnMut(&mut Self, FileId, String) -> Result<T, ErrorData>,
    ) -> Result<T, ErrorData> {
        let name = name.into();

        match self.cache.entry(name.clone()) {
            hash_map::Entry::Occupied(entry) => {
                return Ok(entry
                    .get()
                    .clone()
                    .ok_or_else(|| ErrorData::Basic("recursive import detected".into()))?)
            }
            hash_map::Entry::Vacant(entry) => entry.insert(None),
        };

        let file = self.files.add(name.clone(), source);
        let result = import(self, file, self.files.source(file).to_owned())?;

        self.cache.insert(name, Some(result.clone()));
        Ok(result)
    }
}

impl<T> Pipeline<Rc<T>> {
    pub fn process_root_rc(
        mut self,
        source: Source<'_>,
        mut import: impl FnMut(&mut Self, FileId, String) -> Result<T, ErrorData>,
    ) -> Result<T, Error> {
        match self.process_root(source, |this, file, input| {
            import(this, file, input).map(Rc::new)
        }) {
            Ok(result) => {
                drop(self);
                Ok(Rc::try_unwrap(result).unwrap_or_else(|_| unreachable!()))
            }
            Err(err) => Err(Error::new(self, err)),
        }
    }
}

impl<T> Default for Pipeline<T> {
    fn default() -> Self {
        Pipeline::new()
    }
}

impl<T> Into<Files> for Pipeline<T> {
    fn into(self) -> Files {
        self.files
    }
}
