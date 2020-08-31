use std::collections::hash_map::{self, HashMap};
use std::ffi::OsString;
use std::fmt::Debug;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::{emit, Config};

use crate::{Error, ErrorData};

#[derive(Debug)]
pub struct Pipeline<T> {
    files: Files<Rc<str>>,
    dir: Vec<PathBuf>,
    cache: HashMap<OsString, Option<T>>,
    warnings: Vec<Diagnostic<FileId>>,
}

pub struct PipelineResult<U> {
    files: Files<Rc<str>>,
    warnings: Vec<Diagnostic<FileId>>,
    result: Result<U, ErrorData>,
}

pub struct ProcessOutput<T> {
    pub value: T,
    pub warnings: Vec<Diagnostic<FileId>>,
}

pub type ProcessResult<T> = Result<ProcessOutput<T>, ErrorData>;

pub enum Source<'a> {
    Input(Rc<str>),
    File(&'a Path),
}

impl<T> Pipeline<T> {
    pub fn new() -> Self {
        Pipeline {
            files: Files::new(),
            dir: vec![],
            cache: HashMap::new(),
            warnings: vec![],
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
        import: impl FnMut(&mut Self, FileId, Rc<str>) -> ProcessResult<T>,
    ) -> Result<T, ErrorData> {
        match source {
            Source::File(path) => self.process_file(path, import),
            Source::Input(input) => self.process_input("root", input, import),
        }
    }

    pub fn process_import(
        &mut self,
        path: &str,
        import: impl FnMut(&mut Self, FileId, Rc<str>) -> ProcessResult<T>,
    ) -> Result<T, ErrorData> {
        match path {
            "cmp" => self.process_input("cmp", Rc::from(include_str!("../std/cmp.sl")), import),
            "iter" => self.process_input("iter", Rc::from(include_str!("../std/iter.sl")), import),
            "math" => self.process_input("math", Rc::from(include_str!("../std/math.sl")), import),
            "list" => self.process_input("list", Rc::from(include_str!("../std/list.sl")), import),
            path => self.process_file(path, import),
        }
    }

    pub fn process_file(
        &mut self,
        path: impl AsRef<Path>,
        import: impl FnMut(&mut Self, FileId, Rc<str>) -> ProcessResult<T>,
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
        let result = self.add_file(path, source.into(), import);
        self.dir.pop();
        result
    }

    pub fn process_input(
        &mut self,
        name: impl Into<OsString>,
        input: Rc<str>,
        import: impl FnMut(&mut Self, FileId, Rc<str>) -> ProcessResult<T>,
    ) -> Result<T, ErrorData> {
        self.add_file(name, input, import)
    }

    fn add_file(
        &mut self,
        name: impl Into<OsString>,
        source: Rc<str>,
        mut import: impl FnMut(&mut Self, FileId, Rc<str>) -> ProcessResult<T>,
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
        let output = import(self, file, self.files.source(file).clone())?;

        self.cache.insert(name, Some(output.value.clone()));
        self.warnings.extend(output.warnings);
        Ok(output.value)
    }

    pub fn finish<U>(self, result: Result<U, ErrorData>) -> PipelineResult<U> {
        PipelineResult {
            files: self.files,
            warnings: self.warnings,
            result,
        }
    }
}

impl<T: Debug> Pipeline<Rc<T>> {
    pub fn process_root_rc(
        mut self,
        source: Source<'_>,
        mut import: impl FnMut(&mut Self, FileId, Rc<str>) -> ProcessResult<T>,
    ) -> PipelineResult<T> {
        let result = self.process_root(source, |this, file, input| {
            import(this, file, input).map(|output| ProcessOutput {
                value: Rc::new(output.value),
                warnings: output.warnings,
            })
        });
        let result = self.finish(result);
        result.map(|value| Rc::try_unwrap(value).unwrap())
    }
}

impl<T> Default for Pipeline<T> {
    fn default() -> Self {
        Pipeline::new()
    }
}

impl<U> PipelineResult<U> {
    pub fn emit_warnings(&self, sink: &mut impl WriteColor) -> Result<(), Error> {
        for warning in &self.warnings {
            emit(sink, &Config::default(), &self.files, warning).map_err(Error::basic)?;
        }
        Ok(())
    }

    pub fn into_result(self) -> Result<U, Error> {
        match self.result {
            Ok(value) => Ok(value),
            Err(err) => Err(Error::new(self.files, err)),
        }
    }

    pub fn map<V, F>(self, f: F) -> PipelineResult<V>
    where
        F: FnOnce(U) -> V,
    {
        self.and_then(|value| Ok(f(value)))
    }

    pub fn and_then<V, F>(self, f: F) -> PipelineResult<V>
    where
        F: FnOnce(U) -> Result<V, ErrorData>,
    {
        PipelineResult {
            files: self.files,
            warnings: self.warnings,
            result: self.result.and_then(f),
        }
    }
}
