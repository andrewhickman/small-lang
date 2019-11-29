use std::{fmt, io};

use codespan::Files;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::term::termcolor::{Color, ColorSpec, NoColor, WriteColor};
use codespan_reporting::term::{emit, Config};

#[derive(Debug)]
pub struct Error {
    files: Files,
    data: ErrorData,
}

#[derive(Debug)]
pub enum ErrorData {
    Basic(Box<dyn std::error::Error>),
    Diagnostics(Vec<Diagnostic>),
}

impl Error {
    pub(crate) fn new(files: impl Into<Files>, data: ErrorData) -> Self {
        Error {
            files: files.into(),
            data,
        }
    }

    pub fn basic(err: impl Into<Box<dyn std::error::Error>>) -> Self {
        Error::new(Files::new(), ErrorData::Basic(err.into()))
    }

    pub fn emit(&self, writer: &mut impl WriteColor) -> io::Result<()> {
        match &self.data {
            ErrorData::Basic(err) => {
                writer.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
                write!(writer, "error")?;
                writer.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_bold(true))?;
                write!(writer, ": {}", err)?;
                writer.reset()?;

                let mut err: &dyn std::error::Error = &**err;
                while let Some(source) = err.source() {
                    writer.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
                    write!(writer, "\ncaused by: ")?;
                    writer.reset()?;
                    write!(writer, "{}", source)?;
                    err = source;
                }
                writeln!(writer)
            }
            ErrorData::Diagnostics(diagnostics) => {
                let config = Config::default();
                for diagnostic in diagnostics {
                    emit(writer, &config, &self.files, diagnostic)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut writer = NoColor::new(Vec::new());
        self.emit(&mut writer).unwrap();
        String::from_utf8_lossy(&writer.into_inner()).fmt(f)
    }
}

impl ErrorData {
    pub(crate) fn io(err: io::Error) -> Self {
        ErrorData::Basic(err.into())
    }
}
