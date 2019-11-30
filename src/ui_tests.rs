use std::fs;
use std::path::Path;

use codespan_reporting::term::termcolor::NoColor;
use itertools::zip_eq;

use crate::pipeline::PipelineResult;
use crate::Source;

fn emit<T>(result: PipelineResult<T>) -> String {
    let mut writer = NoColor::new(Vec::new());
    result.emit_warnings(&mut writer).unwrap();
    if let Err(err) = result.into_result() {
        err.emit(&mut writer).unwrap();
    }
    String::from_utf8_lossy(&writer.into_inner()).into_owned()
}

fn test_output(file: impl AsRef<Path>) {
    let file = file.as_ref();
    let expected = fs::read_to_string(file.with_extension("out")).unwrap();
    let actual = emit(crate::check(Source::File(&file.with_extension("sl"))));

    for (expected_line, actual_line) in zip_eq(expected.lines(), actual.lines()) {
        if !expected_line.ends_with("@skip") {
            assert_eq!(expected_line, actual_line);
        }
    }
}

macro_rules! test_case {
    ($name:ident) => {
        #[test]
        fn $name() {
            test_output(concat!("ui/", stringify!($name)))
        }
    };
}

test_case!(enum_mismatch);
test_case!(record_mismatch);
test_case!(multiple_source);
test_case!(nested_error);
test_case!(builtin_source);
test_case!(builtin_primary_source);
test_case!(syntax_error);
test_case!(parse_int_overflow);
test_case!(unused_var);
