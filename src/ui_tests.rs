use std::fs;
use std::path::Path;

use itertools::zip_eq;

use crate::{optimize, rt, Source};

fn test_output(file: impl AsRef<Path>) {
    let file = file.as_ref();
    let expected = fs::read_to_string(file.with_extension("out")).unwrap();
    let actual = crate::run(
        Source::File(&file.with_extension("sl")),
        optimize::Opts::default(),
        rt::Opts::default(),
    )
    .unwrap_err()
    .to_string();

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
