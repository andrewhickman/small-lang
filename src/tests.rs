use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;

use crate::rt::Value;
use crate::{run, Args};

fn run_file(file: impl AsRef<Path>, value: bool) -> Result<Vec<Value>, Box<dyn Error>> {
    run(Args {
        input: read_to_string(Path::new("data").join(file).with_extension("sl"))?,
        value: Value::Bool(value),
    })
}

macro_rules! test_file {
    ($file:ident, $arg:expr, Ok($expected:expr)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file), $arg) {
                Ok(actual) => assert_eq!(actual, vec![$expected]),
                Err(err) => panic!("expected success but got error: {}", err),
            }
        }
    };
    ($file:ident, $arg:expr, Err($expected:expr)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file), $arg) {
                Err(actual) => assert_eq!(actual.to_string(), $expected),
                Ok(_) => panic!("expected error but got success"),
            }
        }
    };
}

test_file!(not, true, Ok(Value::Bool(false)));
test_file!(not2, true, Ok(Value::Bool(true)));
test_file!(xor, true, Ok(Value::Bool(true)));
test_file!(shadow, true, Ok(Value::Bool(false)));
test_file!(bad_main_ty, true, Err("invalid main type"));
test_file!(type_error, true, Err("inference error"));