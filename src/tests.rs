use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;

use crate::rt::Value;

fn run_file(file: impl AsRef<Path>) -> Result<Vec<Value>, Box<dyn Error>> {
    let input = read_to_string(Path::new("data").join(file).with_extension("sl"))?;
    crate::run(&input)
}

macro_rules! test_file {
    ($file:ident, Ok($expected:expr)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Ok(actual) => assert_eq!(actual, vec![$expected]),
                Err(err) => panic!("expected success but got error: {}", err),
            }
        }
    };
    ($file:ident, Err($expected:expr)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Err(actual) => assert_eq!(actual.to_string(), $expected),
                Ok(_) => panic!("expected error but got success"),
            }
        }
    };
}

test_file!(not, Ok(Value::Bool(false)));
test_file!(not2, Ok(Value::Bool(true)));
test_file!(xor, Ok(Value::Bool(true)));
test_file!(shadow, Ok(Value::Bool(false)));
test_file!(undefined_var, Err("undefined var `x`"));
test_file!(type_error, Err("inference error"));
