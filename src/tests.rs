use std::error::Error;
use std::path::{Component, Path, PathBuf};

use crate::{run, Args};

fn run_file(file: impl AsRef<Path>, value: bool) -> Result<(), Box<dyn Error>> {
    run(&Args {
        file: Path::new("data").join(file).with_extension("sl"),
        value,
    })
}

macro_rules! test_file {
    ($file:ident, $arg:expr, Ok($expected:expr)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file), $arg) {
                Ok(actual) => assert_eq!(actual, $expected),
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

test_file!(not, true, Ok(()));
test_file!(not2, true, Ok(()));
test_file!(xor, true, Ok(()));
test_file!(bad_main_ty, true, Err("invalid main type"));
test_file!(type_error, true, Err("inference error"));
