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
}

test_file!(not, true, Ok(()));
test_file!(not2, true, Ok(()));
test_file!(xor, true, Ok(()));
