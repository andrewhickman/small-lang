use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;

use proptest::proptest;

use crate::rt::{Command, Value};
use crate::syntax::tests::arb_expr;
use crate::check::check;

fn run_file(file: impl AsRef<Path>) -> Result<Value, Box<dyn Error>> {
    let input = read_to_string(Path::new("data").join(file).with_extension("sl"))?;
    crate::run(&input)
}

macro_rules! test_file {
    ($file:ident, Ok(Func)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Ok(actual) => { actual.unwrap_func(); },
                Err(err) => panic!("expected success but got error: {}", err),
            }
        }
    };
    ($file:ident, Ok($expected:expr)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Ok(actual) => assert_eq!(actual, $expected),
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
test_file!(expr, Ok(Value::Bool(true)));
test_file!(undefined_var, Err("undefined var `x`"));
test_file!(type_error, Err("inference error"));
test_file!(rec_error, Err("inference error"));
test_file!(rec_func, Ok(Value::Bool(true)));
test_file!(rec_shadow, Ok(Func));
test_file!(pr1, Ok(Func));
test_file!(func_shadow, Ok(Value::Bool(true)));

proptest! {
    #[test]
    fn typecheck_correctness(expr in arb_expr()) {
        if let Ok(func) = check(&expr) {
            let mut ctx = vec![func].into();
            Command::App.exec(&mut ctx);
            assert_eq!(ctx.stack.len(), 1);
        }
    }
}