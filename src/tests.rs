use std::error::Error;
use std::path::Path;

use proptest::proptest;

use crate::check::check;
use crate::rt::{self, Value};
use crate::syntax::tests::arb_expr;
use crate::syntax::SourceMap;

fn run_file(file: impl AsRef<Path>) -> Result<Value, Box<dyn Error>> {
    let file = Path::new("data").join(file).with_extension("sl");
    crate::run_file(&file, rt::Opts::default())
}

macro_rules! test_file {
    ($file:ident, Ok(Func)) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Ok(actual) => {
                    actual.unwrap_func();
                }
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
test_file!(func_shadow, Ok(Value::Bool(true)));
test_file!(rec_chain, Ok(Func));
test_file!(eq_bool, Ok(Value::Bool(true)));
test_file!(eq_record, Ok(Value::Bool(true)));
test_file!(eq_func, Ok(Value::Bool(true)));
test_file!(eq_incomparable, Ok(Value::Bool(false)));
test_file!(curry, Ok(Value::Bool(true)));
test_file!(int, Ok(Value::Int(-300)));
test_file!(add, Ok(Value::Int(4)));
test_file!(add_error, Err("inference error"));
test_file!(sub, Ok(Value::Int(4)));
test_file!(sub_error, Err("inference error"));
test_file!(fibonacci, Ok(Value::Int(377)));
test_file!(string, Ok(Value::String("hello".to_owned())));
test_file!(
    string_escape,
    Ok(Value::String("hel\\lo \"world\"".to_owned()))
);
test_file!(import, Ok(Value::Bool(true)));

test_file!(pr1, Ok(Func));
test_file!(pr2, Ok(Value::Bool(true)));

proptest! {
    #[test]
    fn typecheck_soundness(expr in arb_expr()) {
        if let Ok(func) = check(SourceMap::new(), &expr) {
            rt::run(func, rt::Opts {
                max_stack: 1024,
                max_ops: Some(1_048_576),
            }).ok();
        }
    }
}
