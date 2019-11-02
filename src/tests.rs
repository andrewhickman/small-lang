use std::error::Error;
use std::path::Path;

use proptest::{prelude::*, proptest};

use crate::check::check;
use crate::rt::{self, EnumValue, Value};
use crate::syntax::tests::arb_expr;
use crate::syntax::{SourceMap, Symbol};

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
test_file!(
    enum_variant,
    Ok(Value::Enum(EnumValue {
        tag: Symbol::new("a"),
        value: Box::new(Value::Bool(true)),
    }))
);
test_file!(match_simple, Ok(Value::Int(1)));
test_file!(match_error, Err("inference error"));
test_file!(match_subtyping, Ok(Value::Bool(true)));
test_file!(null, Ok(Value::Null));
test_file!(null_error, Err("inference error"));
test_file!(enum_null_variant, Ok(Value::Bool(true)));
test_file!(iter_range, Ok(Value::Bool(true)));
test_file!(iter_range_map, Ok(Value::Bool(true)));
test_file!(
    iter_range_find,
    Ok(Value::Enum(EnumValue {
        tag: Symbol::new("some"),
        value: Box::new(Value::Int(4)),
    }))
);

test_file!(pr1, Ok(Func));
test_file!(pr2, Ok(Value::Bool(true)));

proptest! {
    #[test]
    fn typecheck_soundness(func in arb_valid_expr()) {
        rt::run(func, rt::Opts {
            max_stack: 1024,
            max_ops: Some(1_048_576),
        }).ok();
    }
}

fn arb_valid_expr() -> impl Strategy<Value = rt::FuncValue> {
    arb_expr().prop_filter_map("type error", |expr| check(SourceMap::new(), &expr).ok())
}
