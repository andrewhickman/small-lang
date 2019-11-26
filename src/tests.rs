use std::path::Path;

use proptest::proptest;

use crate::check::check;
use crate::generate::generate;
use crate::optimize;
use crate::rt::{self, EnumValue, NumberValue, Value};
use crate::syntax::tests::{arb_expr, dummy_file_id};
use crate::syntax::{Source, SourceMap, Symbol};
use crate::Error;

fn run_file(file: impl AsRef<Path>) -> Result<Value, Error> {
    let file = Path::new("data").join(file).with_extension("sl");
    crate::run(
        Source::File(file.as_ref()),
        optimize::Opts::default(),
        rt::Opts {
            max_stack: 512,
            max_ops: Some(100_000),
        },
    )
    .map(|output| output.value)
}

macro_rules! test_file {
    ($file:ident, Ok) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Ok(_) => (),
                Err(err) => panic!("expected success but got error: {}", err),
            }
        }
    };
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
    ($file:ident, Err) => {
        #[test]
        fn $file() {
            match run_file(stringify!($file)) {
                Err(_) => (),
                Ok(_) => panic!("expected error but got success"),
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
test_file!(undefined_var, Err);
test_file!(type_error, Err);
test_file!(rec_error, Err);
test_file!(rec_func, Ok(Value::Bool(true)));
test_file!(rec_shadow, Ok(Func));
test_file!(func_shadow, Ok(Value::Bool(true)));
test_file!(rec_chain, Ok(Func));
test_file!(eq_bool, Ok(Value::Bool(true)));
test_file!(eq_record, Ok(Value::Bool(true)));
test_file!(eq_func, Ok(Value::Bool(true)));
test_file!(eq_incomparable, Ok(Value::Bool(false)));
test_file!(curry, Ok(Value::Bool(true)));
test_file!(int, Ok(Value::Number(NumberValue::Int(-300))));
test_file!(add, Ok(Value::Number(NumberValue::Int(4))));
test_file!(add_error, Err);
test_file!(sub, Ok(Value::Number(NumberValue::Int(4))));
test_file!(sub_error, Err);
test_file!(fibonacci, Ok(Value::Number(NumberValue::Int(377))));
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
test_file!(match_simple, Ok(Value::Number(NumberValue::Int(1))));
test_file!(match_error, Err);
test_file!(match_subtyping, Ok(Value::Bool(true)));
test_file!(null, Ok(Value::Null));
test_file!(null_error, Err);
test_file!(enum_null_variant, Ok(Value::Bool(true)));
test_file!(iter_range, Ok(Value::Bool(true)));
test_file!(iter_range_map, Ok(Value::Bool(true)));
test_file!(
    iter_range_find,
    Ok(Value::Enum(EnumValue {
        tag: Symbol::new("some"),
        value: Box::new(Value::Number(NumberValue::Int(4))),
    }))
);
test_file!(recursive_import_a, Err);
test_file!(recursive_import_b, Err);
test_file!(list_from_iter, Ok(Value::Bool(true)));
test_file!(list_from_iter_take, Ok(Value::Bool(true)));
test_file!(list_length, Ok(Value::Number(NumberValue::Int(3))));
test_file!(
    list_from_iter_length,
    Ok(Value::Number(NumberValue::Int(6)))
);
test_file!(iter_length, Ok(Value::Number(NumberValue::Int(42 - 24))));
test_file!(polymorphism, Ok);
test_file!(builtin_error, Err);
test_file!(flow_error, Err);
test_file!(match_val_error, Err);
test_file!(eq_func_env, Ok(Value::Bool(false)));
test_file!(sugar_curry, Ok(Value::Number(NumberValue::Int(10))));
test_file!(
    sugar_curry_order,
    Ok(Value::Record(im::ordmap![
            Symbol::new("a") => Value::Number(NumberValue::Int(1)),
            Symbol::new("b") => Value::Number(NumberValue::Int(2)),
            Symbol::new("c") => Value::Number(NumberValue::Int(3)),
            Symbol::new("d") => Value::Number(NumberValue::Int(4))
    ]))
);
test_file!(float, Ok(Value::Number(NumberValue::Float(0.0002124))));
test_file!(add_coerce, Ok(Value::Number(NumberValue::Float(3.5))));
test_file!(add_float, Ok(Value::Number(NumberValue::Float(3.5))));
test_file!(sub_coerce, Ok(Value::Number(NumberValue::Float(0.5))));
test_file!(sub_float, Ok(Value::Number(NumberValue::Float(-197.5))));
test_file!(add_string, Ok(Value::String("Hello, world".to_owned())));
test_file!(add_string_2, Ok(Value::String("Hello, world!".to_owned())));
test_file!(add_string_error, Err);
test_file!(var_in_module, Err);
test_file!(slow, Ok(Value::Number(NumberValue::Int(0))));
test_file!(invalid_token, Err);

test_file!(pr1, Ok(Func));
test_file!(pr2, Ok(Value::Bool(true)));
test_file!(pr3, Err);

proptest! {
    #[test]
    fn typecheck_soundness(expr in arb_expr()) {
        if let Ok(expr) = check(&mut SourceMap::new(), dummy_file_id(), &expr) {
            let cmds = generate(&expr);
            rt::run(&cmds, rt::Opts {
                max_stack: 1024,
                max_ops: Some(1_048_576),
            }).ok();
        }
    }
}
