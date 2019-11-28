use crate::generate::generate;
use crate::rt::{Command, NumberValue, Value};
use crate::syntax::Symbol;
use crate::{optimize, Source};

macro_rules! test_case {
    ($name:ident, $cmds:expr) => {
        #[test]
        fn $name() {
            let expr = crate::check(Source::File(
                concat!("codegen/", stringify!($name), ".sl").as_ref(),
            ))
            .unwrap();
            let optimized = optimize::optimize(expr, optimize::Opts { opt_level: 3 });
            assert_eq!(generate(&optimized).as_slice(), $cmds);
        }
    };
}

test_case!(
    inline_iife,
    &[
        Command::Push {
            value: Value::Number(NumberValue::Int(5)),
        },
        Command::Store {
            var: Symbol::new("x"),
        },
        Command::Load {
            var: Symbol::new("x"),
        },
        Command::End,
    ]
);
