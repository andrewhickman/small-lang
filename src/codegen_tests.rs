use crate::generate;
use crate::optimize::Opts;
use crate::pipeline::Source;
use crate::rt::{Command, NumberValue, Value};
use crate::syntax::Symbol;

macro_rules! test_case {
    ($name:ident, $cmds:expr) => {
        #[test]
        fn $name() {
            assert_eq!(
                generate(
                    Source::File(concat!("codegen/", stringify!($name), ".sl").as_ref(),),
                    Opts { opt_level: 3 }
                )
                .unwrap()
                .as_ref(),
                $cmds
            );
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
