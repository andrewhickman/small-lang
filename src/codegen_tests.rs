use crate::generate;
use crate::optimize::Opts;
use crate::pipeline::Source;
use crate::rt::{Command, NumberValue, Value};

macro_rules! test_case {
    ($name:ident, $cmds:expr) => {
        #[test]
        fn $name() {
            assert_eq!(
                generate(
                    Source::File(concat!("codegen/", stringify!($name), ".sl").as_ref(),),
                    Opts { opt_level: 3 }
                )
                .into_result()
                .unwrap()
                .as_ref(),
                $cmds
            );
        }
    };
}

test_case!(
    inline_iife,
    &[Command::Push {
        value: Value::Number(NumberValue::Int(5)),
    },]
);
test_case!(
    not2,
    &[
        Command::Push {
            value: Value::Bool(true),
        },
        Command::Test { jump_offset: 2 },
        Command::Push {
            value: Value::Bool(true),
        },
        Command::Jump { jump_offset: 1 },
        Command::Push {
            value: Value::Bool(false),
        },
        Command::Test { jump_offset: 2 },
        Command::Push {
            value: Value::Bool(true),
        },
        Command::Jump { jump_offset: 1 },
        Command::Push {
            value: Value::Bool(false),
        },
    ]
);
