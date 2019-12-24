use std::rc::Rc;

use codespan::Span;
use im::HashMap;

use crate::check::vars::VarId;
use crate::generate;
use crate::optimize::Opts;
use crate::pipeline::Source;
use crate::rt::{Command, NumberValue, Value};
use crate::syntax::tests::make_file_id;
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
test_case!(
    tail_recursion,
    &[
        Command::Capture {
            span: (make_file_id(0), Span::new(14, 130)),
            rec_var: Some(VarId::new(0)),
            cmds: Rc::new([
                Command::Store { var: VarId::new(1) },
                Command::Load { var: VarId::new(1) },
                Command::Match {
                    jump_offsets: HashMap::default()
                        .update(Symbol::new("none"), 1)
                        .update(Symbol::new("some"), 4),
                },
                Command::Jump { jump_offset: 12 },
                Command::Store { var: VarId::new(3) },
                Command::Push {
                    value: Value::Bool(false),
                },
                Command::Jump { jump_offset: 9 },
                Command::Store { var: VarId::new(3) },
                Command::Load { var: VarId::new(3) },
                Command::Get {
                    field: Symbol::new("value"),
                },
                Command::Test { jump_offset: 4 },
                Command::Load { var: VarId::new(3) },
                Command::Get {
                    field: Symbol::new("tail"),
                },
                Command::Become,
                Command::Jump { jump_offset: 1 },
                Command::Push {
                    value: Value::Bool(true),
                },
            ]),
            vars: vec![]
        },
        Command::Store { var: VarId::new(0) },
        Command::Load { var: VarId::new(0) },
    ]
);
