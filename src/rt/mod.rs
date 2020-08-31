mod builtin;
mod state;

use std::fmt;
use std::iter::empty;
use std::rc::Rc;

use im::OrdMap;
use serde::Serialize;

use crate::check::vars::VarId;
use crate::rt::builtin::Builtin;
use crate::rt::state::Runtime;
use crate::syntax::symbol::{ImSymbolMap, Symbol};
use crate::FileSpan;

pub fn run(cmds: &[Command], opts: Opts) -> Result<Output, Error> {
    assert!(!cmds.is_empty());
    let mut ctx = Runtime::new(builtin::builtins(), opts);
    ctx.exec_all(cmds)?;
    Ok(ctx.finish())
}

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "structopt", derive(structopt::StructOpt))]
pub struct Opts {
    #[cfg_attr(feature = "structopt", structopt(long, default_value = "32"))]
    pub max_stack: u64,
    #[cfg_attr(feature = "structopt", structopt(long))]
    pub max_ops: Option<u64>,
}

#[derive(Debug, Serialize)]
pub struct Output {
    pub value: Value,
    pub op_count: u64,
}

impl Default for Opts {
    fn default() -> Self {
        Opts {
            max_stack: 32,
            max_ops: None,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    StackOverflow,
    TooManyOps,
    IntegerOverflow,
}

#[derive(Clone, Serialize)]
#[serde(untagged)]
pub enum Value {
    Null,
    Bool(bool),
    Number(NumberValue),
    String(String),
    Record(OrdMap<Symbol, Value>),
    Enum(EnumValue),
    Func(FuncValue),
    Builtin {
        #[serde(rename = "$builtin")]
        name: Symbol,
        #[serde(skip)]
        builtin: Builtin,
    },
}

#[derive(Clone, Serialize)]
pub struct FuncValue {
    #[serde(rename = "$rec-var", skip_serializing_if = "Option::is_none")]
    pub rec_var: Option<VarId>,
    #[serde(rename = "$ops")]
    pub cmds: Rc<[Command]>,
    #[serde(rename = "$env")]
    pub env: OrdMap<VarId, Value>,
    #[serde(rename = "$span")]
    pub span: FileSpan,
}

#[derive(Clone, Serialize, PartialEq)]
pub struct EnumValue {
    #[serde(rename = "$tag")]
    pub tag: Symbol,
    #[serde(rename = "$value")]
    pub value: Box<Value>,
}

#[derive(Copy, Clone, Serialize)]
#[serde(untagged)]
pub enum NumberValue {
    Int(i64),
    Float(f64),
}

#[derive(Clone, Serialize)]
#[serde(tag = "op", rename_all = "kebab-case")]
#[derive(PartialEq, Eq)]
pub enum Command {
    Pop,
    Push {
        value: Value,
    },
    Capture {
        span: FileSpan,
        rec_var: Option<VarId>,
        cmds: Rc<[Command]>,
        vars: Vec<VarId>,
    },
    Call,
    Become,
    Test {
        jump_offset: usize,
    },
    Match {
        jump_offsets: ImSymbolMap<usize>,
    },
    Jump {
        jump_offset: usize,
    },
    Set {
        field: Symbol,
    },
    Get {
        field: Symbol,
    },
    Load {
        var: VarId,
    },
    Store {
        var: VarId,
    },
    WrapEnum {
        tag: Symbol,
    },
    Import {
        cmds: Rc<[Command]>,
    },
    Trap,
}

enum ControlFlow {
    Continue,
    Jump(usize),
    Restart,
}

impl Value {
    pub fn unwrap_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("expected bool"),
        }
    }

    pub fn unwrap_number(self) -> NumberValue {
        match self {
            Value::Number(n) => n,
            _ => panic!("expected number"),
        }
    }

    pub fn unwrap_string(self) -> String {
        match self {
            Value::String(s) => s,
            _ => panic!("expected string"),
        }
    }

    pub fn unwrap_func(self) -> FuncValue {
        match self {
            Value::Func(f) => f,
            _ => panic!("expected func"),
        }
    }

    pub fn unwrap_record(self) -> OrdMap<Symbol, Value> {
        match self {
            Value::Record(r) => r,
            _ => panic!("expected record"),
        }
    }

    pub fn unwrap_enum_variant(self) -> EnumValue {
        match self {
            Value::Enum(e) => e,
            _ => panic!("expected enum variant"),
        }
    }
}

impl FuncValue {
    // HACK: to avoid making function types self referential, add them to their own environment
    // lazily.
    fn env<'a>(&'a self) -> impl Iterator<Item = (VarId, Value)> + 'a {
        self.env
            .iter()
            .map(|(&id, val)| (id, val.clone()))
            .chain(self.rec_var.map(|var| (var, Value::Func(self.clone()))))
    }
}

impl Command {
    fn exec(&self, ctx: &mut Runtime) -> Result<ControlFlow, Error> {
        log::trace!("exec {:?}", self);

        Ok(match *self {
            Command::Pop => {
                ctx.pop_stack();
                ControlFlow::Continue
            }
            Command::Push { ref value } => {
                ctx.push_stack(value.clone());
                ControlFlow::Continue
            }
            Command::Capture {
                span,
                rec_var,
                ref cmds,
                ref vars,
            } => {
                let env = vars
                    .iter()
                    .filter(|&&var| rec_var != Some(var))
                    .map(|&var| (var, ctx.get_var(var)))
                    .collect();
                ctx.push_stack(Value::Func(FuncValue {
                    span,
                    rec_var,
                    cmds: cmds.clone(),
                    env,
                }));
                ControlFlow::Continue
            }
            Command::Call => match ctx.pop_stack() {
                Value::Func(func) => {
                    ctx.push_scope(func.env())?;
                    ctx.exec_all(&func.cmds)?;
                    ctx.pop_scope();
                    ControlFlow::Continue
                }
                Value::Builtin { builtin, .. } => {
                    builtin.exec(ctx)?;
                    ControlFlow::Continue
                }
                _ => panic!("expected func"),
            },
            Command::Test { jump_offset } => {
                if ctx.pop_stack().unwrap_bool() {
                    ControlFlow::Jump(jump_offset)
                } else {
                    ControlFlow::Continue
                }
            }
            Command::Match { ref jump_offsets } => {
                let variant = ctx.pop_stack().unwrap_enum_variant();
                ctx.push_stack(*variant.value);
                ControlFlow::Jump(jump_offsets[&variant.tag])
            }
            Command::Jump { jump_offset } => ControlFlow::Jump(jump_offset),
            Command::Become => ControlFlow::Restart,
            Command::Set { field } => {
                let val = ctx.pop_stack();
                let mut rec = ctx.pop_stack().unwrap_record();
                assert!(rec.insert(field, val).is_none());
                ctx.push_stack(Value::Record(rec));
                ControlFlow::Continue
            }
            Command::Get { field } => {
                let rec = ctx.pop_stack().unwrap_record();
                let val = rec[&field].clone();
                ctx.push_stack(val);
                ControlFlow::Continue
            }
            Command::Load { var } => {
                let val = ctx.get_var(var);
                ctx.push_stack(val);
                ControlFlow::Continue
            }
            Command::Store { var } => {
                let val = ctx.pop_stack();
                ctx.set_var(var, val);
                ControlFlow::Continue
            }
            Command::WrapEnum { tag } => {
                let val = ctx.pop_stack();
                let variant = Value::Enum(EnumValue {
                    tag,
                    value: Box::new(val),
                });
                ctx.push_stack(variant);
                ControlFlow::Continue
            }
            Command::Import { ref cmds } => {
                ctx.push_scope(empty())?;
                ctx.exec_all(&cmds)?;
                ctx.pop_scope();
                ControlFlow::Continue
            }
            Command::Trap => panic!("invalid instruction"),
        })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::StackOverflow => "stack overflow".fmt(f),
            Error::TooManyOps => "max operations reached".fmt(f),
            Error::IntegerOverflow => "integer overflow".fmt(f),
        }
    }
}

impl std::error::Error for Error {}

fn write_debug_json<T: Serialize>(val: &T, f: &mut fmt::Formatter) -> fmt::Result {
    let s = if f.alternate() {
        serde_json::to_string_pretty(val).unwrap()
    } else {
        serde_json::to_string(val).unwrap()
    };
    if s.len() < 20_000 {
        write!(f, "{}", s)
    } else {
        write!(f, "...")
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_debug_json(self, f)
    }
}

impl fmt::Debug for FuncValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_debug_json(self, f)
    }
}

impl fmt::Debug for EnumValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_debug_json(self, f)
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_debug_json(self, f)
    }
}
