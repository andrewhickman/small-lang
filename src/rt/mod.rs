mod builtin;
mod state;

use std::fmt;
use std::rc::Rc;

use im::OrdMap;
use serde::Serialize;

use crate::rt::builtin::Builtin;
use crate::rt::state::Runtime;
use crate::syntax::symbol::{ImSymbolMap, Symbol};

pub fn run(cmds: &[Command], opts: Opts) -> Result<Output, Error> {
    assert!(!cmds.is_empty());
    let mut ctx = Runtime::new(builtin::builtins(), opts);
    ctx.exec_all(cmds)?;
    Ok(ctx.finish())
}

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "structopt", derive(structopt::StructOpt))]
pub struct Opts {
    #[cfg_attr(feature = "structopt", structopt(long, default_value = "512"))]
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
            max_stack: 512,
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
    #[serde(rename = "$name", skip_serializing_if = "Option::is_none")]
    pub rec_name: Option<Symbol>,
    #[serde(rename = "$ops")]
    pub cmds: Rc<[Command]>,
    #[serde(rename = "$env")]
    pub env: OrdMap<Symbol, Value>,
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

impl FuncValue {
    pub fn new(cmds: impl Into<Rc<[Command]>>) -> Self {
        FuncValue {
            rec_name: None,
            cmds: cmds.into(),
            env: OrdMap::default(),
        }
    }
}

#[derive(Clone, Serialize)]
#[serde(tag = "op", rename_all = "kebab-case")]
pub enum Command {
    Pop,
    Push {
        value: Value,
    },
    Capture {
        rec_name: Option<Symbol>,
        cmds: Rc<[Command]>,
    },
    Call,
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
        var: Symbol,
    },
    Store {
        var: Symbol,
    },
    WrapEnum {
        tag: Symbol,
    },
    Import {
        cmds: Rc<[Command]>,
    },
    End,
    Trap,
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
    fn env(&self) -> OrdMap<Symbol, Value> {
        let env = self.env.clone();
        if let Some(name) = self.rec_name {
            env.update(name, Value::Func(self.clone()))
        } else {
            env
        }
    }
}

impl Command {
    fn exec(&self, ctx: &mut Runtime) -> Result<Option<usize>, Error> {
        log::trace!("exec {:?}", self);

        Ok(match *self {
            Command::Pop => {
                ctx.pop_stack();
                None
            }
            Command::Push { ref value } => {
                ctx.push_stack(value.clone());
                None
            }
            Command::Capture { rec_name, ref cmds } => {
                let env = ctx.vars().clone();
                ctx.push_stack(Value::Func(FuncValue {
                    rec_name,
                    cmds: cmds.clone(),
                    env,
                }));
                None
            }
            Command::Call => match ctx.pop_stack() {
                Value::Func(func) => {
                    ctx.push_vars(func.env())?;
                    ctx.exec_all(&func.cmds)?;
                    ctx.pop_vars();
                    None
                }
                Value::Builtin { builtin, .. } => {
                    builtin.exec(ctx)?;
                    None
                }
                _ => panic!("expected func"),
            },
            Command::Test { jump_offset } => {
                if ctx.pop_stack().unwrap_bool() {
                    Some(jump_offset)
                } else {
                    None
                }
            }
            Command::Match { ref jump_offsets } => {
                let variant = ctx.pop_stack().unwrap_enum_variant();
                ctx.push_stack(*variant.value);
                Some(jump_offsets[&variant.tag])
            }
            Command::Jump { jump_offset } => Some(jump_offset),
            Command::Set { field } => {
                let val = ctx.pop_stack();
                let mut rec = ctx.pop_stack().unwrap_record();
                assert!(rec.insert(field, val).is_none());
                ctx.push_stack(Value::Record(rec));
                None
            }
            Command::Get { field } => {
                let rec = ctx.pop_stack().unwrap_record();
                let val = rec[&field].clone();
                ctx.push_stack(val);
                None
            }
            Command::Load { var } => {
                let val = ctx.get_var(var);
                ctx.push_stack(val);
                None
            }
            Command::Store { var } => {
                let val = ctx.pop_stack();
                ctx.push_vars(im::ordmap![var => val])?;
                None
            }
            Command::WrapEnum { tag } => {
                let val = ctx.pop_stack();
                let variant = Value::Enum(EnumValue {
                    tag,
                    value: Box::new(val),
                });
                ctx.push_stack(variant);
                None
            }
            Command::Import { ref cmds } => {
                ctx.exec_all(&cmds)?;
                None
            }
            Command::End => {
                ctx.pop_vars();
                None
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
    if s.len() < 256000 {
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
