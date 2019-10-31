mod builtin;

use std::fmt;

use serde::Serialize;
use std::rc::Rc;

use crate::rt::builtin::Builtin;
use crate::syntax::symbol::{ImSymbolMap, Symbol};

pub fn run(func: FuncValue, opts: Opts) -> Result<Value, Error> {
    let mut ctx = Runtime {
        stack: vec![Value::Func(func)],
        vars: vec![builtin::builtins()],
        opts,
    };
    Command::Call.exec(&mut ctx)?;
    assert_eq!(ctx.stack.len(), 1);
    Ok(ctx.stack.into_iter().next().unwrap())
}

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "structopt", derive(structopt::StructOpt))]
pub struct Opts {
    #[cfg_attr(feature = "structopt", structopt(long, default_value = "512"))]
    pub max_stack: u64,
    #[cfg_attr(feature = "structopt", structopt(long))]
    pub max_ops: Option<u64>,
}

impl Default for Opts {
    fn default() -> Self {
        Opts {
            max_stack: 512,
            max_ops: None,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Value {
    Bool(bool),
    Int(i64),
    String(String),
    Record(ImSymbolMap<Value>),
    Func(FuncValue),
    Builtin {
        #[serde(rename = "$builtin")]
        name: Symbol,
        #[serde(skip)]
        builtin: Builtin,
    },
}

#[derive(Clone, Debug, Serialize)]
pub struct FuncValue {
    #[serde(rename = "$name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Symbol>,
    #[serde(rename = "$ops")]
    pub cmds: Rc<[Command]>,
    #[serde(rename = "$env")]
    pub env: ImSymbolMap<Value>,
}

#[derive(Debug, Serialize)]
#[serde(tag = "op", content = "value", rename_all = "kebab-case")]
pub enum Command {
    Push(Value),
    Capture(Option<Symbol>, Rc<[Command]>),
    Call,
    Test(usize),
    Jump(usize),
    Set(Symbol),
    Get(Symbol),
    Load(Symbol),
    Store(Symbol),
    End,
}

#[derive(Debug)]
pub enum Error {
    StackOverflow,
    TooManyOps,
    IntegerOverflow,
}

#[derive(Debug)]
struct Runtime {
    stack: Vec<Value>,
    vars: Vec<ImSymbolMap<Value>>,
    opts: Opts,
}

impl Value {
    pub fn unwrap_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("expected bool"),
        }
    }

    pub fn unwrap_int(self) -> i64 {
        match self {
            Value::Int(i) => i,
            _ => panic!("expected int"),
        }
    }

    pub fn unwrap_func(self) -> FuncValue {
        match self {
            Value::Func(f) => f,
            _ => panic!("expected func"),
        }
    }

    pub fn unwrap_record(self) -> ImSymbolMap<Value> {
        match self {
            Value::Record(r) => r,
            _ => panic!("expected record"),
        }
    }
}

impl FuncValue {
    // HACK: to avoid making function types self referential, add them to their own environment
    // lazily.
    fn env(&self) -> ImSymbolMap<Value> {
        let env = self.env.clone();
        if let Some(name) = self.name {
            env.update(name, Value::Func(self.clone()))
        } else {
            env
        }
    }
}

impl Command {
    fn exec(&self, ctx: &mut Runtime) -> Result<Option<usize>, Error> {
        if let Some(ref mut remaining_ops) = ctx.opts.max_ops {
            if *remaining_ops > 1 {
                *remaining_ops -= 1;
            } else {
                return Err(Error::TooManyOps);
            }
        }

        Ok(match *self {
            Command::Push(ref val) => {
                ctx.stack.push(val.clone());
                None
            }
            Command::Capture(name, ref cmds) => {
                let env = ctx.vars().clone();
                ctx.stack.push(Value::Func(FuncValue {
                    name,
                    cmds: cmds.clone(),
                    env,
                }));
                None
            }
            Command::Call => match ctx.stack.pop().unwrap() {
                Value::Func(func) => {
                    ctx.push_vars(func.env())?;
                    let mut idx = 0;
                    while let Some(cmd) = func.cmds.get(idx) {
                        idx += cmd.exec(ctx)?.unwrap_or(0);
                        idx += 1;
                    }
                    ctx.pop_vars();
                    None
                }
                Value::Builtin { builtin, .. } => {
                    builtin.exec(ctx)?;
                    None
                }
                _ => panic!("expected func"),
            },
            Command::Test(offset) => {
                if ctx.stack.pop().unwrap().unwrap_bool() {
                    Some(offset)
                } else {
                    None
                }
            }
            Command::Jump(offset) => Some(offset),
            Command::Set(label) => {
                let val = ctx.stack.pop().unwrap();
                let mut rec = ctx.stack.pop().unwrap().unwrap_record();
                rec.insert(label, val);
                ctx.stack.push(Value::Record(rec));
                None
            }
            Command::Get(label) => {
                let rec = ctx.stack.pop().unwrap().unwrap_record();
                let val = rec[&label].clone();
                ctx.stack.push(val);
                None
            }
            Command::Load(symbol) => {
                let val = ctx.vars()[&symbol].clone();
                ctx.stack.push(val);
                None
            }
            Command::Store(symbol) => {
                let val = ctx.stack.pop().unwrap();
                ctx.push_vars(ImSymbolMap::default().update(symbol, val))?;
                None
            }
            Command::End => {
                ctx.pop_vars();
                None
            }
        })
    }
}

impl Runtime {
    fn push_vars(&mut self, new_vars: ImSymbolMap<Value>) -> Result<(), Error> {
        if self.vars.len() as u64 >= self.opts.max_stack {
            return Err(Error::StackOverflow);
        }
        let all_vars = new_vars.union(self.vars().clone());
        self.vars.push(all_vars);
        Ok(())
    }

    fn vars(&mut self) -> &mut ImSymbolMap<Value> {
        self.vars.last_mut().unwrap()
    }

    fn pop_vars(&mut self) {
        self.vars.pop().unwrap();
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Int(l), Value::Int(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Record(l), Value::Record(r)) => l == r,
            (Value::Func(l), Value::Func(r)) => Rc::ptr_eq(&l.cmds, &r.cmds),
            (Value::Builtin { builtin: l, .. }, Value::Builtin { builtin: r, .. }) => l == r,
            _ => false,
        }
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
