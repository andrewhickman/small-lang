use serde::{Deserialize, Serialize};
use std::rc::Rc;

use crate::syntax::symbol::{Symbol, SymbolMap};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Value {
    Null,
    Bool(bool),
    Record(SymbolMap<Value>),
    Func(FuncValue),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct FuncValue {
    #[serde(rename = "$name", skip_serializing_if = "Option::is_none")]
    pub name: Option<Symbol>,
    #[serde(rename = "$ops")]
    pub cmds: Rc<[Command]>,
    #[serde(rename = "$env")]
    pub env: SymbolMap<Value>,
}

#[derive(Eq, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "op", content = "value", rename_all = "kebab-case")]
pub enum Command {
    Push(Value),
    Capture(Option<Symbol>, Rc<[Command]>),
    App,
    Test(usize),
    Jump(usize),
    Set(Symbol),
    Get(Symbol),
    Load(Symbol),
    Store(Symbol),
    End,
}

#[derive(Debug)]
pub struct Context {
    pub stack: Vec<Value>,
    pub vars: Vec<SymbolMap<Value>>,
}

impl Value {
    pub fn unwrap_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("expected bool"),
        }
    }

    pub fn unwrap_func(self) -> FuncValue {
        match self {
            Value::Func(f) => f,
            _ => panic!("expected func"),
        }
    }

    pub fn unwrap_record(self) -> SymbolMap<Value> {
        match self {
            Value::Record(r) => r,
            _ => panic!("expected record"),
        }
    }
}

impl FuncValue {
    fn env(&self) -> SymbolMap<Value> {
        let env = self.env.clone();
        if let Some(name) = self.name {
            env.update(name, Value::Func(self.clone()))
        } else {
            env
        }
    }
}

impl Command {
    pub fn exec(&self, ctx: &mut Context) -> Option<usize> {
        match *self {
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
            Command::App => {
                let func = ctx.stack.pop().unwrap().unwrap_func();
                let env = func.env().union(ctx.vars().clone());
                ctx.vars.push(env);
                let mut idx = 0;
                while let Some(cmd) = func.cmds.get(idx) {
                    idx += cmd.exec(ctx).unwrap_or(0);
                    idx += 1;
                }
                ctx.vars.pop();
                None
            }
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
                let vars = ctx.vars().update(symbol, val);
                ctx.vars.push(vars);
                None
            }
            Command::End => {
                ctx.vars.pop().unwrap();
                None
            }
        }
    }
}

impl Context {
    fn vars(&mut self) -> &mut SymbolMap<Value> {
        self.vars.last_mut().unwrap()
    }
}

impl From<Vec<Value>> for Context {
    fn from(stack: Vec<Value>) -> Self {
        Context {
            stack,
            vars: vec![SymbolMap::default()],
        }
    }
}
