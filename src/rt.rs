use std::rc::Rc;

use crate::syntax::symbol::{Symbol, SymbolMap};

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Func(Rc<[Command]>),
    Record(SymbolMap<Value>),
}

#[derive(Debug)]
pub enum Command {
    Push(Value),
    Pop,
    App,
    Test(usize),
    Get(Symbol),
    Begin,
    Load(Symbol),
    Store(Symbol),
    End,
}

#[derive(Debug)]
pub struct Context {
    stack: Vec<Value>,
    vars: Vec<SymbolMap<Value>>,
}

impl Value {
    fn unwrap_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("expected bool"),
        }
    }

    fn unwrap_func(self) -> Rc<[Command]> {
        match self {
            Value::Func(f) => f,
            _ => panic!("expected func"),
        }
    }

    fn unwrap_record(self) -> SymbolMap<Value> {
        match self {
            Value::Record(r) => r,
            _ => panic!("expected record"),
        }
    }
}

impl Command {
    pub fn exec(&self, ctx: &mut Context) -> usize {
        match *self {
            Command::Push(ref val) => {
                ctx.stack.push(val.clone());
                0
            }
            Command::Pop => {
                ctx.stack.pop().unwrap();
                0
            }
            Command::App => {
                let func = ctx.stack.pop().unwrap().unwrap_func();
                let mut idx = 0;
                while let Some(cmd) = func.get(idx) {
                    idx += 1 + cmd.exec(ctx)
                }
                0
            }
            Command::Test(offset) => {
                if ctx.stack.pop().unwrap().unwrap_bool() {
                    offset
                } else {
                    0
                }
            }
            Command::Get(label) => {
                let rec = ctx.stack.pop().unwrap().unwrap_record();
                let val = rec[&label].clone();
                ctx.stack.push(val);
                0
            }
            Command::Begin => {
                let vars = ctx.vars().clone();
                ctx.vars.push(vars);
                0
            }
            Command::Load(symbol) => {
                let val = ctx.vars()[&symbol].clone();
                ctx.stack.push(val);
                0
            }
            Command::Store(symbol) => {
                let val = ctx.stack.pop().unwrap();
                ctx.vars().insert(symbol, val);
                0
            }
            Command::End => {
                ctx.vars.pop().unwrap();
                0
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
            vars: Vec::new(),
        }
    }
}
