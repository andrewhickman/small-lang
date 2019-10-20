use std::fmt;

use crate::rt::{Context, Value};
use crate::syntax::{Symbol, SymbolMap};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Builtin(fn(Value) -> Value);

impl Builtin {
    pub fn exec(&self, ctx: &mut Context) {
        let arg = ctx.stack.pop().unwrap();
        let ret = (self.0)(arg);
        ctx.stack.push(ret);
    }
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<builtin>")
    }
}

impl Value {
    fn builtin(name: &str, f: fn(Value) -> Value) -> Self {
        Value::Builtin {
            name: Symbol::new(name),
            builtin: Builtin(f),
        }
    }
}

pub fn builtins() -> SymbolMap<Value> {
    SymbolMap::default().update(Symbol::new("eq"), Value::builtin("eq", eq))
}

fn eq(val: Value) -> Value {
    let map = val.unwrap_record();
    let result = map[&Symbol::new("l")] == map[&Symbol::new("r")];
    Value::Bool(result)
}
