use std::fmt;
use std::rc::Rc;

use crate::rt::{Context, Value};
use crate::syntax::{Symbol, SymbolMap};

#[derive(Clone)]
pub struct Builtin(Rc<dyn Fn(Value) -> Value>);

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

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Value {
    fn builtin(name: &str, f: fn(Value) -> Value) -> Self {
        Value::Builtin {
            name: Symbol::new(name),
            builtin: Builtin(Rc::new(f)),
        }
    }
}

pub fn builtins() -> SymbolMap<Value> {
    SymbolMap::default().update(Symbol::new("eq"), Value::builtin("eq", eq))
}

fn eq(lhs: Value) -> Value {
    Value::Builtin {
        name: Symbol::new("eq-curried"),
        builtin: Builtin(Rc::new(move |rhs| Value::Bool(lhs == rhs))),
    }
}
