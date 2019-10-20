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
    fn builtin<F>(name: &str, f: F) -> Self
    where
        F: Fn(Value) -> Value + 'static,
    {
        Value::Builtin {
            name: Symbol::new(name),
            builtin: Builtin(Rc::new(f)),
        }
    }
}

pub fn builtins() -> SymbolMap<Value> {
    let eq = binary_func("eq", |lhs, rhs| Value::Bool(lhs == rhs));
    let add = binary_func("add", |lhs, rhs| {
        Value::Int(lhs.unwrap_int() + rhs.unwrap_int())
    });

    SymbolMap::default()
        .update(Symbol::new("eq"), eq)
        .update(Symbol::new("add"), add)
}

fn binary_func<F>(name: &'static str, f: F) -> Value
where
    F: Fn(Value, Value) -> Value + Clone + 'static,
{
    Value::builtin(name, move |arg0| {
        let f = f.clone();
        Value::builtin(&format!("{}-curried", name), move |arg1| {
            f(arg0.clone(), arg1.clone())
        })
    })
}
