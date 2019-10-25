use std::fmt;
use std::rc::Rc;

use crate::rt::{Error, Runtime, Value};
use crate::syntax::{Symbol, SymbolMap};

#[derive(Clone)]
pub struct Builtin(Rc<dyn Fn(Value) -> Result<Value, Error>>);

impl Builtin {
    pub(in crate::rt) fn exec(&self, ctx: &mut Runtime) -> Result<(), Error> {
        let arg = ctx.stack.pop().unwrap();
        let ret = (self.0)(arg)?;
        ctx.stack.push(ret);
        Ok(())
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
        F: Fn(Value) -> Result<Value, Error> + 'static,
    {
        Value::Builtin {
            name: Symbol::new(name),
            builtin: Builtin(Rc::new(f)),
        }
    }
}

pub fn builtins() -> SymbolMap<Value> {
    SymbolMap::default()
        .update(Symbol::new("eq"), binary_func("eq", eq))
        .update(Symbol::new("add"), binary_func("add", add))
        .update(Symbol::new("sub"), binary_func("sub", sub))
}

fn binary_func<F>(name: &'static str, f: F) -> Value
where
    F: Fn(Value, Value) -> Result<Value, Error> + Clone + 'static,
{
    Value::builtin(name, move |arg0| {
        let f = f.clone();
        Ok(Value::builtin(&format!("{}-curried", name), move |arg1| {
            f(arg0.clone(), arg1.clone())
        }))
    })
}

fn eq(lhs: Value, rhs: Value) -> Result<Value, Error> {
    Ok(Value::Bool(lhs == rhs))
}

fn add(lhs: Value, rhs: Value) -> Result<Value, Error> {
    if let Some(result) = i64::checked_add(lhs.unwrap_int(), rhs.unwrap_int()) {
        Ok(Value::Int(result))
    } else {
        Err(Error::IntegerOverflow)
    }
}

fn sub(lhs: Value, rhs: Value) -> Result<Value, Error> {
    if let Some(result) = i64::checked_sub(lhs.unwrap_int(), rhs.unwrap_int()) {
        Ok(Value::Int(result))
    } else {
        Err(Error::IntegerOverflow)
    }
}
