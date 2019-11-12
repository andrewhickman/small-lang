use std::fmt;
use std::rc::Rc;

use crate::rt::{Error, FuncValue, NumberValue, Runtime, Value};
use crate::syntax::{ImSymbolMap, Symbol};

#[derive(Clone)]
pub struct Builtin(Rc<dyn Fn(Value) -> Result<Value, Error>>);

impl Builtin {
    pub(in crate::rt) fn exec(&self, ctx: &mut Runtime) -> Result<(), Error> {
        let arg = ctx.pop_stack();
        let ret = (self.0)(arg)?;
        ctx.push_stack(ret);
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

pub fn builtins() -> ImSymbolMap<Value> {
    ImSymbolMap::default()
        .update(Symbol::new("__builtin_eq"), binary_func("__builtin_eq", eq))
        .update(
            Symbol::new("__builtin_add"),
            binary_func("__builtin_add", add),
        )
        .update(
            Symbol::new("__builtin_sub"),
            binary_func("__builtin_sub", sub),
        )
}

fn binary_func<F>(name: &'static str, f: F) -> Value
where
    F: Fn(Value, Value) -> Result<Value, Error> + Clone + 'static,
{
    Value::builtin(name, move |arg| {
        let record = arg.unwrap_record();
        f(
            record[&Symbol::new("l")].clone(),
            record[&Symbol::new("r")].clone(),
        )
    })
}

fn eq(lhs: Value, rhs: Value) -> Result<Value, Error> {
    Ok(Value::Bool(lhs == rhs))
}

fn add(lhs: Value, rhs: Value) -> Result<Value, Error> {
    if let Some(result) = i64::checked_add(
        lhs.unwrap_number().unwrap_int(),
        rhs.unwrap_number().unwrap_int(),
    ) {
        Ok(Value::Number(NumberValue::Int(result)))
    } else {
        Err(Error::IntegerOverflow)
    }
}

fn sub(lhs: Value, rhs: Value) -> Result<Value, Error> {
    if let Some(result) = i64::checked_sub(
        lhs.unwrap_number().unwrap_int(),
        rhs.unwrap_number().unwrap_int(),
    ) {
        Ok(Value::Number(NumberValue::Int(result)))
    } else {
        Err(Error::IntegerOverflow)
    }
}

fn coerce_int(val: i64) -> f64 {
    // Casts to nearest float if not exactly representable
    val as f64
}

impl PartialEq for NumberValue {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (NumberValue::Int(lhs), NumberValue::Int(rhs)) => lhs == rhs,
            (NumberValue::Int(lhs), NumberValue::Float(rhs)) => coerce_int(lhs) == rhs,
            (NumberValue::Float(lhs), NumberValue::Int(rhs)) => lhs == coerce_int(rhs),
            (NumberValue::Float(lhs), NumberValue::Float(rhs)) => lhs == rhs,
        }
    }
}

impl PartialEq for FuncValue {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.cmds, &other.cmds) && self.env == other.env
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Record(l), Value::Record(r)) => l == r,
            (Value::Enum(l), Value::Enum(r)) => l == r,
            (Value::Func(l), Value::Func(r)) => l == r,
            (Value::Builtin { builtin: l, .. }, Value::Builtin { builtin: r, .. }) => l == r,
            _ => false,
        }
    }
}
