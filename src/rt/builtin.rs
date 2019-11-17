use std::fmt;
use std::ops::{Add, Sub};
use std::rc::Rc;

use crate::rt::{Error, FuncValue, Runtime, Value};
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
            Symbol::new("__builtin_get_add"),
            Value::builtin("__builtin_get_add", get_add),
        )
        .update(
            Symbol::new("__builtin_get_sub"),
            Value::builtin("__builtin_get_sub", get_sub),
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

fn get_add(val: Value) -> Result<Value, Error> {
    match val {
        lhs @ Value::Int(_) => Ok(Value::builtin("__builtin_add_int", move |rhs| {
            add_int(lhs.clone(), rhs)
        })),
        lhs @ Value::Float(_) => Ok(Value::builtin("__builtin_add_float", move |rhs| {
            add_float(lhs.clone(), rhs)
        })),
        lhs @ Value::String(_) => Ok(Value::builtin("__builtin_add_string", move |rhs| {
            add_string(lhs.clone(), rhs)
        })),
        _ => panic!("expected add capability"),
    }
}

fn get_sub(val: Value) -> Result<Value, Error> {
    match val {
        lhs @ Value::Int(_) => Ok(Value::builtin("__builtin_sub_int", move |rhs| {
            sub_int(lhs.clone(), rhs)
        })),
        lhs @ Value::Float(_) => Ok(Value::builtin("__builtin_sub_float", move |rhs| {
            sub_float(lhs.clone(), rhs)
        })),
        _ => panic!("expected add capability"),
    }
}

fn add_int(lhs: Value, rhs: Value) -> Result<Value, Error> {
    Ok(Value::Int(
        i64::checked_add(lhs.unwrap_int(), rhs.unwrap_int()).ok_or(Error::IntegerOverflow)?,
    ))
}

fn sub_int(lhs: Value, rhs: Value) -> Result<Value, Error> {
    Ok(Value::Int(
        i64::checked_sub(lhs.unwrap_int(), rhs.unwrap_int()).ok_or(Error::IntegerOverflow)?,
    ))
}

fn add_float(lhs: Value, rhs: Value) -> Result<Value, Error> {
    Ok(Value::Float(f64::add(
        lhs.unwrap_float(),
        rhs.unwrap_float(),
    )))
}

fn add_string(lhs: Value, rhs: Value) -> Result<Value, Error> {
    let mut s = lhs.unwrap_string();
    s += &rhs.unwrap_string();
    Ok(Value::String(s))
}

fn sub_float(lhs: Value, rhs: Value) -> Result<Value, Error> {
    Ok(Value::Float(f64::sub(
        lhs.unwrap_float(),
        rhs.unwrap_float(),
    )))
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
            (Value::Int(l), Value::Int(r)) => l == r,
            (Value::Float(l), Value::Float(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Record(l), Value::Record(r)) => l == r,
            (Value::Enum(l), Value::Enum(r)) => l == r,
            (Value::Func(l), Value::Func(r)) => l == r,
            (Value::Builtin { builtin: l, .. }, Value::Builtin { builtin: r, .. }) => l == r,
            _ => false,
        }
    }
}
