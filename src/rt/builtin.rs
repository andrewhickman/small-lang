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

fn get_add(lhs: Value) -> Result<Value, Error> {
    match lhs {
        Value::Number(_) => Ok(Value::builtin("__builtin_add_number", move |rhs| {
            add_number(lhs.clone(), rhs)
        })),
        Value::String(_) => Ok(Value::builtin("__builtin_add_string", move |rhs| {
            add_string(lhs.clone(), rhs)
        })),
        _ => panic!("expected addable"),
    }
}

fn get_sub(lhs: Value) -> Result<Value, Error> {
    Ok(Value::builtin("__builtin_sub", move |rhs| {
        sub_number(lhs.clone(), rhs)
    }))
}

fn add_number(lhs: Value, rhs: Value) -> Result<Value, Error> {
    let num = coerce_for_binary_func(
        lhs.unwrap_number(),
        rhs.unwrap_number(),
        |lhs, rhs| {
            i64::checked_add(lhs, rhs)
                .map(NumberValue::Int)
                .ok_or(Error::IntegerOverflow)
        },
        |lhs, rhs| Ok(NumberValue::Float(lhs + rhs)),
    )?;
    Ok(Value::Number(num))
}

fn sub_number(lhs: Value, rhs: Value) -> Result<Value, Error> {
    let num = coerce_for_binary_func(
        lhs.unwrap_number(),
        rhs.unwrap_number(),
        |lhs, rhs| {
            i64::checked_sub(lhs, rhs)
                .map(NumberValue::Int)
                .ok_or(Error::IntegerOverflow)
        },
        |lhs, rhs| Ok(NumberValue::Float(lhs - rhs)),
    )?;
    Ok(Value::Number(num))
}

fn coerce_int(val: i64) -> f64 {
    // Casts to nearest float if not exactly representable
    val as f64
}

fn coerce_for_binary_func<T, I, F>(
    lhs: NumberValue,
    rhs: NumberValue,
    int_func: I,
    float_func: F,
) -> T
where
    I: FnOnce(i64, i64) -> T,
    F: FnOnce(f64, f64) -> T,
{
    match (lhs, rhs) {
        (NumberValue::Int(lhs), NumberValue::Int(rhs)) => int_func(lhs, rhs),
        (NumberValue::Int(lhs), NumberValue::Float(rhs)) => float_func(coerce_int(lhs), rhs),
        (NumberValue::Float(lhs), NumberValue::Int(rhs)) => float_func(lhs, coerce_int(rhs)),
        (NumberValue::Float(lhs), NumberValue::Float(rhs)) => float_func(lhs, rhs),
    }
}

fn add_string(lhs: Value, rhs: Value) -> Result<Value, Error> {
    let mut s = lhs.unwrap_string();
    s += &rhs.unwrap_string();
    Ok(Value::String(s))
}

impl PartialEq for NumberValue {
    fn eq(&self, other: &Self) -> bool {
        coerce_for_binary_func(*self, *other, |l, r| l == r, |l, r| l == r)
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
