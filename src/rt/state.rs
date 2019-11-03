use crate::rt::{Error, FuncValue, Opts, Value};
use crate::syntax::ImSymbolMap;

#[derive(Debug)]
pub(in crate::rt) struct Runtime {
    stack: Vec<Value>,
    vars: Vec<ImSymbolMap<Value>>,
    pub opts: Opts,
}

impl Runtime {
    pub fn new(func: FuncValue, vars: ImSymbolMap<Value>, opts: Opts) -> Self {
        Runtime {
            stack: vec![Value::Func(func)],
            vars: vec![vars],
            opts,
        }
    }

    pub fn finish(self) -> Value {
        assert_eq!(self.stack.len(), 1);
        self.stack.into_iter().next().unwrap()
    }

    pub fn pop_stack(&mut self) -> Value {
        let value = self.stack.pop().unwrap();
        log::trace!("pop {:?}", value);
        value
    }

    pub fn push_stack(&mut self, value: Value) {
        log::trace!("push {:?}", value);
        self.stack.push(value)
    }

    pub fn push_vars(&mut self, new_vars: ImSymbolMap<Value>) -> Result<(), Error> {
        if self.vars.len() as u64 >= self.opts.max_stack {
            return Err(Error::StackOverflow);
        }
        let all_vars = new_vars.union(self.vars().clone());
        self.vars.push(all_vars);
        Ok(())
    }

    pub fn vars(&mut self) -> &mut ImSymbolMap<Value> {
        self.vars.last_mut().unwrap()
    }

    pub fn pop_vars(&mut self) {
        self.vars.pop().unwrap();
    }
}
