use im::OrdMap;

use crate::rt::{Command, Error, Opts, Output, Value};
use crate::syntax::Symbol;

#[derive(Debug)]
pub(in crate::rt) struct Runtime {
    stack: Vec<Value>,
    vars: Vec<OrdMap<Symbol, Value>>,
    opts: Opts,
    op_count: u64,
}

impl Runtime {
    pub fn new(vars: OrdMap<Symbol, Value>, opts: Opts) -> Self {
        Runtime {
            stack: vec![],
            vars: vec![vars],
            opts,
            op_count: 0,
        }
    }

    pub fn incr_op_count(&mut self) -> Result<(), Error> {
        self.op_count += 1;
        if let Some(max_ops) = self.opts.max_ops {
            if self.op_count > max_ops {
                return Err(Error::TooManyOps);
            }
        }
        Ok(())
    }

    pub fn finish(self) -> Output {
        assert_eq!(self.stack.len(), 1);
        assert_eq!(self.vars.len(), 1);
        Output {
            value: self.stack.into_iter().next().unwrap(),
            op_count: self.op_count,
        }
    }

    pub fn exec_all(&mut self, cmds: &[Command]) -> Result<(), Error> {
        let mut idx = 0;
        while let Some(cmd) = cmds.get(idx) {
            self.incr_op_count()?;

            idx += cmd.exec(self)?.unwrap_or(0);
            idx += 1;
        }
        Ok(())
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

    pub fn push_vars(&mut self, new_vars: OrdMap<Symbol, Value>) -> Result<(), Error> {
        if self.vars.len() as u64 >= self.opts.max_stack {
            return Err(Error::StackOverflow);
        }
        let all_vars = new_vars.union(self.vars().clone());
        self.vars.push(all_vars);
        Ok(())
    }

    pub fn vars(&self) -> &OrdMap<Symbol, Value> {
        self.vars.last().unwrap()
    }

    pub fn get_var(&self, var: Symbol) -> Value {
        self.vars()[&var].clone()
    }

    pub fn pop_vars(&mut self) {
        self.vars.pop().unwrap();
    }
}
