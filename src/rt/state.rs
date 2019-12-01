use std::collections::HashMap;
use std::iter::FromIterator;

use crate::check::vars::VarId;
use crate::rt::{Command, Error, Opts, Output, Value};

#[derive(Debug)]
pub(in crate::rt) struct Runtime {
    stack: Vec<Value>,
    builtins: HashMap<VarId, Value>,
    var_stack: Vec<HashMap<VarId, Value>>,
    opts: Opts,
    op_count: u64,
}

impl Runtime {
    pub fn new(builtins: HashMap<VarId, Value>, opts: Opts) -> Self {
        Runtime {
            stack: vec![],
            builtins,
            var_stack: vec![HashMap::new()],
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
        assert_eq!(self.var_stack.len(), 1);
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

    pub fn push_scope<I>(&mut self, vars: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = (VarId, Value)>,
    {
        if self.var_stack.len() as u64 >= self.opts.max_stack {
            return Err(Error::StackOverflow);
        }
        self.var_stack.push(HashMap::from_iter(vars));
        log::trace!("push scope {:?}", self.vars());
        Ok(())
    }

    pub fn vars(&self) -> &HashMap<VarId, Value> {
        self.var_stack.last().unwrap()
    }

    pub fn vars_mut(&mut self) -> &mut HashMap<VarId, Value> {
        self.var_stack.last_mut().unwrap()
    }

    pub fn set_var(&mut self, var: VarId, value: Value) {
        log::trace!("set var {:?}", var);
        self.vars_mut().insert(var, value);
    }

    pub fn get_var(&self, var: VarId) -> Value {
        log::trace!("get var {:?}", var);
        if let Some(value) = self.builtins.get(&var) {
            value.clone()
        } else {
            self.vars()[&var].clone()
        }
    }

    pub fn pop_scope(&mut self) {
        log::trace!("pop scope");
        self.var_stack.pop();
    }
}
