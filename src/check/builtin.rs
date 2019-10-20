use mlsub::auto::StateId;
use mlsub::Polarity;

use crate::check::{Context, Scheme};
use crate::syntax::Symbol;

impl Context {
    pub(in crate::check) fn set_builtins(&mut self) {
        let eq = self.build_eq();
        self.set_var(Symbol::new("eq"), eq);
        let add = self.build_add();
        self.set_var(Symbol::new("add"), add);
    }

    fn build_eq(&mut self) -> Scheme {
        let arg0 = self.auto.build_empty(Polarity::Neg);
        let arg1 = self.auto.build_empty(Polarity::Neg);
        let ret = self.build_bool(Polarity::Pos);

        self.build_binary_func(arg0, arg1, ret)
    }

    fn build_add(&mut self) -> Scheme {
        let arg0 = self.build_int(Polarity::Neg);
        let arg1 = self.build_int(Polarity::Neg);
        let ret = self.build_int(Polarity::Pos);

        self.build_binary_func(arg0, arg1, ret)
    }

    fn build_binary_func(&mut self, arg0: StateId, arg1: StateId, ret: StateId) -> Scheme {
        let func0 = self.build_func(Polarity::Pos, arg1, ret);
        let func1 = self.build_func(Polarity::Pos, arg0, func0);

        Scheme::empty(func1)
    }
}
