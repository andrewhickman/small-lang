use mlsub::Polarity;

use crate::check::{Context, Scheme};
use crate::syntax::Symbol;

impl Context {
    pub(in crate::check) fn set_builtins(&mut self) {
        let eq = self.build_eq();
        self.set_var(Symbol::new("eq"), eq);
    }

    fn build_eq(&mut self) -> Scheme {
        let pair = self.auto.build_var();

        let range = self.build_bool(Polarity::Pos);
        let curried_func = self.build_func(Polarity::Pos, pair.neg, range);
        let func = self.build_func(Polarity::Pos, pair.neg, curried_func);

        Scheme::empty(func)
    }
}
