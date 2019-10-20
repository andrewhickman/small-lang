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
        let arg = self.auto.build_empty(Polarity::Neg);
        let range = self.build_bool(Polarity::Pos);
        let func = self.build_func(Polarity::Pos, arg, range);

        let arg = self.auto.build_empty(Polarity::Neg);
        let func = self.build_func(Polarity::Pos, arg, func);

        Scheme::empty(func)
    }

    fn build_add(&mut self) -> Scheme {
        let arg = self.build_int(Polarity::Neg);
        let range = self.build_int(Polarity::Pos);
        let func = self.build_func(Polarity::Pos, arg, range);

        let arg = self.build_int(Polarity::Neg);
        let func = self.build_func(Polarity::Pos, arg, func);

        Scheme::empty(func)
    }
}
