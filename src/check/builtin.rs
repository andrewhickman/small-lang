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
        let dom = self.build_record(
            Polarity::Neg,
            [(Symbol::new("l"), pair.neg), (Symbol::new("r"), pair.neg)]
                .iter()
                .copied(),
        );

        let range = self.build_bool(Polarity::Pos);

        let func = self.build_func(Polarity::Pos, dom, range);
        Scheme::empty(func)
    }
}
