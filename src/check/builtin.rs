use mlsub::auto::StateId;
use mlsub::Polarity;

use crate::check::ty::Number;
use crate::check::{Context, Scheme};
use crate::syntax::Symbol;

impl<'a> Context<'a> {
    pub(in crate::check) fn set_builtins(&mut self) {
        let eq = self.build_eq();
        self.set_var(Symbol::new("__builtin_eq"), eq);
        let add = self.build_binary_int_op();
        self.set_var(Symbol::new("__builtin_add"), add);
        let sub = self.build_binary_int_op();
        self.set_var(Symbol::new("__builtin_sub"), sub);
    }

    fn build_eq(&mut self) -> Scheme {
        let arg0 = self.auto.build_empty(Polarity::Neg);
        let arg1 = self.auto.build_empty(Polarity::Neg);
        let ret = self.build_bool(Polarity::Pos, None);

        self.build_binary_func(arg0, arg1, ret)
    }

    fn build_binary_int_op(&mut self) -> Scheme {
        // TODO handle constraints originating here in error messages
        let arg0 = self.build_number(Polarity::Neg, None, Number::Int);
        let arg1 = self.build_number(Polarity::Neg, None, Number::Int);
        let ret = self.build_number(Polarity::Pos, None, Number::Int);

        self.build_binary_func(arg0, arg1, ret)
    }

    fn build_binary_func(&mut self, lhs: StateId, rhs: StateId, ret: StateId) -> Scheme {
        let arg = self.build_record(
            Polarity::Neg,
            None,
            [(Symbol::new("l"), lhs), (Symbol::new("r"), rhs)]
                .iter()
                .copied(),
        );
        Scheme::empty(self.build_func(Polarity::Pos, None, arg, ret))
    }
}
