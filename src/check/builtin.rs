use mlsub::auto::StateId;
use mlsub::Polarity;

use crate::check::Context;
use crate::syntax::Symbol;

impl Context {
    pub(in crate::check) fn set_builtins(&mut self) {
        let eq = self.build_eq();
        self.set_var(Symbol::new("__builtin_eq"), eq);
        let add = self.build_binary_int_op();
        self.set_var(Symbol::new("__builtin_add"), add);
        let sub = self.build_binary_int_op();
        self.set_var(Symbol::new("__builtin_sub"), sub);
    }

    fn build_eq(&mut self) -> StateId {
        let arg0 = self.auto.build_empty(Polarity::Neg);
        let arg1 = self.auto.build_empty(Polarity::Neg);
        let ret = self.build_bool(Polarity::Pos);

        self.build_binary_func(arg0, arg1, ret)
    }

    fn build_binary_int_op(&mut self) -> StateId {
        let arg0 = self.build_int(Polarity::Neg);
        let arg1 = self.build_int(Polarity::Neg);
        let ret = self.build_int(Polarity::Pos);

        self.build_binary_func(arg0, arg1, ret)
    }

    fn build_binary_func(&mut self, lhs: StateId, rhs: StateId, ret: StateId) -> StateId {
        let arg = self.build_record(
            Polarity::Neg,
            [(Symbol::new("l"), lhs), (Symbol::new("r"), rhs)]
                .iter()
                .copied(),
        );
        self.build_func(Polarity::Pos, arg, ret)
    }
}
