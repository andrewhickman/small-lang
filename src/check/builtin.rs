use mlsub::auto::StateId;
use mlsub::Polarity;

use crate::check::vars::VarId;
use crate::check::{Context, Scheme};
use crate::syntax::Symbol;

impl<T, F> Context<T, F> {
    pub(in crate::check) fn set_builtins(&mut self) {
        let eq = self.build_eq();
        assert_eq!(
            self.push_var(Symbol::new("__builtin_eq"), None, &eq),
            VarId::BUILTIN_EQ
        );
        let add = self.build_capability_getter(Symbol::new("add"));
        assert_eq!(
            self.push_var(Symbol::new("__builtin_get_add"), None, &add),
            VarId::BUILTIN_GET_ADD
        );
        let sub = self.build_capability_getter(Symbol::new("sub"));
        assert_eq!(
            self.push_var(Symbol::new("__builtin_get_sub"), None, &sub),
            VarId::BUILTIN_GET_SUB
        );

        assert_eq!(self.vars.len(), VarId::NUM_BUILTINS);
    }

    fn build_eq(&mut self) -> Scheme {
        let arg0 = self.auto.build_empty(Polarity::Neg);
        let arg1 = self.auto.build_empty(Polarity::Neg);
        let ret = self.build_bool(Polarity::Pos, None);

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

    fn build_capability_getter(&mut self, name: Symbol) -> Scheme {
        let capability_pair = self.auto.build_var();
        let capability = self.build_capability(Polarity::Neg, None, name, capability_pair.neg);

        Scheme::empty(self.build_func(Polarity::Pos, None, capability, capability_pair.pos))
    }
}
