use im::{ordmap, OrdMap};
use mlsub::auto::{StateId, StateSet};
use mlsub::Polarity;

use crate::check::{Context, Scheme};
use crate::syntax::Symbol;

impl<'a> Context<'a> {
    pub(in crate::check) fn set_builtins(&mut self) {
        self.set_empty_capabilities();
        self.set_int_capabilities();
        self.set_float_capabilities();
        self.set_string_capabilities();

        let eq = self.build_eq();
        self.set_var(Symbol::new("__builtin_eq"), eq);
        let add = self.build_capability_getter(Symbol::new("add"));
        self.set_var(Symbol::new("__builtin_get_add"), add);
        let sub = self.build_capability_getter(Symbol::new("sub"));
        self.set_var(Symbol::new("__builtin_get_sub"), sub);
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

    fn set_empty_capabilities(&mut self) {
        self.capabilities
            .set_empty(OrdMap::default(), OrdMap::default())
    }

    fn set_int_capabilities(&mut self) {
        let pos = self.build_int_capabilities(Polarity::Pos);
        let neg = self.build_int_capabilities(Polarity::Neg);
        self.capabilities.set_int(pos, neg)
    }

    fn set_string_capabilities(&mut self) {
        let pos = self.build_string_capabilities(Polarity::Pos);
        let neg = self.build_string_capabilities(Polarity::Neg);
        self.capabilities.set_string(pos, neg);
    }

    fn set_float_capabilities(&mut self) {
        let pos = self.build_float_capabilities(Polarity::Pos);
        let neg = self.build_float_capabilities(Polarity::Neg);
        self.capabilities.set_float(pos, neg)
    }

    fn build_int_capabilities(&mut self, pol: Polarity) -> OrdMap<Symbol, StateSet> {
        ordmap! {
            Symbol::new("add") => StateSet::new(self.build_binary_int_op(pol)),
            Symbol::new("sub") => StateSet::new(self.build_binary_int_op(pol))
        }
    }

    fn build_float_capabilities(&mut self, pol: Polarity) -> OrdMap<Symbol, StateSet> {
        ordmap! {
            Symbol::new("add") => StateSet::new(self.build_binary_float_op(pol)),
            Symbol::new("sub") => StateSet::new(self.build_binary_float_op(pol))
        }
    }

    fn build_binary_int_op(&mut self, pol: Polarity) -> StateId {
        let arg = self.build_int(-pol, None);
        let ret = self.build_int(pol, None);

        self.build_func(pol, None, arg, ret)
    }

    fn build_binary_float_op(&mut self, pol: Polarity) -> StateId {
        let arg = self.build_float(-pol, None);
        let ret = self.build_float(pol, None);

        self.build_func(pol, None, arg, ret)
    }

    fn build_string_capabilities(&mut self, pol: Polarity) -> OrdMap<Symbol, StateSet> {
        let add_arg = self.build_string(-pol, None);
        let add_ret = self.build_string(pol, None);
        let add = self.build_func(pol, None, add_arg, add_ret);
        ordmap! {
            Symbol::new("add") => StateSet::new(add)
        }
    }
}
