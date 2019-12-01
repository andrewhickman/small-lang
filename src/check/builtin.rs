use im::{ordmap, OrdMap};
use mlsub::auto::{StateId, StateSet};
use mlsub::Polarity;

use crate::check::ty::NumberConstructor;
use crate::check::vars::VarId;
use crate::check::{Context, Scheme};
use crate::syntax::Symbol;

impl<F> Context<F> {
    pub(in crate::check) fn set_builtins(&mut self) {
        self.set_empty_capabilities();
        self.set_number_capabilities();
        self.set_string_capabilities();

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

    fn set_empty_capabilities(&mut self) {
        self.capabilities
            .set_empty(OrdMap::default(), OrdMap::default())
    }

    fn set_number_capabilities(&mut self) {
        let pos = self.build_number_capabilities(Polarity::Pos, NumberConstructor::Int);
        let neg = self.build_number_capabilities(Polarity::Neg, NumberConstructor::Int);
        self.capabilities.set_int(pos, neg);
        let pos = self.build_number_capabilities(Polarity::Pos, NumberConstructor::Float);
        let neg = self.build_number_capabilities(Polarity::Neg, NumberConstructor::Float);
        self.capabilities.set_float(pos, neg);
    }

    fn set_string_capabilities(&mut self) {
        let pos = self.build_string_capabilities(Polarity::Pos);
        let neg = self.build_string_capabilities(Polarity::Neg);
        self.capabilities.set_string(pos, neg);
    }

    fn build_number_capabilities(
        &mut self,
        pol: Polarity,
        num: NumberConstructor,
    ) -> OrdMap<Symbol, StateSet> {
        ordmap! {
            Symbol::new("add") => StateSet::new(self.build_binary_number_op(pol, num)),
            Symbol::new("sub") => StateSet::new(self.build_binary_number_op(pol, num))
        }
    }

    fn build_number_arg(&mut self, pol: Polarity, num: NumberConstructor) -> (StateId, StateId) {
        let pair = self.auto.build_var();
        let float = self.build_number(-pol, None, num);

        let arg = self
            .auto
            .build_add(-pol, [pair.get(-pol), float].iter().copied());
        (arg, pair.get(pol))
    }

    fn build_binary_number_op(&mut self, pol: Polarity, num: NumberConstructor) -> StateId {
        let ret0 = self.build_number(pol, None, num);
        let (arg, ret1) = self.build_number_arg(pol, NumberConstructor::Float);

        let ret = self.auto.build_add(pol, [ret0, ret1].iter().copied());
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
