use std::iter::once;
use std::rc::Rc;

use mlsub::auto::{Automaton, StateId};
use mlsub::Polarity;

use crate::check::ty::Constructor;
use crate::syntax::{ImSymbolMap, Symbol};

#[derive(Debug, Clone)]
pub(in crate::check) struct Scheme {
    ty: StateId,
    env: ImSymbolMap<StateId>,
}

#[derive(Debug, Clone)]
pub(in crate::check) struct ReducedScheme {
    auto: Rc<Automaton<Constructor>>,
    scheme: Scheme,
}

impl Scheme {
    pub fn empty(ty: StateId) -> Self {
        Scheme {
            ty,
            env: ImSymbolMap::default(),
        }
    }

    pub fn singleton(ty: StateId, var: (Symbol, StateId)) -> Self {
        Scheme {
            ty,
            env: once(var).collect(),
        }
    }

    pub fn with_ty(self, ty: StateId) -> Self {
        Scheme { ty, env: self.env }
    }

    pub fn join(auto: &mut Automaton<Constructor>, ty: StateId, lhs: Self, rhs: Self) -> Self {
        let env = ImSymbolMap::union_with(lhs.env, rhs.env, |l, r| {
            auto.build_add(Polarity::Neg, [l, r].iter().cloned())
        });
        Scheme { ty, env }
    }

    pub fn join_all<I>(auto: &mut Automaton<Constructor>, ty: StateId, schemes: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        schemes.into_iter().fold(Scheme::empty(ty), |lhs, rhs| {
            Scheme::join(auto, ty, lhs, rhs)
        })
    }

    pub fn ty(&self) -> StateId {
        self.ty
    }

    pub fn remove_var(&mut self, var: Symbol) -> Option<StateId> {
        self.env.remove(&var)
    }

    pub fn reduce(&self, auto: &Automaton<Constructor>) -> ReducedScheme {
        let env: Vec<(Symbol, StateId)> = self.env.iter().copied().collect();

        let mut reduced_auto = Automaton::new();
        let mut range = reduced_auto.reduce(
            auto,
            once((self.ty, Polarity::Pos)).chain(env.iter().map(|&(_, id)| (id, Polarity::Neg))),
        );

        ReducedScheme {
            auto: Rc::new(reduced_auto),
            scheme: Scheme {
                ty: range.next().unwrap(),
                env: itertools::zip_eq(env, range)
                    .map(|((var, _), id)| (var, id))
                    .collect(),
            },
        }
    }
}

impl ReducedScheme {
    pub fn add_to(&self, auto: &mut Automaton<Constructor>) -> Scheme {
        let offset = auto.add_from(&self.auto);

        Scheme {
            ty: self.scheme.ty.shift(offset),
            env: self
                .scheme
                .env
                .iter()
                .map(|&(var, id)| (var, id.shift(offset)))
                .collect(),
        }
    }
}
