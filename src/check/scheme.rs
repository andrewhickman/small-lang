use std::iter::once;

use mlsub::auto::{Automaton, StateId};
use mlsub::Polarity;

use crate::check::ty::Constructor;
use crate::syntax::{ImSymbolMap, Symbol};

#[derive(Debug, Clone)]
pub(in crate::check) struct Scheme {
    ty: StateId,
    env: ImSymbolMap<StateId>,
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

    pub fn deep_clone(&self, auto: &mut Automaton<Constructor>) -> Self {
        let env: Vec<(Symbol, StateId)> = self.env.iter().copied().collect();
        let mut cloned = auto.clone_states(
            once((self.ty, Polarity::Pos)).chain(env.iter().map(|&(_, id)| (id, Polarity::Neg))),
        );
        Scheme {
            ty: cloned.next().unwrap(),
            env: itertools::zip_eq(env, cloned)
                .map(|((var, _), id)| (var, id))
                .collect(),
        }
    }
}
