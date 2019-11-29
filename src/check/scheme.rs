use std::iter::once;
use std::rc::Rc;

use mlsub::auto::{flow, Automaton, StateId};
use mlsub::Polarity;

use crate::check::ty::Constructor;
use crate::syntax::{ImSymbolMap, Symbol};

/// Represents a typing scheme, with variable substitutions produced by biunification.
#[derive(Debug, Clone)]
pub struct Scheme {
    ty: StateId,
    env: ImSymbolMap<StateId>,
}

/// A typing scheme in reduced form.
#[derive(Debug, Clone)]
pub struct ReducedScheme {
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

    pub fn from_var(var: Symbol, pair: flow::Pair) -> Self {
        Scheme {
            ty: pair.pos,
            env: once((var, pair.neg)).collect(),
        }
    }

    pub fn with_ty(self, ty: StateId) -> Self {
        Scheme { ty, env: self.env }
    }

    pub fn join(auto: &mut Automaton<Constructor>, ty: StateId, lhs: &Self, rhs: &Self) -> Self {
        let env = ImSymbolMap::union_with(lhs.env.clone(), rhs.env.clone(), |l, r| {
            auto.build_add(Polarity::Neg, [l, r].iter().cloned())
        });
        Scheme { ty, env }
    }

    pub fn join_all<'a, I>(auto: &mut Automaton<Constructor>, ty: StateId, schemes: I) -> Self
    where
        I: IntoIterator<Item = &'a Self>,
    {
        schemes.into_iter().fold(Scheme::empty(ty), |lhs, rhs| {
            Scheme::join(auto, ty, &lhs, rhs)
        })
    }

    pub fn ty(&self) -> StateId {
        self.ty
    }

    pub fn without_var(&self, var: Symbol) -> (Self, Option<StateId>) {
        match self.env.extract(&var) {
            Some((ty, env)) => (Scheme { env, ty: self.ty }, Some(ty)),
            None => (self.clone(), None),
        }
    }

    pub(in crate::check) fn reduce(&self, auto: &Automaton<Constructor>) -> ReducedScheme {
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
