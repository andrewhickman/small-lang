mod ty;

use std::iter::once;

use mlsub::auto::{Automaton, StateId, StateSet};
use mlsub::Polarity;

use crate::check::ty::Constructor;
use crate::syntax::{Expr, Symbol, SymbolMap};

pub fn check(expr: &Expr) -> Result<(), &'static str> {
    let mut ctx = Context::default();
    let mut reduced = Automaton::new();

    let scheme = ctx.check_expr(expr).map_err(|()| "inference error")?;

    // put scheme into reduced form.
    let mut states = reduced.reduce(
        &ctx.auto,
        once((scheme.expr, Polarity::Pos)).chain(scheme.env.values().map(|&v| (v, Polarity::Neg))),
    );
    let actual = states.next().unwrap();

    // build expected type, bool -> unit
    let b = reduced.build_constructed(Polarity::Neg, Constructor::Bool);
    let u = reduced.build_empty(Polarity::Pos);
    let expected = reduced.build_constructed(
        Polarity::Pos,
        Constructor::Func(StateSet::new(b), StateSet::new(u)),
    );

    reduced
        .subsume(expected, actual)
        .map_err(|()| "invalid main type")
}

struct Context {
    auto: Automaton<Constructor>,
    vars: Vec<SymbolMap<Scheme>>,
}

#[derive(Debug, Clone)]
struct Scheme {
    expr: StateId,
    env: SymbolMap<StateId>,
}

impl Context {
    fn default() -> Self {
        Context {
            auto: Automaton::new(),
            vars: vec![SymbolMap::default()],
        }
    }
}

impl Context {
    fn check_expr(&mut self, expr: &Expr) -> Result<Scheme, ()> {
        match expr {
            Expr::Var(symbol) => self.check_var(*symbol),
            Expr::Abs(symbol, expr) => self.check_func(*symbol, expr),
            Expr::App(func, arg) => self.check_call(func, arg),
            Expr::Let(symbol, val, expr) => self.check_let(*symbol, val, expr),
            Expr::True => self.check_bool(true),
            Expr::False => self.check_bool(false),
            Expr::If(cond, cons, alt) => self.check_if(cond, cons, alt),
            Expr::Cons(map) => self.check_record(map),
            Expr::Proj(expr, label) => self.check_proj(expr, *label),
        }
    }

    fn check_var(&mut self, symbol: Symbol) -> Result<Scheme, ()> {
        if let Some(scheme) = self.get_var(symbol) {
            return Ok(scheme);
        }

        let pair = self.auto.build_var();
        let env = SymbolMap::default();
        Ok(Scheme {
            expr: pair.pos,
            env: env.update(symbol, pair.neg),
        })
    }

    fn check_func(&mut self, symbol: Symbol, expr: &Expr) -> Result<Scheme, ()> {
        let mut body = self.check_expr(expr)?;
        let dom = body
            .env
            .remove(&symbol)
            .unwrap_or_else(|| self.auto.build_empty(Polarity::Neg));
        let func = self.build_func(Polarity::Pos, dom, body.expr);
        Ok(Scheme {
            env: body.env,
            expr: func,
        })
    }

    fn check_call(&mut self, func: &Expr, arg: &Expr) -> Result<Scheme, ()> {
        let func = self.check_expr(func)?;
        let arg = self.check_expr(arg)?;

        let pair = self.auto.build_var();
        let f = self.build_func(Polarity::Neg, arg.expr, pair.neg);
        self.auto.biunify(func.expr, f)?;

        Ok(Scheme {
            expr: pair.pos,
            env: self.meet_env(func.env, arg.env),
        })
    }

    fn check_let(&mut self, symbol: Symbol, val: &Expr, expr: &Expr) -> Result<Scheme, ()> {
        let val = self.check_expr(val)?;
        self.push_var(symbol, val.clone());
        let expr = self.check_expr(expr)?;
        self.pop_var();
        Ok(Scheme {
            env: self.meet_env(val.env, expr.env),
            expr: expr.expr,
        })
    }

    fn check_bool(&mut self, val: bool) -> Result<Scheme, ()> {
        let expr = self
            .auto
            .build_constructed(Polarity::Pos, Constructor::Bool);
        Ok(Scheme {
            expr,
            env: SymbolMap::default(),
        })
    }

    fn check_if(&mut self, cond: &Expr, cons: &Expr, alt: &Expr) -> Result<Scheme, ()> {
        let cond = self.check_expr(cond)?;
        let cons = self.check_expr(cons)?;
        let alt = self.check_expr(alt)?;

        let pair = self.auto.build_var();
        let b = self
            .auto
            .build_constructed(Polarity::Neg, Constructor::Bool);

        self.auto.biunify_all(
            [(cond.expr, b), (cons.expr, pair.neg), (alt.expr, pair.neg)]
                .iter()
                .cloned(),
        )?;

        let env = self.meet_envs([cond.env, cons.env, alt.env].iter().cloned());
        Ok(Scheme {
            env,
            expr: pair.pos,
        })
    }

    fn check_record(&mut self, rec: &SymbolMap<Expr>) -> Result<Scheme, ()> {
        let ids = rec
            .iter()
            .map(|(symbol, expr)| Ok((*symbol, self.check_expr(expr)?)))
            .collect::<Result<Vec<(Symbol, Scheme)>, ()>>()?;
        let expr = self.build_record(
            Polarity::Pos,
            ids.iter().map(|(symbol, scheme)| (*symbol, scheme.expr)),
        );
        let env = self.meet_envs(ids.into_iter().map(|(_, scheme)| scheme.env));
        Ok(Scheme { expr, env })
    }

    fn check_proj(&mut self, expr: &Expr, symbol: Symbol) -> Result<Scheme, ()> {
        let expr = self.check_expr(expr)?;

        let pair = self.auto.build_var();
        let rec = self.build_record(Polarity::Neg, once((symbol, pair.neg)));
        self.auto.biunify(expr.expr, rec)?;

        Ok(Scheme {
            expr: pair.pos,
            env: expr.env,
        })
    }

    fn push_var(&mut self, symbol: Symbol, scheme: Scheme) {
        let mut vars = self.vars.last().cloned().unwrap();
        vars.insert(symbol, scheme);
        self.vars.push(vars);
    }

    fn get_var(&mut self, symbol: Symbol) -> Option<Scheme> {
        self.vars.last().unwrap().get(&symbol).cloned()
    }

    fn pop_var(&mut self) {
        self.vars.pop();
    }

    fn meet_env(&mut self, lhs: SymbolMap<StateId>, rhs: SymbolMap<StateId>) -> SymbolMap<StateId> {
        SymbolMap::union_with(lhs, rhs, |l, r| {
            self.auto.build_add(Polarity::Neg, [l, r].iter().cloned())
        })
    }

    fn meet_envs<I>(&mut self, envs: I) -> SymbolMap<StateId>
    where
        I: IntoIterator<Item = SymbolMap<StateId>>,
    {
        envs.into_iter()
            .fold(SymbolMap::default(), |l, r| self.meet_env(l, r))
    }

    fn build_func(&mut self, pol: Polarity, dom: StateId, range: StateId) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::Func(StateSet::new(dom), StateSet::new(range)),
        )
    }

    fn build_record<I>(&mut self, pol: Polarity, iter: I) -> StateId
    where
        I: IntoIterator<Item = (Symbol, StateId)>,
    {
        self.auto.build_constructed(
            pol,
            Constructor::Record(
                iter.into_iter()
                    .map(|(sym, id)| (sym, StateSet::new(id)))
                    .collect(),
            ),
        )
    }
}
