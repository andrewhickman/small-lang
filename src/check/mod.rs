mod ty;

use std::iter::once;

use mlsub::auto::{Automaton, StateId, StateSet};
use mlsub::Polarity;

use crate::check::ty::Constructor;
use crate::rt::{Command, Value};
use crate::syntax::{Expr, LocExpr, Symbol, SymbolMap};

pub fn check(expr: &LocExpr) -> Result<Value, String> {
    let mut ctx = Context::default();
    // let mut reduced = Automaton::new();

    let (_, value) = ctx.check_expr(expr).map_err(|err| match err {
        Error::TypeError => "inference error".to_owned(),
        Error::UndefinedVar(symbol) => format!("undefined var `{}`", symbol),
    })?;

    Ok(Value::Func(value.into(), SymbolMap::default()))

    // // put scheme into reduced form.
    // let mut states = reduced.reduce(
    //     &ctx.auto,
    //     once((scheme.expr, Polarity::Pos)).chain(scheme.env.values().map(|&v| (v, Polarity::Neg))),
    // );
    // let actual = states.next().unwrap();

    // // build expected type, bool -> unit
    // let b = reduced.build_constructed(Polarity::Pos, Constructor::Bool);
    // let u = reduced.build_empty(Polarity::Neg);
    // let expected = reduced.build_constructed(
    //     Polarity::Neg,
    //     Constructor::Func(StateSet::new(b), StateSet::new(u)),
    // );

    // reduced
    //     .biunify(actual, expected)
    //     .map_err(|()| "invalid main type".to_owned())?;

    // assert_eq!(value.len(), 1);
    // Ok(Value::Func(value.into()))
}

enum Error {
    UndefinedVar(Symbol),
    TypeError,
}

impl From<()> for Error {
    fn from((): ()) -> Self {
        Error::TypeError
    }
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
    fn check_expr(&mut self, expr: &LocExpr) -> Result<(Scheme, Vec<Command>), Error> {
        match &expr.expr {
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

    fn check_var(&mut self, symbol: Symbol) -> Result<(Scheme, Vec<Command>), Error> {
        let cmd = Command::Load(symbol);
        if let Some(scheme) = self.get_var(symbol) {
            return Ok((scheme, vec![cmd]));
        }

        Err(Error::UndefinedVar(symbol))
    }

    fn check_func(
        &mut self,
        symbol: Symbol,
        expr: &LocExpr,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let pair = self.auto.build_var();
        self.push_var(
            symbol,
            Scheme {
                expr: pair.pos,
                env: SymbolMap::default().update(symbol, pair.neg),
            },
        );
        let (mut body, mut body_cmds) = self.check_expr(expr)?;
        self.pop_var();

        let dom = body
            .env
            .remove(&symbol)
            .unwrap_or_else(|| self.auto.build_empty(Polarity::Neg));
        let func = self.build_func(Polarity::Pos, dom, body.expr);

        body_cmds.insert(0, Command::Store(symbol));
        body_cmds.push(Command::End);
        let cmd = Command::Capture(body_cmds.into());

        Ok((
            Scheme {
                env: body.env,
                expr: func,
            },
            vec![cmd],
        ))
    }

    fn check_call(
        &mut self,
        func: &LocExpr,
        arg: &LocExpr,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let (func, fcmd) = self.check_expr(func)?;
        let (arg, mut cmds) = self.check_expr(arg)?;

        let pair = self.auto.build_var();
        let f = self.build_func(Polarity::Neg, arg.expr, pair.neg);
        self.auto.biunify(func.expr, f)?;

        cmds.extend(fcmd);
        cmds.push(Command::App);
        Ok((
            Scheme {
                expr: pair.pos,
                env: self.meet_env(func.env, arg.env),
            },
            cmds,
        ))
    }

    fn check_let(
        &mut self,
        symbol: Symbol,
        val: &LocExpr,
        expr: &LocExpr,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let (val, mut cmds) = self.check_expr(val)?;

        self.push_var(symbol, val.clone());
        let (expr, ecmds) = self.check_expr(expr)?;
        self.pop_var();

        cmds.push(Command::Store(symbol));
        cmds.extend(ecmds);
        cmds.push(Command::End);

        Ok((
            Scheme {
                env: self.meet_env(val.env, expr.env),
                expr: expr.expr,
            },
            cmds,
        ))
    }

    fn check_bool(&mut self, val: bool) -> Result<(Scheme, Vec<Command>), Error> {
        let expr = self
            .auto
            .build_constructed(Polarity::Pos, Constructor::Bool);
        let cmd = vec![Command::Push(Value::Bool(val))];
        Ok((
            Scheme {
                expr,
                env: SymbolMap::default(),
            },
            cmd,
        ))
    }

    fn check_if(
        &mut self,
        cond: &LocExpr,
        cons: &LocExpr,
        alt: &LocExpr,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let (cond, mut cmds) = self.check_expr(cond)?;
        let (cons, cons_cmds) = self.check_expr(cons)?;
        let (alt, alt_cmds) = self.check_expr(alt)?;

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

        cmds.push(Command::Test(alt_cmds.len() + 1));
        cmds.extend(alt_cmds);
        cmds.push(Command::Jump(cons_cmds.len()));
        cmds.extend(cons_cmds);

        Ok((
            Scheme {
                env,
                expr: pair.pos,
            },
            cmds,
        ))
    }

    fn check_record(&mut self, rec: &SymbolMap<LocExpr>) -> Result<(Scheme, Vec<Command>), Error> {
        let mut ids = rec
            .iter()
            .map(|(symbol, expr)| {
                let (expr, cmds) = self.check_expr(expr)?;
                Ok((*symbol, expr, cmds))
            })
            .collect::<Result<Vec<(Symbol, Scheme, Vec<Command>)>, Error>>()?;

        let cmds = ids.iter_mut().fold(
            vec![Command::Push(Value::Record(SymbolMap::default()))],
            |mut cmds, (symbol, _, val_cmds)| {
                cmds.append(val_cmds);
                cmds.push(Command::Set(*symbol));
                cmds
            },
        );

        let expr = self.build_record(
            Polarity::Pos,
            ids.iter().map(|(symbol, scheme, _)| (*symbol, scheme.expr)),
        );
        let env = self.meet_envs(ids.into_iter().map(|(_, scheme, _)| scheme.env));

        Ok((Scheme { expr, env }, cmds))
    }

    fn check_proj(
        &mut self,
        expr: &LocExpr,
        symbol: Symbol,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let (expr, mut cmds) = self.check_expr(expr)?;

        let pair = self.auto.build_var();
        let rec = self.build_record(Polarity::Neg, once((symbol, pair.neg)));
        self.auto.biunify(expr.expr, rec)?;

        cmds.push(Command::Get(symbol));

        Ok((
            Scheme {
                expr: pair.pos,
                env: expr.env,
            },
            cmds,
        ))
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
