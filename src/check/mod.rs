mod builtin;
mod ty;

use std::fmt;
use std::iter::once;

use codespan::FileId;
use mlsub::auto::{Automaton, StateId, StateSet};
use mlsub::Polarity;

use crate::check::ty::Constructor;
use crate::rt::{Command, FuncValue, Value};
use crate::syntax::{
    CallExpr, EnumExpr, Expr, FuncExpr, IfExpr, ImSymbolMap, LetExpr, ProjExpr, RecExpr, SourceMap,
    Spanned, Symbol, SymbolMap,
};

pub fn check(
    source: SourceMap,
    expr: &Spanned<Expr>,
) -> Result<FuncValue, Box<dyn std::error::Error>> {
    let mut ctx = Context::new(source);
    let (_, cmds) = ctx.check_expr(expr)?;
    Ok(FuncValue::new(cmds))
}

#[derive(Debug)]
enum Error {
    UndefinedVar(Symbol),
    TypeCheck,
    Import(String, Box<dyn std::error::Error>),
}

impl From<()> for Error {
    fn from((): ()) -> Self {
        Error::TypeCheck
    }
}

struct Context {
    auto: Automaton<Constructor>,
    vars: Vec<ImSymbolMap<StateId>>,
    source: SourceMap,
}

impl Context {
    fn new(source: SourceMap) -> Self {
        let mut ctx = Context {
            auto: Automaton::new(),
            vars: vec![ImSymbolMap::default()],
            source,
        };
        ctx.set_builtins();
        ctx
    }
}

impl Context {
    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Result<(StateId, Vec<Command>), Error> {
        match &expr.val {
            Expr::Var(symbol) => self.check_var(*symbol),
            Expr::Func(func) => self.check_func(func, None),
            Expr::Call(call_expr) => self.check_call(call_expr),
            Expr::Let(let_expr) => self.check_let(let_expr),
            Expr::Rec(rec) => self.check_rec(rec),
            Expr::Bool(val) => self.check_bool(*val),
            Expr::Int(val) => self.check_int(*val),
            Expr::String(val) => self.check_string(val.clone()),
            Expr::If(if_expr) => self.check_if(if_expr),
            Expr::Record(map) => self.check_record(map),
            Expr::Enum(enum_expr) => self.check_enum(enum_expr),
            Expr::Proj(proj) => self.check_proj(proj),
            Expr::Import(path) => self.check_import(path),
        }
    }

    fn check_var(&mut self, var: Symbol) -> Result<(StateId, Vec<Command>), Error> {
        let cmd = Command::Load { var };
        if let Some(id) = self.get_var(var) {
            return Ok((id, vec![cmd]));
        }

        Err(Error::UndefinedVar(var))
    }

    fn check_func(
        &mut self,
        func: &FuncExpr,
        name: Option<Symbol>,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let pair = self.auto.build_var();
        self.push_var(func.arg.val, pair.pos);
        let (body_ty, mut body_cmds) = self.check_expr(&func.body)?;
        self.pop_var();

        let func_ty = self.build_func(Polarity::Pos, pair.neg, body_ty);

        body_cmds.insert(0, Command::Store { var: func.arg.val });
        body_cmds.push(Command::End);
        let cmd = Command::Capture {
            name,
            cmds: body_cmds.into(),
        };

        Ok((func_ty, vec![cmd]))
    }

    fn check_call(&mut self, call: &CallExpr) -> Result<(StateId, Vec<Command>), Error> {
        let (func_ty, func_cmds) = self.check_expr(&call.func)?;
        let (arg_ty, arg_cmds) = self.check_expr(&call.arg)?;

        let pair = self.auto.build_var();
        let f = self.build_func(Polarity::Neg, arg_ty, pair.neg);
        self.auto.biunify(func_ty, f)?;

        let mut cmds = arg_cmds;
        cmds.extend(func_cmds);
        cmds.push(Command::Call);
        Ok((pair.pos, cmds))
    }

    fn check_let(&mut self, let_expr: &LetExpr) -> Result<(StateId, Vec<Command>), Error> {
        let (val_ty, val_cmds) = self.check_expr(&let_expr.val)?;

        self.push_var(let_expr.name.val, val_ty.clone());
        let (body_ty, body_cmds) = self.check_expr(&let_expr.body)?;
        self.pop_var();

        let mut cmds = val_cmds;
        cmds.push(Command::Store {
            var: let_expr.name.val,
        });
        cmds.extend(body_cmds);
        cmds.push(Command::End);

        Ok((body_ty, cmds))
    }

    fn check_rec(&mut self, rec: &RecExpr) -> Result<(StateId, Vec<Command>), Error> {
        let pair = self.auto.build_var();
        self.push_var(rec.name.val, pair.pos);
        let (func_ty, func_cmds) = self.check_func(&rec.func.val, Some(rec.name.val))?;
        self.pop_var();

        self.auto.biunify(func_ty, pair.neg)?;

        self.push_var(rec.name.val, func_ty.clone());
        let (body_ty, body_cmds) = self.check_expr(&rec.body)?;
        self.pop_var();

        let mut cmds = func_cmds;
        cmds.push(Command::Store { var: rec.name.val });
        cmds.extend(body_cmds);
        cmds.push(Command::End);

        Ok((body_ty, cmds))
    }

    fn check_if(&mut self, if_expr: &IfExpr) -> Result<(StateId, Vec<Command>), Error> {
        let (cond_ty, cond_cmds) = self.check_expr(&if_expr.cond)?;
        let (cons_ty, cons_cmds) = self.check_expr(&if_expr.cons)?;
        let (alt_ty, alt_cmds) = self.check_expr(&if_expr.alt)?;

        let pair = self.auto.build_var();
        let bool_ty = self.build_bool(Polarity::Neg);

        self.auto.biunify_all(
            [(cond_ty, bool_ty), (cons_ty, pair.neg), (alt_ty, pair.neg)]
                .iter()
                .copied(),
        )?;

        let mut cmds = cond_cmds;
        cmds.push(Command::Test {
            jump_offset: alt_cmds.len() + 1,
        });
        cmds.extend(alt_cmds);
        cmds.push(Command::Jump {
            jump_offset: cons_cmds.len(),
        });
        cmds.extend(cons_cmds);

        Ok((pair.pos, cmds))
    }

    fn check_record(
        &mut self,
        rec: &SymbolMap<Spanned<Expr>>,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let mut ids = rec
            .iter()
            .map(|(symbol, expr)| {
                let (expr, cmds) = self.check_expr(expr)?;
                Ok((*symbol, expr, cmds))
            })
            .collect::<Result<Vec<(Symbol, StateId, Vec<Command>)>, Error>>()?;

        let cmds = ids.iter_mut().fold(
            vec![Command::Push {
                value: Value::Record(ImSymbolMap::default()),
            }],
            |mut cmds, &mut (field, _, ref mut val_cmds)| {
                cmds.append(val_cmds);
                cmds.push(Command::Set { field });
                cmds
            },
        );

        let record_ty =
            self.build_record(Polarity::Pos, ids.iter().map(|&(field, id, _)| (field, id)));

        Ok((record_ty, cmds))
    }

    fn check_enum(&mut self, enum_expr: &EnumExpr) -> Result<(StateId, Vec<Command>), Error> {
        let (expr_ty, expr_cmds) = self.check_expr(&enum_expr.expr)?;

        let mut cmds = expr_cmds;
        cmds.push(Command::WrapEnum { tag: enum_expr.tag });

        let enum_ty = self.build_enum_variant(Polarity::Pos, enum_expr.tag, expr_ty);

        Ok((enum_ty, cmds))
    }

    fn check_proj(&mut self, proj: &ProjExpr) -> Result<(StateId, Vec<Command>), Error> {
        let (expr_ty, expr_cmds) = self.check_expr(&proj.expr)?;

        let pair = self.auto.build_var();
        let record = self.build_record(Polarity::Neg, once((proj.field.val, pair.neg)));
        self.auto.biunify(expr_ty, record)?;

        let mut cmds = expr_cmds;
        cmds.push(Command::Get {
            field: proj.field.val,
        });

        Ok((pair.pos, cmds))
    }

    fn check_import(&mut self, path: &str) -> Result<(StateId, Vec<Command>), Error> {
        let (file, expr) = match self.resolve_import(path) {
            Ok(expr) => expr,
            Err(err) => return Err(Error::Import(path.to_owned(), err)),
        };
        let result = self.check_expr(&expr);
        if file.is_some() {
            self.source.end_file();
        }
        result
    }

    fn resolve_import(
        &mut self,
        path: &str,
    ) -> Result<(Option<FileId>, Spanned<Expr>), Box<dyn std::error::Error>> {
        Ok(if path == "std" {
            (
                None,
                Expr::parse(include_str!("../../std/std.sl")).expect("syntax error in std"),
            )
        } else {
            let (file, expr) = self.source.parse_file(path)?;
            (Some(file), expr)
        })
    }

    fn check_bool(&mut self, val: bool) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_bool(Polarity::Pos);
        let cmd = vec![Command::Push {
            value: Value::Bool(val),
        }];
        Ok((ty, cmd))
    }

    fn check_int(&mut self, val: i64) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_int(Polarity::Pos);
        let cmd = vec![Command::Push {
            value: Value::Int(val),
        }];
        Ok((ty, cmd))
    }

    fn check_string(&mut self, val: String) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_string(Polarity::Pos);
        let cmd = vec![Command::Push {
            value: Value::String(val),
        }];
        Ok((ty, cmd))
    }

    fn push_var(&mut self, symbol: Symbol, ty: StateId) {
        let mut vars = self.vars.last().cloned().unwrap();
        vars.insert(symbol, ty);
        self.vars.push(vars);
    }

    fn set_var(&mut self, symbol: Symbol, ty: StateId) {
        self.vars.last_mut().unwrap().insert(symbol, ty);
    }

    fn get_var(&mut self, symbol: Symbol) -> Option<StateId> {
        self.vars.last().unwrap().get(&symbol).cloned()
    }

    fn pop_var(&mut self) {
        self.vars.pop();
    }

    fn build_bool(&mut self, pol: Polarity) -> StateId {
        self.auto.build_constructed(pol, Constructor::Bool)
    }

    fn build_int(&mut self, pol: Polarity) -> StateId {
        self.auto.build_constructed(pol, Constructor::Int)
    }

    fn build_string(&mut self, pol: Polarity) -> StateId {
        self.auto.build_constructed(pol, Constructor::String)
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

    fn build_enum_variant(&mut self, pol: Polarity, field: Symbol, expr: StateId) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::Enum(once((field, StateSet::new(expr))).collect()),
        )
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::TypeCheck => "inference error".fmt(f),
            Error::UndefinedVar(symbol) => write!(f, "undefined var `{}`", symbol),
            Error::Import(path, err) => write!(f, "failed to import module `{}`: `{}`", path, err),
        }
    }
}

impl std::error::Error for Error {}
