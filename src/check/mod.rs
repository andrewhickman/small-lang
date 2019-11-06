mod builtin;
mod ty;

use std::collections::HashMap;
use std::iter::once;

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use mlsub::auto::{Automaton, StateId, StateSet};
use mlsub::{BiunifyError, Polarity};

use crate::check::ty::{Constructor, ConstructorKind};
use crate::rt::{Command, FuncValue, Value};
use crate::syntax::{
    CallExpr, EnumExpr, Expr, FuncExpr, IfExpr, ImSymbolMap, LetExpr, MatchExpr, ProjExpr, RecExpr,
    SourceCacheResult, SourceMap, Spanned, Symbol, SymbolMap,
};
use crate::ErrorData;

pub fn check(
    source: &mut SourceMap,
    file: FileId,
    expr: &Spanned<Expr>,
) -> Result<FuncValue, Vec<Diagnostic>> {
    let mut ctx = Context::new(source, file);
    let (_, cmds) = ctx.check_expr(expr).map_err(Error::into_diagnostics)?;
    Ok(FuncValue::new(cmds))
}

#[derive(Debug)]
enum Error {
    UndefinedVar(FileId, Span, Symbol),
    Import(FileId, Span, String, ErrorData),
    TypeCheck(BiunifyError<Constructor>),
}

impl From<BiunifyError<Constructor>> for Error {
    fn from(err: BiunifyError<Constructor>) -> Self {
        Error::TypeCheck(err)
    }
}

struct Context<'a> {
    auto: Automaton<Constructor>,
    vars: Vec<ImSymbolMap<StateId>>,
    cache: HashMap<FileId, (StateId, Vec<Command>)>,
    files: Vec<FileId>,
    source: &'a mut SourceMap,
}

impl<'a> Context<'a> {
    fn new(source: &'a mut SourceMap, file: FileId) -> Self {
        let mut ctx = Context {
            auto: Automaton::new(),
            vars: vec![ImSymbolMap::default()],
            cache: HashMap::new(),
            files: vec![file],
            source,
        };
        ctx.set_builtins();
        ctx
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Result<(StateId, Vec<Command>), Error> {
        match &expr.val {
            Expr::Null => self.check_null(expr.span),
            Expr::Var(symbol) => self.check_var(*symbol, expr.span),
            Expr::Func(func) => self.check_func(func, expr.span, None),
            Expr::Call(call_expr) => self.check_call(call_expr, expr.span),
            Expr::Let(let_expr) => self.check_let(let_expr),
            Expr::Rec(rec) => self.check_rec(rec),
            Expr::Bool(val) => self.check_bool(*val, expr.span),
            Expr::Int(val) => self.check_int(*val, expr.span),
            Expr::String(val) => self.check_string(val.clone(), expr.span),
            Expr::If(if_expr) => self.check_if(if_expr),
            Expr::Record(map) => self.check_record(map, expr.span),
            Expr::Enum(enum_expr) => self.check_enum(enum_expr, expr.span),
            Expr::Match(match_expr) => self.check_match(match_expr, expr.span),
            Expr::Proj(proj) => self.check_proj(proj, expr.span),
            Expr::Import(path) => self.check_import(path, expr.span),
        }
    }

    fn check_var(&mut self, var: Symbol, span: Span) -> Result<(StateId, Vec<Command>), Error> {
        let cmd = Command::Load { var };
        if let Some(id) = self.get_var(var) {
            Ok((id, vec![cmd]))
        } else {
            Err(Error::UndefinedVar(self.file(), span, var))
        }
    }

    fn check_func(
        &mut self,
        func: &FuncExpr,
        span: Span,
        name: Option<Symbol>,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let pair = self.auto.build_var();
        self.push_var(func.arg.val, pair.pos);
        let (body_ty, mut body_cmds) = self.check_expr(&func.body)?;
        self.pop_var();

        let func_ty = self.build_func(Polarity::Pos, span, pair.neg, body_ty);

        body_cmds.insert(0, Command::Store { var: func.arg.val });
        body_cmds.push(Command::End);
        let cmd = Command::Capture {
            name,
            cmds: body_cmds.into(),
        };

        Ok((func_ty, vec![cmd]))
    }

    fn check_call(
        &mut self,
        call: &CallExpr,
        span: Span,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let (func_ty, func_cmds) = self.check_expr(&call.func)?;
        let (arg_ty, arg_cmds) = self.check_expr(&call.arg)?;

        let pair = self.auto.build_var();
        let f = self.build_func(Polarity::Neg, span, arg_ty, pair.neg);
        self.auto.biunify(func_ty, f)?;

        let mut cmds = arg_cmds;
        cmds.extend(func_cmds);
        cmds.push(Command::Call);
        Ok((pair.pos, cmds))
    }

    fn check_let(&mut self, let_expr: &LetExpr) -> Result<(StateId, Vec<Command>), Error> {
        let (val_ty, val_cmds) = self.check_expr(&let_expr.val)?;

        self.push_var(let_expr.name.val, val_ty);
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
        let (func_ty, func_cmds) =
            self.check_func(&rec.func.val, rec.func.span, Some(rec.name.val))?;
        self.pop_var();

        self.auto.biunify(func_ty, pair.neg)?;

        self.push_var(rec.name.val, func_ty);
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
        let bool_ty = self.build_bool(Polarity::Neg, if_expr.cond.span);

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
        span: Span,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let fields = rec
            .iter()
            .map(|(symbol, expr)| {
                let (expr, cmds) = self.check_expr(expr)?;
                Ok((*symbol, expr, cmds))
            })
            .collect::<Result<Vec<(Symbol, StateId, Vec<Command>)>, Error>>()?;

        let record_ty = self.build_record(
            Polarity::Pos,
            span,
            fields.iter().map(|&(field, id, _)| (field, id)),
        );

        let cmds = fields.into_iter().fold(
            vec![Command::Push {
                value: Value::Record(ImSymbolMap::default()),
            }],
            |mut cmds, (field, _, val_cmds)| {
                cmds.extend(val_cmds);
                cmds.push(Command::Set { field });
                cmds
            },
        );

        Ok((record_ty, cmds))
    }

    fn check_enum(
        &mut self,
        enum_expr: &EnumExpr,
        span: Span,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let (expr_ty, expr_cmds) = match &enum_expr.expr {
            Some(expr) => self.check_expr(expr)?,
            None => self.check_null(span)?,
        };

        let mut cmds = expr_cmds;
        cmds.push(Command::WrapEnum {
            tag: enum_expr.tag.val,
        });

        let enum_ty = self.build_enum_variant(Polarity::Pos, span, enum_expr.tag.val, expr_ty);

        Ok((enum_ty, cmds))
    }

    fn check_match(
        &mut self,
        match_expr: &MatchExpr,
        span: Span,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let (expr_ty, expr_cmds) = self.check_expr(&match_expr.expr)?;

        let result_pair = self.auto.build_var();

        let cases = match_expr
            .cases
            .iter()
            .map(|(&tag, case)| {
                let case_var = self.auto.build_var();

                if let Some(name) = case.val.name {
                    self.push_var(name.val, case_var.pos);
                }
                let (case_ty, mut case_cmds) = self.check_expr(&case.val.expr)?;
                if let Some(name) = case.val.name {
                    self.pop_var();

                    case_cmds.insert(0, Command::Store { var: name.val });
                    case_cmds.push(Command::End);
                } else {
                    case_cmds.insert(0, Command::Pop);
                }

                Ok((tag, case_var.neg, case_ty, case_cmds))
            })
            .collect::<Result<Vec<(Symbol, StateId, StateId, Vec<Command>)>, Error>>()?;
        let enum_ty = self.build_enum(
            Polarity::Neg,
            span,
            cases.iter().map(|&(tag, in_ty, _, _)| (tag, in_ty)),
        );

        self.auto.biunify(expr_ty, enum_ty)?;
        self.auto.biunify_all(
            cases
                .iter()
                .map(|&(_, _, out_ty, _)| (out_ty, result_pair.neg)),
        )?;

        let (jump_offsets, mut cmds_total_len) = cases.iter().fold(
            (ImSymbolMap::default(), 0),
            |(jump_offsets, cmds_len), &(tag, _, _, ref val_cmds)| {
                (
                    jump_offsets.update(tag, cmds_len),
                    cmds_len + val_cmds.len() + 1,
                )
            },
        );

        let mut cmds = expr_cmds;
        cmds.push(Command::Match { jump_offsets });
        cmds_total_len += cmds.len();

        for (_, _, _, val_cmds) in cases {
            cmds.extend(val_cmds);
            cmds.push(Command::Jump {
                jump_offset: cmds_total_len - cmds.len() - 1,
            });
        }

        Ok((result_pair.pos, cmds))
    }

    fn check_proj(
        &mut self,
        proj: &ProjExpr,
        span: Span,
    ) -> Result<(StateId, Vec<Command>), Error> {
        let (expr_ty, expr_cmds) = self.check_expr(&proj.expr)?;

        let pair = self.auto.build_var();
        let record = self.build_record(Polarity::Neg, span, once((proj.field.val, pair.neg)));
        self.auto.biunify(expr_ty, record)?;

        let mut cmds = expr_cmds;
        cmds.push(Command::Get {
            field: proj.field.val,
        });

        Ok((pair.pos, cmds))
    }

    fn check_import(&mut self, path: &str, span: Span) -> Result<(StateId, Vec<Command>), Error> {
        match self.resolve_import(path) {
            Ok(SourceCacheResult::Miss(file, expr)) => {
                let (ty, cmds) = self.check_expr(&expr)?;
                self.source.end_file();

                assert!(self.cache.insert(file, (ty, cmds.clone())).is_none());
                Ok((ty, cmds))
            }
            Ok(SourceCacheResult::Hit(file)) => match self.cache.get(&file) {
                Some(result) => Ok(result.clone()),
                None => Err(Error::Import(
                    self.file(),
                    span,
                    path.to_owned(),
                    ErrorData::Basic("recursive import detected".into()),
                )),
            },
            Err(err) => Err(Error::Import(self.file(), span, path.to_owned(), err)),
        }
    }

    fn resolve_import(&mut self, path: &str) -> Result<SourceCacheResult, ErrorData> {
        match path {
            "cmp" => self
                .source
                .parse_input("cmp", include_str!("../../std/cmp.sl")),
            "iter" => self
                .source
                .parse_input("iter", include_str!("../../std/iter.sl")),
            "math" => self
                .source
                .parse_input("math", include_str!("../../std/math.sl")),
            "list" => self
                .source
                .parse_input("list", include_str!("../../std/list.sl")),
            path => self.source.parse_file(path),
        }
    }

    fn check_null(&mut self, span: Span) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_null(Polarity::Pos, span);
        let cmd = vec![Command::Push { value: Value::Null }];
        Ok((ty, cmd))
    }

    fn check_bool(&mut self, val: bool, span: Span) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_bool(Polarity::Pos, span);
        let cmd = vec![Command::Push {
            value: Value::Bool(val),
        }];
        Ok((ty, cmd))
    }

    fn check_int(&mut self, val: i64, span: Span) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_int(Polarity::Pos, span);
        let cmd = vec![Command::Push {
            value: Value::Int(val),
        }];
        Ok((ty, cmd))
    }

    fn check_string(&mut self, val: String, span: Span) -> Result<(StateId, Vec<Command>), Error> {
        let ty = self.build_string(Polarity::Pos, span);
        let cmd = vec![Command::Push {
            value: Value::String(val),
        }];
        Ok((ty, cmd))
    }

    fn file(&self) -> FileId {
        *self.files.last().unwrap()
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

    fn build_null(&mut self, pol: Polarity, span: Span) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Null, self.file(), span),
        )
    }

    fn build_bool(&mut self, pol: Polarity, span: Span) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Bool, self.file(), span),
        )
    }

    fn build_int(&mut self, pol: Polarity, span: Span) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Int, self.file(), span),
        )
    }

    fn build_string(&mut self, pol: Polarity, span: Span) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::String, self.file(), span),
        )
    }

    fn build_func(&mut self, pol: Polarity, span: Span, dom: StateId, range: StateId) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Func(StateSet::new(dom), StateSet::new(range)),
                self.file(),
                span,
            ),
        )
    }

    fn build_record<I>(&mut self, pol: Polarity, span: Span, iter: I) -> StateId
    where
        I: IntoIterator<Item = (Symbol, StateId)>,
    {
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Record(
                    iter.into_iter()
                        .map(|(sym, id)| (sym, StateSet::new(id)))
                        .collect(),
                ),
                self.file(),
                span,
            ),
        )
    }

    fn build_enum<I>(&mut self, pol: Polarity, span: Span, iter: I) -> StateId
    where
        I: IntoIterator<Item = (Symbol, StateId)>,
    {
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Enum(
                    iter.into_iter()
                        .map(|(tag, ty)| (tag, StateSet::new(ty)))
                        .collect(),
                ),
                self.file(),
                span,
            ),
        )
    }

    fn build_enum_variant(
        &mut self,
        pol: Polarity,
        span: Span,
        field: Symbol,
        expr: StateId,
    ) -> StateId {
        self.build_enum(pol, span, once((field, expr)))
    }
}

impl Error {
    fn into_diagnostics(self) -> Vec<Diagnostic> {
        match self {
            Error::TypeCheck(err) => {
                let (file, span) = err.constraint.0.spans()[0];
                let diagnostic =
                    Diagnostic::new_error(
                        format!(
                            "expected `{}` but found `{}`",
                            err.constraint.1, err.constraint.0
                        ),
                        Label::new(file, span, "found here"),
                    )
                    .with_secondary_labels(
                        err.constraint.1.spans().iter().map(|&(file, span)| {
                            Label::new(file, span, "expected type inferred here")
                        }),
                    );
                vec![diagnostic]
            }
            Error::UndefinedVar(file, span, symbol) => vec![Diagnostic::new_error(
                format!("undefined variable `{}`", symbol),
                Label::new(file, span, "used here"),
            )],
            Error::Import(file, span, path, data) => match data {
                ErrorData::Basic(err) => vec![Diagnostic::new_error(
                    format!("failed to import module from `{}`: {}", path, err),
                    Label::new(file, span, "imported here"),
                )],
                ErrorData::Diagnostics(diagnostics) => diagnostics,
            },
        }
    }
}
