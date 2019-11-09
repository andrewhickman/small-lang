mod builtin;
mod scheme;
mod ty;

use std::collections::HashMap;
use std::iter::once;

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use mlsub::auto::{flow, Automaton, StateId, StateSet};
use mlsub::{BiunifyError, Polarity};

use crate::check::scheme::Scheme;
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
    TypeCheck(FileId, Span, BiunifyError<Constructor>),
}

struct Context<'a> {
    auto: Automaton<Constructor>,
    vars: Vec<ImSymbolMap<Scheme>>,
    files: Vec<FileId>,
    cache: HashMap<FileId, (Scheme, Vec<Command>)>,
    source: &'a mut SourceMap,
}

impl<'a> Context<'a> {
    fn new(source: &'a mut SourceMap, file: FileId) -> Self {
        let mut ctx = Context {
            auto: Automaton::new(),
            vars: vec![ImSymbolMap::default()],
            files: vec![file],
            cache: HashMap::default(),
            source,
        };
        ctx.set_builtins();
        ctx
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Result<(Scheme, Vec<Command>), Error> {
        match &expr.val {
            Expr::Null => self.check_null(expr.span),
            Expr::Var(symbol) => self.check_var(*symbol, expr.span),
            Expr::Func(func) => self.check_func(func, expr.span, None),
            Expr::Call(call_expr) => self.check_call(call_expr, expr.span),
            Expr::Let(let_expr) => self.check_let(let_expr),
            Expr::Rec(rec) => self.check_rec(rec, expr.span),
            Expr::Bool(val) => self.check_bool(*val, expr.span),
            Expr::Int(val) => self.check_int(*val, expr.span),
            Expr::String(val) => self.check_string(val.clone(), expr.span),
            Expr::If(if_expr) => self.check_if(if_expr, expr.span),
            Expr::Record(map) => self.check_record(map, expr.span),
            Expr::Enum(enum_expr) => self.check_enum(enum_expr, expr.span),
            Expr::Match(match_expr) => self.check_match(match_expr, expr.span),
            Expr::Proj(proj) => self.check_proj(proj, expr.span),
            Expr::Import(path) => self.check_import(path, expr.span),
        }
    }

    fn check_var(&mut self, var: Symbol, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let cmd = Command::Load { var };
        if let Some(scheme) = self.get_var(var) {
            return Ok((scheme, vec![cmd]));
        } else {
            Err(Error::UndefinedVar(self.file(), span, var))
        }
    }

    fn check_func(
        &mut self,
        func: &FuncExpr,
        span: Span,
        name: Option<Symbol>,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let arg_pair = self.auto.build_var();
        let ret_pair = self.auto.build_var();
        self.push_var(
            func.arg.val,
            Scheme::singleton(arg_pair.pos, (func.arg.val, arg_pair.neg)),
        );
        let (mut body_scheme, mut body_cmds) = self.check_expr(&func.body)?;
        self.pop_var();

        let domain_ty = body_scheme.remove_var(func.arg.val);
        let func_ty = self.build_func(Polarity::Pos, Some(span), arg_pair.neg, ret_pair.pos);
        self.auto
            .biunify_all(
                once((body_scheme.ty(), ret_pair.neg))
                    .chain(domain_ty.map(|ty| (arg_pair.pos, ty))),
            )
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        body_cmds.insert(0, Command::Store { var: func.arg.val });
        body_cmds.push(Command::End);
        let cmd = Command::Capture {
            name,
            cmds: body_cmds.into(),
        };

        Ok((body_scheme.with_ty(func_ty), vec![cmd]))
    }

    fn check_call(&mut self, call: &CallExpr, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let (func_scheme, func_cmds) = self.check_expr(&call.func)?;
        let (arg_scheme, arg_cmds) = self.check_expr(&call.arg)?;

        let arg_pair = self.auto.build_var();
        let ret_pair = self.auto.build_var();
        let func_ty = self.build_func(Polarity::Neg, Some(span), arg_pair.pos, ret_pair.neg);
        self.auto
            .biunify_all(
                [(func_scheme.ty(), func_ty), (arg_scheme.ty(), arg_pair.neg)]
                    .iter()
                    .copied(),
            )
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        let mut cmds = arg_cmds;
        cmds.extend(func_cmds);
        cmds.push(Command::Call);

        Ok((
            Scheme::join(&mut self.auto, ret_pair.pos, func_scheme, arg_scheme),
            cmds,
        ))
    }

    fn check_let(&mut self, let_expr: &LetExpr) -> Result<(Scheme, Vec<Command>), Error> {
        let (val_scheme, val_cmds) = self.check_expr(&let_expr.val)?;

        self.push_var(let_expr.name.val, val_scheme.clone());
        let (body_scheme, body_cmds) = self.check_expr(&let_expr.body)?;
        self.pop_var();

        let mut cmds = val_cmds;
        cmds.push(Command::Store {
            var: let_expr.name.val,
        });
        cmds.extend(body_cmds);
        cmds.push(Command::End);

        Ok((
            Scheme::join(&mut self.auto, body_scheme.ty(), val_scheme, body_scheme),
            cmds,
        ))
    }

    fn check_rec(&mut self, rec: &RecExpr, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let func_pair = self.auto.build_var();
        self.push_var(
            rec.name.val,
            Scheme::singleton(func_pair.pos, (rec.name.val, func_pair.neg)),
        );
        let (mut func_scheme, func_cmds) =
            self.check_func(&rec.func.val, rec.func.span, Some(rec.name.val))?;
        self.pop_var();

        let domain_ty = func_scheme.remove_var(rec.name.val);
        self.auto
            .biunify_all(
                once((func_scheme.ty(), func_pair.neg))
                    .chain(domain_ty.map(|ty| (func_pair.pos, ty))),
            )
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        self.push_var(rec.name.val, func_scheme.clone());
        let (body_scheme, body_cmds) = self.check_expr(&rec.body)?;
        self.pop_var();

        let mut cmds = func_cmds;
        cmds.push(Command::Store { var: rec.name.val });
        cmds.extend(body_cmds);
        cmds.push(Command::End);

        Ok((
            Scheme::join(&mut self.auto, body_scheme.ty(), func_scheme, body_scheme),
            cmds,
        ))
    }

    fn check_if(&mut self, if_expr: &IfExpr, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let (cond_scheme, cond_cmds) = self.check_expr(&if_expr.cond)?;
        let (cons_scheme, cons_cmds) = self.check_expr(&if_expr.cons)?;
        let (alt_scheme, alt_cmds) = self.check_expr(&if_expr.alt)?;

        let pair = self.auto.build_var();
        let bool_ty = self.build_bool(Polarity::Neg, Some(if_expr.cond.span));

        self.auto
            .biunify_all(
                [
                    (cond_scheme.ty(), bool_ty),
                    (cons_scheme.ty(), pair.neg),
                    (alt_scheme.ty(), pair.neg),
                ]
                .iter()
                .copied(),
            )
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        let mut cmds = cond_cmds;
        cmds.push(Command::Test {
            jump_offset: alt_cmds.len() + 1,
        });
        cmds.extend(alt_cmds);
        cmds.push(Command::Jump {
            jump_offset: cons_cmds.len(),
        });
        cmds.extend(cons_cmds);

        Ok((
            Scheme::join_all(
                &mut self.auto,
                pair.pos,
                once(cond_scheme)
                    .chain(once(cons_scheme))
                    .chain(once(alt_scheme)),
            ),
            cmds,
        ))
    }

    fn check_record(
        &mut self,
        rec: &SymbolMap<Spanned<Expr>>,
        span: Span,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let mut fields = rec
            .iter()
            .map(|(symbol, expr)| {
                let (scheme, cmds) = self.check_expr(expr)?;
                Ok((*symbol, self.auto.build_var(), scheme, cmds))
            })
            .collect::<Result<Vec<(Symbol, flow::Pair, Scheme, Vec<Command>)>, Error>>()?;

        let record_ty = self.build_record(
            Polarity::Pos,
            Some(span),
            fields
                .iter()
                .map(|&(field, val_pair, _, _)| (field, val_pair.pos)),
        );
        self.auto
            .biunify_all(
                fields
                    .iter()
                    .map(|&(_, val_pair, ref scheme, _)| (scheme.ty(), val_pair.neg)),
            )
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        let cmds = fields.iter_mut().fold(
            vec![Command::Push {
                value: Value::Record(ImSymbolMap::default()),
            }],
            |mut cmds, &mut (field, _, _, ref mut val_cmds)| {
                cmds.append(val_cmds);
                cmds.push(Command::Set { field });
                cmds
            },
        );

        Ok((
            Scheme::join_all(
                &mut self.auto,
                record_ty,
                fields.into_iter().map(|(_, _, scheme, _)| scheme),
            ),
            cmds,
        ))
    }

    fn check_enum(
        &mut self,
        enum_expr: &EnumExpr,
        span: Span,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let (expr_scheme, expr_cmds) = match &enum_expr.expr {
            Some(expr) => self.check_expr(expr)?,
            None => self.check_null(span)?,
        };

        let mut cmds = expr_cmds;
        cmds.push(Command::WrapEnum {
            tag: enum_expr.tag.val,
        });

        let val_pair = self.auto.build_var();
        let enum_ty =
            self.build_enum_variant(Polarity::Pos, Some(span), enum_expr.tag.val, val_pair.pos);
        self.auto
            .biunify(expr_scheme.ty(), val_pair.neg)
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        Ok((expr_scheme.with_ty(enum_ty), cmds))
    }

    fn check_match(
        &mut self,
        match_expr: &MatchExpr,
        span: Span,
    ) -> Result<(Scheme, Vec<Command>), Error> {
        let (expr_scheme, expr_cmds) = self.check_expr(&match_expr.expr)?;

        let result_pair = self.auto.build_var();

        let mut cases = match_expr
            .cases
            .iter()
            .map(|(&tag, case)| {
                let case_pair = self.auto.build_var();

                if let Some(name) = case.val.name {
                    self.push_var(name.val, Scheme::singleton(case_pair.pos, (name.val, case_pair.neg)));
                }
                let (mut case_scheme, mut case_cmds) = self.check_expr(&case.val.expr)?;
                let val_ty = if let Some(name) = case.val.name {
                    self.pop_var();

                    case_cmds.insert(0, Command::Store { var: name.val });
                    case_cmds.push(Command::End);

                    case_scheme.remove_var(name.val)
                } else {
                    case_cmds.insert(0, Command::Pop);
                    None
                };

                Ok((tag, case_pair, case_scheme, val_ty, case_cmds))
            })
            .collect::<Result<Vec<(Symbol, flow::Pair, Scheme, Option<StateId>, Vec<Command>)>, Error>>()?;

        let enum_ty = self.build_enum(
            Polarity::Neg,
            Some(span),
            cases
                .iter()
                .map(|&(tag, val_pair, _, _, _)| (tag, val_pair.neg)),
        );

        self.auto
            .biunify(expr_scheme.ty(), enum_ty)
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;
        self.auto
            .biunify_all(
                cases
                    .iter()
                    .flat_map(|(_, val_pair, out_scheme, val_ty, _)| {
                        once((out_scheme.ty(), result_pair.neg))
                            .chain(val_ty.map(|ty| (val_pair.pos, ty)))
                    }),
            )
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        let (jump_offsets, mut cmds_total_len) = cases.iter().fold(
            (ImSymbolMap::default(), 0),
            |(jump_offsets, cmds_len), &(tag, _, _, _, ref val_cmds)| {
                (
                    jump_offsets.update(tag, cmds_len),
                    cmds_len + val_cmds.len() + 1,
                )
            },
        );

        let mut cmds = expr_cmds;
        cmds.push(Command::Match { jump_offsets });
        cmds_total_len += cmds.len();

        for &mut (_, _, _, _, ref mut val_cmds) in &mut cases {
            cmds.append(val_cmds);
            cmds.push(Command::Jump {
                jump_offset: cmds_total_len - cmds.len() - 1,
            });
        }

        Ok((
            Scheme::join_all(
                &mut self.auto,
                result_pair.pos,
                once(expr_scheme).chain(cases.into_iter().map(|(_, _, scheme, _, _)| scheme)),
            ),
            cmds,
        ))
    }

    fn check_proj(&mut self, proj: &ProjExpr, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let (expr_scheme, expr_cmds) = self.check_expr(&proj.expr)?;

        let field_pair = self.auto.build_var();
        let record_ty = self.build_record(
            Polarity::Neg,
            Some(span),
            once((proj.field.val, field_pair.neg)),
        );
        self.auto
            .biunify(expr_scheme.ty(), record_ty)
            .map_err(|err| Error::TypeCheck(self.file(), span, err))?;

        let mut cmds = expr_cmds;
        cmds.push(Command::Get {
            field: proj.field.val,
        });

        Ok((expr_scheme.with_ty(field_pair.pos), cmds))
    }

    fn check_import(&mut self, path: &str, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        match self.resolve_import(path) {
            Ok(SourceCacheResult::Miss(file, expr)) => {
                self.files.push(file);
                let (ty, cmds) = self.check_expr(&expr)?;
                self.files.pop();
                self.source.end_file();

                assert!(self
                    .cache
                    .insert(file, (ty.clone(), cmds.clone()))
                    .is_none());
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

    fn check_null(&mut self, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let ty = self.build_null(Polarity::Pos, Some(span));
        let cmd = vec![Command::Push { value: Value::Null }];
        Ok((Scheme::empty(ty), cmd))
    }

    fn check_bool(&mut self, val: bool, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let ty = self.build_bool(Polarity::Pos, Some(span));
        let cmd = vec![Command::Push {
            value: Value::Bool(val),
        }];
        Ok((Scheme::empty(ty), cmd))
    }

    fn check_int(&mut self, val: i64, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let ty = self.build_int(Polarity::Pos, Some(span));
        let cmd = vec![Command::Push {
            value: Value::Int(val),
        }];
        Ok((Scheme::empty(ty), cmd))
    }

    fn check_string(&mut self, val: String, span: Span) -> Result<(Scheme, Vec<Command>), Error> {
        let ty = self.build_string(Polarity::Pos, Some(span));
        let cmd = vec![Command::Push {
            value: Value::String(val),
        }];
        Ok((Scheme::empty(ty), cmd))
    }

    fn file(&self) -> FileId {
        *self.files.last().unwrap()
    }

    fn push_var(&mut self, symbol: Symbol, ty: Scheme) {
        let mut vars = self.vars.last().cloned().unwrap();
        vars.insert(symbol, ty);
        self.vars.push(vars);
    }

    fn set_var(&mut self, symbol: Symbol, scheme: Scheme) {
        self.vars.last_mut().unwrap().insert(symbol, scheme);
    }

    fn get_var(&mut self, symbol: Symbol) -> Option<Scheme> {
        match self.vars.last().unwrap().get(&symbol) {
            Some(scheme) => Some(scheme.deep_clone(&mut self.auto)),
            None => None,
        }
    }

    fn pop_var(&mut self) {
        self.vars.pop();
    }

    fn build_null(&mut self, pol: Polarity, span: Option<Span>) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Null, span.map(|span| (self.file(), span))),
        )
    }

    fn build_bool(&mut self, pol: Polarity, span: Option<Span>) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Bool, span.map(|span| (self.file(), span))),
        )
    }

    fn build_int(&mut self, pol: Polarity, span: Option<Span>) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(ConstructorKind::Int, span.map(|span| (self.file(), span))),
        )
    }

    fn build_string(&mut self, pol: Polarity, span: Option<Span>) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::String,
                span.map(|span| (self.file(), span)),
            ),
        )
    }

    fn build_func(
        &mut self,
        pol: Polarity,
        span: Option<Span>,
        dom: StateId,
        range: StateId,
    ) -> StateId {
        self.auto.build_constructed(
            pol,
            Constructor::new(
                ConstructorKind::Func(StateSet::new(dom), StateSet::new(range)),
                span.map(|span| (self.file(), span)),
            ),
        )
    }

    fn build_record<I>(&mut self, pol: Polarity, span: Option<Span>, iter: I) -> StateId
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
                span.map(|span| (self.file(), span)),
            ),
        )
    }

    fn build_enum<I>(&mut self, pol: Polarity, span: Option<Span>, iter: I) -> StateId
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
                span.map(|span| (self.file(), span)),
            ),
        )
    }

    fn build_enum_variant(
        &mut self,
        pol: Polarity,
        span: Option<Span>,
        field: Symbol,
        expr: StateId,
    ) -> StateId {
        self.build_enum(pol, span, once((field, expr)))
    }
}

impl Error {
    fn into_diagnostics(self) -> Vec<Diagnostic> {
        match self {
            Error::TypeCheck(expr_file, expr_span, err) => {
                let actual_span = err.constraint.0.spans().get(0);
                let expected_span = err.constraint.1.spans().get(0);

                let primary_span = match (actual_span, expected_span) {
                    (Some(&actual_span), _) => actual_span,
                    (_, Some(&expected_span)) => expected_span,
                    (None, None) => (expr_file, expr_span),
                };

                let mut diagnostic = Diagnostic::new_error(
                    format!(
                        "expected `{}` but found `{}`",
                        err.constraint.1, err.constraint.0
                    ),
                    Label::new(primary_span.0, primary_span.1, "found here"),
                );
                if actual_span.is_some() {
                    diagnostic
                        .secondary_labels
                        .extend(err.constraint.1.spans().iter().map(|&(file, span)| {
                            Label::new(file, span, "expected type inferred here")
                        }));
                }
                diagnostic
                    .secondary_labels
                    .extend(err.stack.iter().filter_map(|(label, found, _)| {
                        found.spans().get(0).map(|&(file, span)| {
                            Label::new(
                                file,
                                span,
                                format!("in {} of type `{}` here...", label, found),
                            )
                        })
                    }));
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
