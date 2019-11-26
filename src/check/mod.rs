pub mod ir;

mod builtin;
mod scheme;
mod ty;

use std::collections::HashMap;
use std::iter::once;
use std::rc::Rc;

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use mlsub::auto::Automaton;
use mlsub::{BiunifyError, Polarity};

use crate::check::scheme::{ReducedScheme, Scheme};
use crate::check::ty::{Constructor, NumberConstructor};
use crate::rt::{NumberValue, Value};
use crate::syntax::{ast, ImSymbolMap, SourceCacheResult, SourceMap, Symbol, SymbolMap};
use crate::ErrorData;

pub fn check(
    source: &mut SourceMap,
    file: FileId,
    expr: &ast::Spanned<ast::Expr>,
) -> Result<ir::Expr, Vec<Diagnostic>> {
    let mut ctx = Context::new(source);
    ctx.check_expr(expr, file).map_err(Error::into_diagnostics)
}

type FileSpan = (FileId, Span);

#[derive(Debug)]
enum Error {
    UndefinedVar(FileSpan, Symbol),
    Import(FileSpan, String, ErrorData),
    TypeCheck(FileSpan, Box<BiunifyError<Constructor>>),
}

struct Context<'a> {
    auto: Automaton<Constructor>,
    vars: Vec<ImSymbolMap<ReducedScheme>>,
    cache: HashMap<FileId, Rc<ir::Expr>>,
    source: &'a mut SourceMap,
    capabilities: ty::Capabilities,
}

impl<'a> Context<'a> {
    fn new(source: &'a mut SourceMap) -> Self {
        let mut ctx = Context {
            auto: Automaton::new(),
            vars: vec![ImSymbolMap::default()],
            cache: HashMap::default(),
            capabilities: ty::Capabilities::default(),
            source,
        };
        ctx.set_builtins();
        ctx
    }

    fn check_expr(
        &mut self,
        expr: &ast::Spanned<ast::Expr>,
        file: FileId,
    ) -> Result<ir::Expr, Error> {
        let span = (file, expr.span);
        match &expr.val {
            ast::Expr::Null => self.check_null(span),
            ast::Expr::Var(symbol) => self.check_var(*symbol, span),
            ast::Expr::Func(func) => self.check_func(func, span, None),
            ast::Expr::Call(call_expr) => self.check_call(call_expr, span),
            ast::Expr::Let(let_expr) => self.check_let(let_expr, span),
            ast::Expr::Rec(rec) => self.check_rec(rec, span),
            ast::Expr::Bool(val) => self.check_bool(*val, span),
            ast::Expr::Int(val) => self.check_int(*val, span),
            ast::Expr::Float(val) => self.check_float(*val, span),
            ast::Expr::String(val) => self.check_string(val.clone(), span),
            ast::Expr::If(if_expr) => self.check_if(if_expr, span),
            ast::Expr::Record(map) => self.check_record(map, span),
            ast::Expr::Enum(enum_expr) => self.check_enum(enum_expr, span),
            ast::Expr::Match(match_expr) => self.check_match(match_expr, span),
            ast::Expr::Proj(proj) => self.check_proj(proj, span),
            ast::Expr::Import(path) => self.check_import(path, span),
        }
    }

    fn check_var(&mut self, var: Symbol, span: FileSpan) -> Result<ir::Expr, Error> {
        if let Some(scheme) = self.get_var(var) {
            Ok(ir::Expr {
                scheme,
                kind: ir::ExprKind::Var(var),
            })
        } else {
            Err(Error::UndefinedVar(span, var))
        }
    }

    fn check_func(
        &mut self,
        func: &ast::FuncExpr,
        span: FileSpan,
        rec_name: Option<Symbol>,
    ) -> Result<ir::Expr, Error> {
        let arg_pair = self.auto.build_var();
        let ret_pair = self.auto.build_var();
        self.push_var(func.arg.val, Scheme::from_var(func.arg.val, arg_pair));
        let body = self.check_expr(&func.body, span.0)?;
        self.pop_var();

        let (scheme, domain_ty) = body.scheme.without_var(func.arg.val);
        let func_ty = self.build_func(Polarity::Pos, Some(span), arg_pair.neg, ret_pair.pos);
        self.auto
            .biunify_all(
                once((body.scheme.ty(), ret_pair.neg))
                    .chain(domain_ty.map(|ty| (arg_pair.pos, ty))),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(ir::Expr {
            scheme: scheme.with_ty(func_ty),
            kind: ir::ExprKind::Func(Box::new(ir::Func {
                arg: func.arg.val,
                body,
                rec_name,
            })),
        })
    }

    fn check_call(&mut self, call: &ast::CallExpr, span: FileSpan) -> Result<ir::Expr, Error> {
        let func = self.check_expr(&call.func, span.0)?;
        let arg = self.check_expr(&call.arg, span.0)?;

        let arg_pair = self.auto.build_var();
        let ret_pair = self.auto.build_var();
        let func_ty = self.build_func(Polarity::Neg, Some(span), arg_pair.pos, ret_pair.neg);
        self.auto
            .biunify_all(
                [(func.scheme.ty(), func_ty), (arg.scheme.ty(), arg_pair.neg)]
                    .iter()
                    .copied(),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(ir::Expr {
            scheme: Scheme::join(&mut self.auto, ret_pair.pos, &func.scheme, &arg.scheme),
            kind: ir::ExprKind::Call(Box::new(ir::Call { arg, func })),
        })
    }

    fn check_let(&mut self, let_expr: &ast::LetExpr, span: FileSpan) -> Result<ir::Expr, Error> {
        let val = self.check_expr(&let_expr.val, span.0)?;

        self.push_var(let_expr.name.val, val.scheme.clone());
        let body = self.check_expr(&let_expr.body, span.0)?;
        self.pop_var();

        Ok(ir::Expr {
            scheme: Scheme::join(&mut self.auto, body.scheme.ty(), &val.scheme, &body.scheme),
            kind: ir::ExprKind::Let(Box::new(ir::Let {
                name: let_expr.name.val,
                val,
                body,
            })),
        })
    }

    fn check_rec(&mut self, rec: &ast::RecExpr, span: FileSpan) -> Result<ir::Expr, Error> {
        let func_pair = self.auto.build_var();
        self.push_var(rec.name.val, Scheme::from_var(rec.name.val, func_pair));
        let func = self.check_func(&rec.func.val, (span.0, rec.func.span), Some(rec.name.val))?;
        self.pop_var();

        let (scheme, func_ty) = func.scheme.without_var(rec.name.val);
        self.auto
            .biunify_all(
                once((func.scheme.ty(), func_pair.neg))
                    .chain(func_ty.map(|ty| (func_pair.pos, ty))),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        self.push_var(rec.name.val, func.scheme.clone());
        let body = self.check_expr(&rec.body, span.0)?;
        self.pop_var();

        Ok(ir::Expr {
            scheme: Scheme::join(&mut self.auto, body.scheme.ty(), &scheme, &body.scheme),
            kind: ir::ExprKind::Let(Box::new(ir::Let {
                name: rec.name.val,
                val: func,
                body,
            })),
        })
    }

    fn check_if(&mut self, if_expr: &ast::IfExpr, span: FileSpan) -> Result<ir::Expr, Error> {
        let cond = self.check_expr(&if_expr.cond, span.0)?;
        let cons = self.check_expr(&if_expr.cons, span.0)?;
        let alt = self.check_expr(&if_expr.alt, span.0)?;

        let pair = self.auto.build_var();
        let bool_ty = self.build_bool(Polarity::Neg, Some((span.0, if_expr.cond.span)));

        self.auto
            .biunify_all(
                [
                    (cond.scheme.ty(), bool_ty),
                    (cons.scheme.ty(), pair.neg),
                    (alt.scheme.ty(), pair.neg),
                ]
                .iter()
                .copied(),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(ir::Expr {
            scheme: Scheme::join_all(
                &mut self.auto,
                pair.pos,
                once(&cond.scheme)
                    .chain(once(&cons.scheme))
                    .chain(once(&alt.scheme)),
            ),
            kind: ir::ExprKind::If(Box::new(ir::If { cond, cons, alt })),
        })
    }

    fn check_record(
        &mut self,
        rec: &SymbolMap<ast::Spanned<ast::Expr>>,
        span: FileSpan,
    ) -> Result<ir::Expr, Error> {
        let fields = rec
            .iter()
            .map(|(symbol, expr)| {
                Ok((
                    *symbol,
                    ir::RecordEntry {
                        expr: self.check_expr(expr, span.0)?,
                        pair: self.auto.build_var(),
                    },
                ))
            })
            .collect::<Result<SymbolMap<_>, Error>>()?;

        let record_ty = self.build_record(
            Polarity::Pos,
            Some(span),
            fields.iter().map(|(&field, entry)| (field, entry.pair.pos)),
        );
        self.auto
            .biunify_all(
                fields
                    .values()
                    .map(|entry| (entry.expr.scheme.ty(), entry.pair.neg)),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(ir::Expr {
            scheme: Scheme::join_all(
                &mut self.auto,
                record_ty,
                fields.values().map(|entry| &entry.expr.scheme),
            ),
            kind: ir::ExprKind::Record(fields),
        })
    }

    fn check_enum(&mut self, enum_expr: &ast::EnumExpr, span: FileSpan) -> Result<ir::Expr, Error> {
        let expr = match &enum_expr.expr {
            Some(expr) => self.check_expr(expr, span.0)?,
            None => self.check_null(span)?,
        };

        let val_pair = self.auto.build_var();
        let enum_ty =
            self.build_enum_variant(Polarity::Pos, Some(span), enum_expr.tag.val, val_pair.pos);
        self.auto
            .biunify(expr.scheme.ty(), val_pair.neg)
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        // Ok((, cmds))
        Ok(ir::Expr {
            scheme: expr.scheme.clone().with_ty(enum_ty),
            kind: ir::ExprKind::Enum(Box::new(ir::Enum {
                tag: enum_expr.tag.val,
                expr,
            })),
        })
    }

    fn check_match(
        &mut self,
        match_expr: &ast::MatchExpr,
        span: FileSpan,
    ) -> Result<ir::Expr, Error> {
        let expr = self.check_expr(&match_expr.expr, span.0)?;

        let result_pair = self.auto.build_var();

        let cases = match_expr
            .cases
            .iter()
            .map(|(&tag, case)| {
                let case_pair = self.auto.build_var();
                let name = case.val.name.map(|name| name.val);

                if let Some(name) = name {
                    self.push_var(name, Scheme::from_var(name, case_pair));
                }
                let case_expr = self.check_expr(&case.val.expr, span.0)?;
                let (scheme, val_ty) = if let Some(name) = name {
                    self.pop_var();
                    case_expr.scheme.without_var(name)
                } else {
                    (case_expr.scheme.clone(), None)
                };

                Ok((
                    tag,
                    ir::MatchCase {
                        expr: case_expr,
                        name,
                        scheme,
                        val_pair: case_pair,
                        val_ty,
                    },
                ))
            })
            .collect::<Result<SymbolMap<_>, Error>>()?;

        let enum_ty = self.build_enum(
            Polarity::Neg,
            Some(span),
            cases.iter().map(|(&tag, case)| (tag, case.val_pair.neg)),
        );

        self.auto
            .biunify(expr.scheme.ty(), enum_ty)
            .map_err(|err| Error::TypeCheck(span, err.into()))?;
        self.auto
            .biunify_all(cases.values().flat_map(|case| {
                once((case.scheme.ty(), result_pair.neg))
                    .chain(case.val_ty.map(|ty| (case.val_pair.pos, ty)))
            }))
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(ir::Expr {
            scheme: Scheme::join_all(
                &mut self.auto,
                result_pair.pos,
                once(&expr.scheme).chain(cases.values().map(|case| &case.scheme)),
            ),
            kind: ir::ExprKind::Match(Box::new(ir::Match { cases, expr })),
        })
    }

    fn check_proj(&mut self, proj: &ast::ProjExpr, span: FileSpan) -> Result<ir::Expr, Error> {
        let expr = self.check_expr(&proj.expr, span.0)?;

        let field_pair = self.auto.build_var();
        let record_ty = self.build_record(
            Polarity::Neg,
            Some(span),
            once((proj.field.val, field_pair.neg)),
        );
        self.auto
            .biunify(expr.scheme.ty(), record_ty)
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(ir::Expr {
            scheme: expr.scheme.clone().with_ty(field_pair.pos),
            kind: ir::ExprKind::Proj(Box::new(ir::Proj {
                expr,
                field: proj.field.val,
            })),
        })
    }

    fn check_import(&mut self, path: &str, span: FileSpan) -> Result<ir::Expr, Error> {
        let expr = match self.resolve_import(path) {
            Ok(SourceCacheResult::Miss(file, expr)) => {
                let vars = self.vars.split_off(1);
                let expr = Rc::new(self.check_expr(&expr, file)?);
                self.vars.extend(vars);
                self.source.end_source();

                self.cache.insert(file, expr.clone());
                expr.clone()
            }
            Ok(SourceCacheResult::Hit(file)) => match self.cache.get(&file) {
                Some(expr) => expr.clone(),
                None => {
                    return Err(Error::Import(
                        span,
                        path.to_owned(),
                        ErrorData::Basic("recursive import detected".into()),
                    ))
                }
            },
            Err(err) => return Err(Error::Import(span, path.to_owned(), err)),
        };

        Ok(ir::Expr {
            scheme: expr.scheme.clone(),
            kind: ir::ExprKind::Import(expr),
        })
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

    fn check_null(&mut self, span: FileSpan) -> Result<ir::Expr, Error> {
        let ty = self.build_null(Polarity::Pos, Some(span));
        Ok(ir::Expr {
            scheme: Scheme::empty(ty),
            kind: ir::ExprKind::Literal(Value::Null),
        })
    }

    fn check_bool(&mut self, val: bool, span: FileSpan) -> Result<ir::Expr, Error> {
        let ty = self.build_bool(Polarity::Pos, Some(span));
        Ok(ir::Expr {
            scheme: Scheme::empty(ty),
            kind: ir::ExprKind::Literal(Value::Bool(val)),
        })
    }

    fn check_int(&mut self, val: i64, span: FileSpan) -> Result<ir::Expr, Error> {
        let ty = self.build_number(Polarity::Pos, Some(span), NumberConstructor::Int);
        Ok(ir::Expr {
            scheme: Scheme::empty(ty),
            kind: ir::ExprKind::Literal(Value::Number(NumberValue::Int(val))),
        })
    }

    fn check_float(&mut self, val: f64, span: FileSpan) -> Result<ir::Expr, Error> {
        let ty = self.build_number(Polarity::Pos, Some(span), NumberConstructor::Float);
        Ok(ir::Expr {
            scheme: Scheme::empty(ty),
            kind: ir::ExprKind::Literal(Value::Number(NumberValue::Float(val))),
        })
    }

    fn check_string(&mut self, val: String, span: FileSpan) -> Result<ir::Expr, Error> {
        let ty = self.build_string(Polarity::Pos, Some(span));
        Ok(ir::Expr {
            scheme: Scheme::empty(ty),
            kind: ir::ExprKind::Literal(Value::String(val)),
        })
    }

    fn push_var(&mut self, symbol: Symbol, scheme: Scheme) {
        let vars = self.vars.last().unwrap().clone();
        self.vars.push(vars);
        self.set_var(symbol, scheme)
    }

    fn set_var(&mut self, symbol: Symbol, scheme: Scheme) {
        self.vars
            .last_mut()
            .unwrap()
            .insert(symbol, scheme.reduce(&self.auto));
    }

    fn get_var(&mut self, symbol: Symbol) -> Option<Scheme> {
        match self.vars.last().unwrap().get(&symbol) {
            Some(scheme) => Some(scheme.add_to(&mut self.auto)),
            None => None,
        }
    }

    fn pop_var(&mut self) {
        self.vars.pop();
    }
}

impl Error {
    fn into_diagnostics(self) -> Vec<Diagnostic> {
        match self {
            Error::TypeCheck((expr_file, expr_span), err) => {
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
                        if found.is_object() {
                            None
                        } else {
                            found.spans().get(0).map(|&(file, span)| {
                                Label::new(
                                    file,
                                    span,
                                    format!("in {} of type `{}` here", label, found),
                                )
                            })
                        }
                    }));
                vec![diagnostic]
            }
            Error::UndefinedVar((file, span), symbol) => vec![Diagnostic::new_error(
                format!("undefined variable `{}`", symbol),
                Label::new(file, span, "used here"),
            )],
            Error::Import((file, span), path, data) => match data {
                ErrorData::Basic(err) => vec![Diagnostic::new_error(
                    format!("failed to import module from `{}`: {}", path, err),
                    Label::new(file, span, "imported here"),
                )],
                ErrorData::Diagnostics(diagnostics) => diagnostics,
            },
        }
    }
}
