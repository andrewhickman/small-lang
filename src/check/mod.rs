pub mod ir;
pub mod scheme;
pub mod vars;

mod builtin;
mod ty;

use std::iter::once;

use codespan::FileId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use mlsub::auto::{flow, Automaton, StateId};
use mlsub::{BiunifyError, Polarity};

use crate::check::scheme::{ReducedScheme, Scheme};
use crate::check::ty::{Constructor, NumberConstructor};
use crate::check::vars::{VarId, Vars};
use crate::rt::{NumberValue, Value};
use crate::syntax::{ast, Symbol, SymbolMap};
use crate::{ErrorData, FileSpan};

pub fn check<T, F>(
    file: FileId,
    expr: &ast::Spanned<ast::Expr>,
    import: F,
) -> Result<(ReducedScheme, ir::Expr<T>, Vec<Diagnostic<FileId>>), ErrorData>
where
    T: Clone,
    F: FnMut(&str) -> Result<(ReducedScheme, T), ErrorData>,
{
    let mut ctx = Context::new(import);
    ctx.set_builtins();
    let output = ctx
        .check_expr(expr, file)
        .map_err(Error::into_diagnostics)
        .map_err(ErrorData::Diagnostics)?;
    Ok((
        output.scheme.reduce(&ctx.auto),
        ir::Expr {
            nodes: ctx.ir,
            id: output.expr,
        },
        ctx.warnings,
    ))
}

#[derive(Debug)]
enum Error {
    UndefinedVar(FileSpan, Symbol),
    Import(FileSpan, String, ErrorData),
    TypeCheck(FileSpan, Box<BiunifyError<Constructor>>),
}

struct Context<T, F> {
    auto: Automaton<Constructor>,
    vars: Vars,
    import: F,
    warnings: Vec<Diagnostic<FileId>>,
    ir: ir::Nodes<T>,
}

struct CheckOutput {
    scheme: Scheme,
    expr: ir::NodeId,
}

impl<T, F> Context<T, F>
where
    T: Clone,
    F: FnMut(&str) -> Result<(ReducedScheme, T), ErrorData>,
{
    fn new(import: F) -> Self {
        Context {
            auto: Automaton::new(),
            vars: Vars::default(),
            warnings: vec![],
            ir: ir::Nodes::new(),
            import,
        }
    }

    fn check_expr(
        &mut self,
        expr: &ast::Spanned<ast::Expr>,
        file: FileId,
    ) -> Result<CheckOutput, Error> {
        let span = (file, expr.span);
        match &expr.val {
            ast::Expr::Null => self.check_null(span),
            ast::Expr::Var(name) => self.check_var(*name, span),
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

    fn check_var(&mut self, name: Symbol, span: FileSpan) -> Result<CheckOutput, Error> {
        if let Some((scheme, id)) = self.get_var(name) {
            Ok(CheckOutput {
                scheme,
                expr: self.ir.add(ir::Node::Var(id)),
            })
        } else {
            Err(Error::UndefinedVar(span, name))
        }
    }

    fn check_func(
        &mut self,
        func: &ast::FuncExpr,
        span: FileSpan,
        rec_var: Option<VarId>,
    ) -> Result<CheckOutput, Error> {
        let arg_pair = self.auto.build_var();
        let ret_pair = self.auto.build_var();

        let arg_var = self.ir.next();
        self.push_var(
            arg_var,
            func.arg.val,
            (span.0, func.arg.span),
            &Scheme::from_var(func.arg.val, arg_pair),
        );
        let body = self.check_expr(&func.body, span.0)?;
        self.pop_var(func.arg.val);

        let (scheme, domain_ty) = body.scheme.without_var(func.arg.val);
        let func_ty = self.build_func(Polarity::Pos, Some(span), arg_pair.neg, ret_pair.pos);
        self.auto
            .biunify_all(
                once((body.scheme.ty(), ret_pair.neg))
                    .chain(domain_ty.map(|ty| (arg_pair.pos, ty))),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(CheckOutput {
            scheme: scheme.with_ty(func_ty),
            expr: self.ir.add_at(
                arg_var,
                ir::Node::Func(ir::Func {
                    span: span,
                    body: body.expr,
                    rec_var,
                }),
            ),
        })
    }

    fn check_call(&mut self, call: &ast::CallExpr, span: FileSpan) -> Result<CheckOutput, Error> {
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

        Ok(CheckOutput {
            scheme: Scheme::join(&mut self.auto, ret_pair.pos, &func.scheme, &arg.scheme),
            expr: self.ir.add(ir::Node::Call(ir::Call {
                arg: arg.expr,
                func: func.expr,
            })),
        })
    }

    fn check_let(&mut self, let_expr: &ast::LetExpr, span: FileSpan) -> Result<CheckOutput, Error> {
        let val = self.check_expr(&let_expr.val, span.0)?;

        let name_var = self.ir.next();
        self.push_var(
            name_var,
            let_expr.name.val,
            (span.0, let_expr.name.span),
            &val.scheme,
        );
        let body = self.check_expr(&let_expr.body, span.0)?;
        self.pop_var(let_expr.name.val);

        Ok(CheckOutput {
            scheme: Scheme::join(&mut self.auto, body.scheme.ty(), &val.scheme, &body.scheme),
            expr: self.ir.add_at(
                name_var,
                ir::Node::Let(ir::Let {
                    val: val.expr,
                    body: body.expr,
                }),
            ),
        })
    }

    fn check_rec(&mut self, rec: &ast::RecExpr, span: FileSpan) -> Result<CheckOutput, Error> {
        let func_pair = self.auto.build_var();
        let rec_var = self.ir.next();
        self.push_var(
            rec_var,
            rec.name.val,
            (span.0, rec.name.span),
            &Scheme::from_var(rec.name.val, func_pair),
        );
        let func = self.check_func(&rec.func.val, (span.0, rec.func.span), Some(rec_var))?;

        let (scheme, func_ty) = func.scheme.without_var(rec.name.val);
        self.auto
            .biunify_all(
                once((func.scheme.ty(), func_pair.neg))
                    .chain(func_ty.map(|ty| (func_pair.pos, ty))),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        self.set_var_scheme(rec_var, &func.scheme);
        let body = self.check_expr(&rec.body, span.0)?;
        self.pop_var(rec.name.val);

        Ok(CheckOutput {
            scheme: Scheme::join(&mut self.auto, body.scheme.ty(), &scheme, &body.scheme),
            expr: self.ir.add_at(
                rec_var,
                ir::Node::Let(ir::Let {
                    val: func.expr,
                    body: body.expr,
                }),
            ),
        })
    }

    fn check_if(&mut self, if_expr: &ast::IfExpr, span: FileSpan) -> Result<CheckOutput, Error> {
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

        Ok(CheckOutput {
            scheme: Scheme::join_all(
                &mut self.auto,
                pair.pos,
                once(&cond.scheme)
                    .chain(once(&cons.scheme))
                    .chain(once(&alt.scheme)),
            ),
            expr: self.ir.add(ir::Node::If(ir::If {
                cond: cond.expr,
                cons: cons.expr,
                alt: alt.expr,
            })),
        })
    }

    fn check_record(
        &mut self,
        rec: &SymbolMap<ast::Spanned<ast::Expr>>,
        span: FileSpan,
    ) -> Result<CheckOutput, Error> {
        struct CheckRecordEntryOutput {
            field: Symbol,
            expr: ir::NodeId,
            scheme: Scheme,
            pair: flow::Pair,
        }

        let fields = rec
            .iter()
            .map(|(&field, expr)| {
                let CheckOutput { expr, scheme } = self.check_expr(expr, span.0)?;
                Ok(CheckRecordEntryOutput {
                    field,
                    expr,
                    scheme,
                    pair: self.auto.build_var(),
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let record_ty = self.build_record(
            Polarity::Pos,
            Some(span),
            fields.iter().map(|entry| (entry.field, entry.pair.pos)),
        );
        self.auto
            .biunify_all(
                fields
                    .iter()
                    .map(|entry| (entry.scheme.ty(), entry.pair.neg)),
            )
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(CheckOutput {
            scheme: Scheme::join_all(
                &mut self.auto,
                record_ty,
                fields.iter().map(|entry| &entry.scheme),
            ),
            expr: self.ir.add(ir::Node::Record(
                fields
                    .into_iter()
                    .map(|entry| (entry.field, entry.expr.into()))
                    .collect(),
            )),
        })
    }

    fn check_enum(
        &mut self,
        enum_expr: &ast::EnumExpr,
        span: FileSpan,
    ) -> Result<CheckOutput, Error> {
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

        Ok(CheckOutput {
            scheme: expr.scheme.clone().with_ty(enum_ty),
            expr: self.ir.add(ir::Node::Enum(ir::Enum {
                tag: enum_expr.tag.val,
                expr: expr.expr,
            })),
        })
    }

    fn check_match(
        &mut self,
        match_expr: &ast::MatchExpr,
        span: FileSpan,
    ) -> Result<CheckOutput, Error> {
        struct CheckMatchCaseOutput {
            tag: Symbol,
            expr: ir::NodeId,
            val_pair: flow::Pair,
            val_ty: Option<StateId>,
            scheme: Scheme,
        }

        let expr = self.check_expr(&match_expr.expr, span.0)?;

        let result_pair = self.auto.build_var();

        let name_var = self.ir.next();
        let cases = match_expr
            .cases
            .iter()
            .map(|(&tag, case)| {
                let case_pair = self.auto.build_var();

                if let Some(name) = case.val.name {
                    self.push_var(
                        name_var,
                        name.val,
                        (span.0, name.span),
                        &Scheme::from_var(name.val, case_pair),
                    );
                }

                let case_expr = self.check_expr(&case.val.expr, span.0)?;

                let (scheme, val_ty) = if let Some(name) = case.val.name {
                    self.pop_var(name.val);
                    case_expr.scheme.without_var(name.val)
                } else {
                    (case_expr.scheme.clone(), None)
                };

                Ok(CheckMatchCaseOutput {
                    tag,
                    expr: case_expr.expr,
                    scheme,
                    val_pair: case_pair,
                    val_ty,
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let enum_ty = self.build_enum(
            Polarity::Neg,
            Some(span),
            cases.iter().map(|case| (case.tag, case.val_pair.neg)),
        );

        self.auto
            .biunify(expr.scheme.ty(), enum_ty)
            .map_err(|err| Error::TypeCheck(span, err.into()))?;
        self.auto
            .biunify_all(cases.iter().flat_map(|case| {
                once((case.scheme.ty(), result_pair.neg))
                    .chain(case.val_ty.map(|ty| (case.val_pair.pos, ty)))
            }))
            .map_err(|err| Error::TypeCheck(span, err.into()))?;

        Ok(CheckOutput {
            scheme: Scheme::join_all(
                &mut self.auto,
                result_pair.pos,
                once(&expr.scheme).chain(cases.iter().map(|case| &case.scheme)),
            ),
            expr: self.ir.add_at(
                name_var,
                ir::Node::Match(ir::Match {
                    cases: cases
                        .into_iter()
                        .map(|case| (case.tag, case.expr))
                        .collect(),
                    expr: expr.expr,
                }),
            ),
        })
    }

    fn check_proj(&mut self, proj: &ast::ProjExpr, span: FileSpan) -> Result<CheckOutput, Error> {
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

        Ok(CheckOutput {
            scheme: expr.scheme.clone().with_ty(field_pair.pos),
            expr: self.ir.add(ir::Node::Proj(ir::Proj {
                expr: expr.expr,
                field: proj.field.val,
            })),
        })
    }

    fn check_import(&mut self, path: &str, span: FileSpan) -> Result<CheckOutput, Error> {
        let (reduced_scheme, expr) =
            (self.import)(path).map_err(|err| Error::Import(span, path.to_owned(), err))?;
        let scheme = reduced_scheme.add_to(&mut self.auto);
        Ok(CheckOutput {
            scheme,
            expr: self.ir.add(ir::Node::Import(expr)),
        })
    }

    fn check_null(&mut self, span: FileSpan) -> Result<CheckOutput, Error> {
        let ty = self.build_null(Polarity::Pos, Some(span));
        Ok(CheckOutput {
            scheme: Scheme::empty(ty),
            expr: self.ir.add(ir::Node::Literal(Value::Null)),
        })
    }

    fn check_bool(&mut self, val: bool, span: FileSpan) -> Result<CheckOutput, Error> {
        let ty = self.build_bool(Polarity::Pos, Some(span));
        Ok(CheckOutput {
            scheme: Scheme::empty(ty),
            expr: self.ir.add(ir::Node::Literal(Value::Bool(val))),
        })
    }

    fn check_int(&mut self, val: i64, span: FileSpan) -> Result<CheckOutput, Error> {
        let ty = self.build_number(Polarity::Pos, Some(span), NumberConstructor::Int);
        Ok(CheckOutput {
            scheme: Scheme::empty(ty),
            expr: self
                .ir
                .add(ir::Node::Literal(Value::Number(NumberValue::Int(val)))),
        })
    }

    fn check_float(&mut self, val: f64, span: FileSpan) -> Result<CheckOutput, Error> {
        let ty = self.build_number(Polarity::Pos, Some(span), NumberConstructor::Float);
        Ok(CheckOutput {
            scheme: Scheme::empty(ty),
            expr: self
                .ir
                .add(ir::Node::Literal(Value::Number(NumberValue::Float(val)))),
        })
    }

    fn check_string(&mut self, val: String, span: FileSpan) -> Result<CheckOutput, Error> {
        let ty = self.build_string(Polarity::Pos, Some(span));
        Ok(CheckOutput {
            scheme: Scheme::empty(ty),
            expr: self.ir.add(ir::Node::Literal(Value::String(val))),
        })
    }
}

impl<T, F> Context<T, F> {
    fn push_var(
        &mut self,
        id: VarId,
        symbol: Symbol,
        span: impl Into<Option<FileSpan>>,
        scheme: &Scheme,
    ) {
        let reduced = scheme.reduce(&self.auto);
        log::debug!("Push var {} ({:?})", symbol, id);
        self.vars.push(id, symbol, span.into(), reduced);
    }

    fn set_var_scheme(&mut self, id: VarId, scheme: &Scheme) {
        let reduced = scheme.reduce(&self.auto);
        self.vars.get_mut(id).scheme = reduced;
    }

    fn get_var(&mut self, symbol: Symbol) -> Option<(Scheme, VarId)> {
        let id = self.vars.get_id(symbol);
        if let Some(id) = id {
            let data = self.vars.get_mut(id);
            data.uses += 1;
            Some((data.scheme.add_to(&mut self.auto), id))
        } else {
            None
        }
    }

    fn pop_var(&mut self, symbol: Symbol) -> VarId {
        let id = self.vars.pop(symbol);
        log::debug!("Pop var {} ({:?})", symbol, id);
        let data = self.vars.get(id);
        if data.uses == 0 {
            let (file, span) = data.span.unwrap();
            self.warnings.push(
                Diagnostic::warning()
                    .with_message(format!("Unused variable `{}`", data.name))
                    .with_labels(vec![Label::primary(file, span).with_message("defined here")]),
            );
        }
        id
    }
}

impl Error {
    fn into_diagnostics(self) -> Vec<Diagnostic<FileId>> {
        match self {
            Error::TypeCheck((expr_file, expr_span), err) => {
                let actual_span = err.constraint.0.spans().get(0);
                let expected_span = err.constraint.1.spans().get(0);

                let primary_span = match (actual_span, expected_span) {
                    (Some(&actual_span), _) => actual_span,
                    (_, Some(&expected_span)) => expected_span,
                    (None, None) => (expr_file, expr_span),
                };

                let mut diagnostic = Diagnostic::error()
                    .with_message(format!(
                        "expected `{}` but found `{}`",
                        err.constraint.1, err.constraint.0
                    ))
                    .with_labels(vec![
                        Label::primary(primary_span.0, primary_span.1).with_message("found here")
                    ]);
                if actual_span.is_some() {
                    diagnostic
                        .labels
                        .extend(err.constraint.1.spans().iter().map(|&(file, span)| {
                            Label::secondary(file, span).with_message("expected type inferred here")
                        }));
                }
                diagnostic
                    .labels
                    .extend(err.stack.iter().filter_map(|(label, found, _)| {
                        if found.is_object() {
                            None
                        } else {
                            found.spans().get(0).map(|&(file, span)| {
                                Label::secondary(file, span)
                                    .with_message(format!("in {} of type `{}` here", label, found))
                            })
                        }
                    }));
                vec![diagnostic]
            }
            Error::UndefinedVar((file, span), symbol) => vec![Diagnostic::error()
                .with_message(format!("undefined variable `{}`", symbol))
                .with_labels(vec![Label::primary(file, span).with_message("used here")])],
            Error::Import((file, span), path, data) => match data {
                ErrorData::Basic(err) => vec![Diagnostic::error()
                    .with_message(format!("failed to import module from `{}`: {}", path, err))
                    .with_labels(vec![
                        Label::primary(file, span).with_message("imported here")
                    ])],
                ErrorData::Diagnostics(diagnostics) => diagnostics,
            },
        }
    }
}
