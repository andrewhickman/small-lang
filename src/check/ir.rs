use std::rc::Rc;

use crate::check::VarId;
use crate::rt::{Command, Value};
use crate::syntax::{Symbol, SymbolMap};

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum Expr<T = Rc<[Command]>> {
    Literal(Value),
    Var(VarId),
    Call(Box<Call<T>>),
    Let(Box<Let<T>>),
    Func(Box<Func<T>>),
    If(Box<If<T>>),
    Proj(Box<Proj<T>>),
    Enum(Box<Enum<T>>),
    Record(SymbolMap<Self>),
    Match(Box<Match<T>>),
    Import(T),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Func<T = Rc<[Command]>> {
    pub arg: VarId,
    pub body: Expr<T>,
    pub rec_var: Option<VarId>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Call<T = Rc<[Command]>> {
    pub arg: Expr<T>,
    pub func: Expr<T>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Let<T = Rc<[Command]>> {
    pub name: VarId,
    pub val: Expr<T>,
    pub body: Expr<T>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct If<T = Rc<[Command]>> {
    pub cond: Expr<T>,
    pub cons: Expr<T>,
    pub alt: Expr<T>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Proj<T = Rc<[Command]>> {
    pub expr: Expr<T>,
    pub field: Symbol,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Enum<T = Rc<[Command]>> {
    pub tag: Symbol,
    pub expr: Expr<T>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Match<T = Rc<[Command]>> {
    pub expr: Expr<T>,
    pub cases: SymbolMap<MatchCase<T>>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct MatchCase<T = Rc<[Command]>> {
    pub expr: Expr<T>,
    pub name: Option<VarId>,
}

pub trait Visitor<T = Rc<[Command]>> {
    fn visit_expr(&mut self, expr: &Expr<T>) {
        self.visit_expr_match(expr)
    }

    fn visit_expr_match(&mut self, expr: &Expr<T>) {
        match expr {
            Expr::Literal(value) => self.visit_literal(value),
            Expr::Var(var) => self.visit_var(*var),
            Expr::Call(call_expr) => self.visit_call(&*call_expr),
            Expr::Let(let_expr) => self.visit_let(&*let_expr),
            Expr::Func(func_expr) => self.visit_func(&*func_expr),
            Expr::If(if_expr) => self.visit_if(&*if_expr),
            Expr::Proj(proj_expr) => self.visit_proj(&*proj_expr),
            Expr::Enum(enum_expr) => self.visit_enum(&*enum_expr),
            Expr::Record(record_expr) => self.visit_record(record_expr),
            Expr::Match(match_expr) => self.visit_match(match_expr),
            Expr::Import(import_expr) => self.visit_import(import_expr),
        }
    }

    fn visit_literal(&mut self, _value: &Value) {}

    fn visit_var(&mut self, _var: VarId) {}

    fn visit_call(&mut self, call_expr: &Call<T>) {
        self.visit_expr(&call_expr.arg);
        self.visit_expr(&call_expr.func);
    }

    fn visit_let(&mut self, let_expr: &Let<T>) {
        self.visit_expr(&let_expr.val);
        self.visit_expr(&let_expr.body);
    }

    fn visit_func(&mut self, func_expr: &Func<T>) {
        self.visit_expr(&func_expr.body);
    }

    fn visit_if(&mut self, if_expr: &If<T>) {
        self.visit_expr(&if_expr.cond);
        self.visit_expr(&if_expr.cons);
        self.visit_expr(&if_expr.alt);
    }

    fn visit_proj(&mut self, proj_expr: &Proj<T>) {
        self.visit_expr(&proj_expr.expr);
    }

    fn visit_enum(&mut self, enum_expr: &Enum<T>) {
        self.visit_expr(&enum_expr.expr);
    }

    fn visit_record(&mut self, record_expr: &SymbolMap<Expr<T>>) {
        for expr in record_expr.values() {
            self.visit_expr(expr);
        }
    }

    fn visit_match(&mut self, record_expr: &Match<T>) {
        self.visit_expr(&record_expr.expr);
        for case in record_expr.cases.values() {
            self.visit_match_case(case);
        }
    }

    fn visit_match_case(&mut self, case: &MatchCase<T>) {
        self.visit_expr(&case.expr);
    }

    fn visit_import(&mut self, _import: &T) {}
}

pub trait VisitorMut<T = Rc<[Command]>> {
    fn visit_expr(&mut self, expr: &mut Expr<T>) {
        self.visit_expr_match(expr)
    }

    fn visit_expr_match(&mut self, expr: &mut Expr<T>) {
        match expr {
            Expr::Literal(value) => self.visit_literal(value),
            Expr::Var(var) => self.visit_var(&mut *var),
            Expr::Call(call_expr) => self.visit_call(&mut *call_expr),
            Expr::Let(let_expr) => self.visit_let(&mut *let_expr),
            Expr::Func(func_expr) => self.visit_func(&mut *func_expr),
            Expr::If(if_expr) => self.visit_if(&mut *if_expr),
            Expr::Proj(proj_expr) => self.visit_proj(&mut *proj_expr),
            Expr::Enum(enum_expr) => self.visit_enum(&mut *enum_expr),
            Expr::Record(record_expr) => self.visit_record(record_expr),
            Expr::Match(match_expr) => self.visit_match(match_expr),
            Expr::Import(import_expr) => self.visit_import(import_expr),
        }
    }

    fn visit_literal(&mut self, _value: &mut Value) {}

    fn visit_var(&mut self, _var: &mut VarId) {}

    fn visit_call(&mut self, call_expr: &mut Call<T>) {
        self.visit_expr(&mut call_expr.arg);
        self.visit_expr(&mut call_expr.func);
    }

    fn visit_let(&mut self, let_expr: &mut Let<T>) {
        self.visit_expr(&mut let_expr.val);
        self.visit_expr(&mut let_expr.body);
    }

    fn visit_func(&mut self, func_expr: &mut Func<T>) {
        self.visit_expr(&mut func_expr.body);
    }

    fn visit_if(&mut self, if_expr: &mut If<T>) {
        self.visit_expr(&mut if_expr.cond);
        self.visit_expr(&mut if_expr.cons);
        self.visit_expr(&mut if_expr.alt);
    }

    fn visit_proj(&mut self, proj_expr: &mut Proj<T>) {
        self.visit_expr(&mut proj_expr.expr);
    }

    fn visit_enum(&mut self, enum_expr: &mut Enum<T>) {
        self.visit_expr(&mut enum_expr.expr);
    }

    fn visit_record(&mut self, record_expr: &mut SymbolMap<Expr<T>>) {
        for expr in record_expr.values_mut() {
            self.visit_expr(expr);
        }
    }

    fn visit_match(&mut self, record_expr: &mut Match<T>) {
        self.visit_expr(&mut record_expr.expr);
        for case in record_expr.cases.values_mut() {
            self.visit_match_case(case);
        }
    }

    fn visit_match_case(&mut self, case: &mut MatchCase<T>) {
        self.visit_expr(&mut case.expr);
    }

    fn visit_import(&mut self, _import: &mut T) {}
}

impl<T> Expr<T> {
    pub fn visit<V>(&self, visitor: &mut V)
    where
        V: Visitor<T> + ?Sized,
    {
        visitor.visit_expr(self)
    }

    pub fn visit_mut<V>(&mut self, visitor: &mut V)
    where
        V: VisitorMut<T> + ?Sized,
    {
        visitor.visit_expr(self)
    }
}

impl<T> Let<T> {
    pub fn is_rec(&self) -> bool {
        match &self.val {
            Expr::Func(func) => func.rec_var.is_some(),
            _ => false,
        }
    }
}
