use std::fmt::*;
use std::str::FromStr;

use crate::syntax::ast;

macro_rules! impl_debug_with_display {
    ($t:ty) => {
        impl Debug for $t {
            fn fmt(&self, f: &mut Formatter) -> Result {
                Display::fmt(self, f)
            }
        }
    };
}

impl Display for ast::Expr {
    fn fmt(&self, mut f: &mut Formatter) -> Result {
        match self {
            ast::Expr::Null => write!(f, "null"),
            ast::Expr::Bool(val) => write!(f, "{}", val),
            ast::Expr::Int(val) => write!(f, "{}", val),
            ast::Expr::Float(val) => {
                let s = val.to_string();
                // avoid ambiguity between int and float
                if i64::from_str(&s).is_ok() {
                    write!(f, "{}.", s)
                } else {
                    write!(f, "{}", s)
                }
            }
            ast::Expr::String(val) => {
                write!(f, "\"{}\"", val.replace("\\", "\\\\").replace("\"", "\\\""))
            }
            ast::Expr::Var(var) => write!(f, "{}", var),
            ast::Expr::Record(map) => {
                if map.is_empty() {
                    write!(f, "{{}}")
                } else {
                    writeln!(f, "{{")?;
                    for (field, expr) in map {
                        writeln!(indented(&mut f), "{}: {},", field, expr)?;
                    }
                    write!(f, "}}")
                }
            }
            ast::Expr::Enum(expr) => write!(f, "{}", expr),
            ast::Expr::Func(expr) => write!(f, "{}", expr),
            ast::Expr::Call(expr) => write!(f, "{}", expr),
            ast::Expr::Let(expr) => write!(f, "{}", expr),
            ast::Expr::Rec(expr) => write!(f, "{}", expr),
            ast::Expr::If(expr) => write!(f, "{}", expr),
            ast::Expr::Proj(expr) => write!(f, "{}", expr),
            ast::Expr::Match(expr) => write!(f, "{}", expr),
            ast::Expr::Import(val) => write!(f, "import {}", ast::Expr::String(val.clone())),
        }
    }
}

impl Display for ast::EnumExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(expr) = &self.expr {
            write!(f, "[{}: {}]", self.tag, expr)
        } else {
            write!(f, "[{}]", self.tag)
        }
    }
}

impl Display for ast::FuncExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "func {} => {}", self.arg, self.body)
    }
}

impl Display for ast::CallExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{} {}",
            CallFuncExpr(&self.func.val),
            SimpleExpr(&self.arg.val)
        )
    }
}

impl Display for ast::LetExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let {} = {} in\n{}", self.name, self.val, self.body)
    }
}

impl Display for ast::RecExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let rec {} = {} in\n{}", self.name, self.func, self.body)
    }
}

impl Display for ast::IfExpr {
    fn fmt(&self, mut f: &mut Formatter) -> Result {
        writeln!(f, "if {}", self.cond)?;
        write!(indented(&mut f), "then {}\nelse {}", self.cons, self.alt)
    }
}

impl Display for ast::ProjExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}.{}", ProjValExpr(&self.expr.val), self.field)
    }
}

impl Display for ast::MatchExpr {
    fn fmt(&self, mut f: &mut Formatter) -> Result {
        write!(f, "match {} with ", self.expr)?;
        if self.cases.len() == 0 {
            write!(f, "[]")
        } else {
            writeln!(f, "[")?;
            for (tag, case) in &self.cases {
                writeln!(indented(&mut f), "{}{},", tag, case)?;
            }
            write!(f, "]")
        }
    }
}

impl Display for ast::MatchExprCase {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(name) = self.name {
            write!(f, ": {}", name)?;
        }
        write!(f, " => {}", self.expr)
    }
}

impl<T: Display> Display for ast::Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.val)
    }
}

impl<T: Debug> Debug for ast::Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.val)
    }
}

impl ast::Expr {
    fn is_simple(&self) -> bool {
        match self {
            ast::Expr::Null
            | ast::Expr::Bool(_)
            | ast::Expr::Var(_)
            | ast::Expr::Int(_)
            | ast::Expr::Float(_)
            | ast::Expr::String(_)
            | ast::Expr::Record(_)
            | ast::Expr::Match(_)
            | ast::Expr::Enum(_)
            | ast::Expr::Proj(_)
            | ast::Expr::Import(_) => true,
            ast::Expr::Func(_)
            | ast::Expr::Call(_)
            | ast::Expr::Let(_)
            | ast::Expr::Rec(_)
            | ast::Expr::If(_) => false,
        }
    }

    fn is_call_func(&self) -> bool {
        match self {
            ast::Expr::Call(_) => true,
            _ => self.is_simple(),
        }
    }

    fn is_proj_val(&self) -> bool {
        match self {
            ast::Expr::Int(_) | ast::Expr::Float(_) => false,
            _ => self.is_simple(),
        }
    }
}

struct SimpleExpr<'a>(&'a ast::Expr);
struct CallFuncExpr<'a>(&'a ast::Expr);
struct ProjValExpr<'a>(&'a ast::Expr);

impl<'a> Display for SimpleExpr<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.0.is_simple() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "({})", self.0)
        }
    }
}

impl<'a> Display for CallFuncExpr<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.0.is_call_func() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "({})", self.0)
        }
    }
}

impl<'a> Display for ProjValExpr<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.0.is_proj_val() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "({})", self.0)
        }
    }
}

impl_debug_with_display!(ast::Expr);
impl_debug_with_display!(ast::EnumExpr);
impl_debug_with_display!(ast::FuncExpr);
impl_debug_with_display!(ast::CallExpr);
impl_debug_with_display!(ast::LetExpr);
impl_debug_with_display!(ast::RecExpr);
impl_debug_with_display!(ast::IfExpr);
impl_debug_with_display!(ast::ProjExpr);
impl_debug_with_display!(ast::MatchExpr);
impl_debug_with_display!(ast::MatchExprCase);

struct Indented<W> {
    writer: W,
    on_newline: bool,
}

impl<W> Write for Indented<W>
where
    W: Write,
{
    fn write_str(&mut self, mut s: &str) -> Result {
        while !s.is_empty() {
            if self.on_newline {
                self.writer.write_str("  ")?;
            }

            let split = match s.find('\n') {
                Some(pos) => {
                    self.on_newline = true;
                    pos + 1
                }
                None => {
                    self.on_newline = false;
                    s.len()
                }
            };
            self.writer.write_str(&s[..split])?;
            s = &s[split..];
        }

        Ok(())
    }
}

fn indented<W>(writer: W) -> Indented<W> {
    Indented {
        writer,
        on_newline: true,
    }
}
