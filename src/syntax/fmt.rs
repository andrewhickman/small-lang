use std::fmt::*;

use crate::syntax::*;

macro_rules! impl_debug_with_display {
    ($t:ty) => {
        impl Debug for $t {
            fn fmt(&self, f: &mut Formatter) -> Result {
                Display::fmt(self, f)
            }
        }
    };
}

impl Display for Expr {
    fn fmt(&self, mut f: &mut Formatter) -> fmt::Result {
        match self {
            Expr::Null => write!(f, "null"),
            Expr::Bool(val) => write!(f, "{}", val),
            Expr::Int(val) => write!(f, "{}", val),
            Expr::String(val) => {
                write!(f, "\"{}\"", val.replace("\\", "\\\\").replace("\"", "\\\""))
            }
            Expr::Var(var) => write!(f, "{}", var),
            Expr::Record(map) => {
                if map.len() == 0 {
                    write!(f, "{{}}")
                } else {
                    writeln!(f, "{{")?;
                    for (field, expr) in map {
                        writeln!(indented(&mut f), "{}: {},", field, expr)?;
                    }
                    write!(f, "}}")
                }
            }
            Expr::Enum(expr) => write!(f, "{}", expr),
            Expr::Func(expr) => write!(f, "{}", expr),
            Expr::Call(expr) => write!(f, "{}", expr),
            Expr::Let(expr) => write!(f, "{}", expr),
            Expr::Rec(expr) => write!(f, "{}", expr),
            Expr::If(expr) => write!(f, "{}", expr),
            Expr::Proj(expr) => write!(f, "{}", expr),
            Expr::Match(expr) => write!(f, "{}", expr),
            Expr::Import(val) => write!(f, "import {}", Expr::String(val.clone())),
        }
    }
}

impl Display for EnumExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(expr) = &self.expr {
            write!(f, "[{}: {}]", self.tag, expr)
        } else {
            write!(f, "[{}]", self.tag)
        }
    }
}

impl Display for FuncExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "func {} => {}", self.arg, self.body)
    }
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{} {}",
            CallFuncExpr(&self.func.val),
            SimpleExpr(&self.arg.val)
        )
    }
}

impl Display for LetExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let {} = {} in\n{}", self.name, self.val, self.body)
    }
}

impl Display for RecExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let rec {} = {} in\n{}", self.name, self.func, self.body)
    }
}

impl Display for IfExpr {
    fn fmt(&self, mut f: &mut Formatter) -> Result {
        writeln!(f, "if {}", self.cond)?;
        write!(indented(&mut f), "then {}\nelse {}", self.cons, self.alt)
    }
}

impl Display for ProjExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}.{}", ProjValExpr(&self.expr.val), self.field)
    }
}

impl Display for MatchExpr {
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

impl Display for MatchExprCase {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(name) = self.name {
            write!(f, ": {}", name)?;
        }
        write!(f, " => {}", self.expr)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.val)
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.val)
    }
}

impl Expr {
    fn is_simple(&self) -> bool {
        match self {
            Expr::Null
            | Expr::Bool(_)
            | Expr::Var(_)
            | Expr::Int(_)
            | Expr::String(_)
            | Expr::Record(_)
            | Expr::Match(_)
            | Expr::Enum(_)
            | Expr::Proj(_)
            | Expr::Import(_) => true,
            Expr::Func(_) | Expr::Call(_) | Expr::Let(_) | Expr::Rec(_) | Expr::If(_) => false,
        }
    }

    fn is_call_func(&self) -> bool {
        match self {
            Expr::Call(_) => true,
            _ => self.is_simple(),
        }
    }

    fn is_proj_val(&self) -> bool {
        match self {
            Expr::Int(_) => false,
            _ => self.is_simple(),
        }
    }
}

struct SimpleExpr<'a>(&'a Expr);
struct CallFuncExpr<'a>(&'a Expr);
struct ProjValExpr<'a>(&'a Expr);

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

impl_debug_with_display!(Expr);
impl_debug_with_display!(EnumExpr);
impl_debug_with_display!(FuncExpr);
impl_debug_with_display!(CallExpr);
impl_debug_with_display!(LetExpr);
impl_debug_with_display!(RecExpr);
impl_debug_with_display!(IfExpr);
impl_debug_with_display!(ProjExpr);
impl_debug_with_display!(MatchExpr);
impl_debug_with_display!(MatchExprCase);

struct Indented<W> {
    writer: W,
    on_newline: bool,
}

impl<W> Write for Indented<W>
where
    W: Write,
{
    fn write_str(&mut self, mut s: &str) -> fmt::Result {
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
