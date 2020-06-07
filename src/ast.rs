use crate::lexer::Op;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub enum Type {
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    List(Box<Type>),
}

fn fmt_tuple<T: Display, F>(f: &mut Formatter<'_>, v: &Vec<T>, fmt: F) -> Result
where
    F: Fn(&mut Formatter<'_>, &T) -> Result,
{
    match v.len() {
        0 => write!(f, "()"),
        1 => {
            write!(f, "(")?;
            fmt(f, v.first().unwrap())?;
            write!(f, ",)")
        }
        _ => {
            write!(f, "(")?;
            let mut it = v.iter();
            fmt(f, it.next().unwrap())?;
            for x in v {
                write!(f, ", ")?;
                fmt(f, &x)?;
            }
            write!(f, ")")
        }
    }
}

fn fmt_list<T, F>(
    f: &mut Formatter<'_>,
    v: &Vec<T>,
    left: &str,
    right: &str,
    pat: &str,
    fmt: F,
) -> Result
where
    F: Fn(&mut Formatter<'_>, &T) -> Result,
{
    write!(f, "{}", left)?;
    let mut it = v.iter();
    if let Some(x) = it.next() {
        fmt(f, x)?;
    }
    for x in it {
        write!(f, "{}", pat)?;
        fmt(f, x)?;
    }
    write!(f, "{}", right)
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Func(x, y) => {
                if let Type::Func(_, _) = **x {
                    write!(f, "({}) → {}", x, y)
                } else {
                    write!(f, "{} → {}", x, y)
                }
            }
            Type::Tuple(tys) => fmt_tuple(f, tys, |f, ty| write!(f, "{}", ty)),
            Type::Union(tys) => fmt_list(f, tys, "(", ")", "| ", |f, ty| write!(f, "{}", ty)),
            Type::List(ty) => write!(f, "[{}]", ty),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Lambda(String, Type, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Fix(Box<Expr>),

    UnaryOp(Op, Box<Expr>),
    BinOp(Op, Box<Expr>, Box<Expr>),
    Tuple(Vec<Expr>),
    TupleIndex(Box<Expr>, i32),
    Union(Vec<(Type, Option<Expr>)>),
    CaseOf(Box<Expr>, Vec<(Type, String, Expr)>),
    List(Vec<Expr>),
    Nil(Box<Expr>),
    Head(Box<Expr>),
    Tail(Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    Identifier(String),
    Int(i32),
    Bool(bool),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Lambda(id, ty, expr) => write!(f, "λ{}:{}. {}", id, ty, expr),
            Expr::Apply(expr0, expr1) => {
                match expr0 {
                    box Expr::BinOp(_, _, _) | box Expr::UnaryOp(_, _) => write!(f, "({})", expr0)?,
                    _ => write!(f, "{}", expr0)?,
                }
                write!(f, " ")?;
                match expr1 {
                    box Expr::BinOp(_, _, _) | box Expr::UnaryOp(_, _) => write!(f, "({})", expr1),
                    _ => write!(f, "{}", expr1),
                }
            }
            Expr::Fix(expr) => write!(f, "fix ({})", expr),
            Expr::UnaryOp(op, expr) => match expr {
                box Expr::BinOp(_, _, _) | box Expr::Apply(_, _) => {
                    write!(f, "{}({})", op.to_str(), expr)
                }
                _ => write!(f, "{}{}", op.to_str(), expr),
            },
            Expr::BinOp(op, expr0, expr1) => {
                let p = op.prior().unwrap();
                if match expr0 {
                    box Expr::BinOp(op0, _, _) => {
                        if let Some(p0) = op0.prior() {
                            p0 < p
                        } else {
                            false
                        }
                    }
                    box Expr::UnaryOp(_, _) | box Expr::Apply(_, _) => true,
                    _ => false,
                } {
                    write!(f, "({})", expr0)?;
                } else {
                    write!(f, "{}", expr0)?;
                }
                write!(f, " {} ", op.to_str())?;
                if match expr1 {
                    box Expr::BinOp(op1, _, _) => {
                        if let Some(p1) = op1.prior() {
                            p1 < p
                        } else {
                            false
                        }
                    }
                    box Expr::UnaryOp(_, _) | box Expr::Apply(_, _) => true,
                    _ => false,
                } {
                    write!(f, "({})", expr1)?;
                } else {
                    write!(f, "{}", expr1)?;
                }
                Ok(())
            }
            Expr::Tuple(exprs) => fmt_tuple(f, exprs, |f, expr| write!(f, "{}", expr)),
            Expr::TupleIndex(expr, i) => write!(f, "({}.{})", expr, i),
            Expr::Union(unions) => fmt_list(f, unions, "union (", ")", "| ", |f, x| {
                write!(f, "{}", x.0)?;
                if let Some(ref expr) = x.1 {
                    write!(f, " {}", expr)?;
                }
                Ok(())
            }),
            Expr::CaseOf(expr, cases) => {
                write!(f, "case {} of ", expr)?;
                fmt_list(f, cases, "(", ")", "| ", |f, x| {
                    write!(f, "{} {} ⇒ {}", x.0, x.1, x.2)
                })
            }
            Expr::List(exprs) => {
                fmt_list(f, exprs, "[", "]", ", ", |f, expr| write!(f, "{}", expr))
            }
            Expr::Nil(expr) => write!(f, "nil ({})", expr),
            Expr::Head(expr) => write!(f, "head ({})", expr),
            Expr::Tail(expr) => write!(f, "tail ({})", expr),
            Expr::IfThenElse(cond, expr0, expr1) => {
                write!(f, "if {} then {} else {}", cond, expr0, expr1)
            }
            Expr::Identifier(id) => write!(f, "{}", id),
            Expr::Int(x) => write!(f, "{}", x),
            Expr::Bool(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug)]
pub enum AST {
    Let(String, Type, Expr),
    Expr(Expr),
}

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            AST::Let(id, ty, expr) => write!(f, "let {}:{} = {};", id, ty, expr),
            AST::Expr(expr) => write!(f, "{}", expr),
        }
    }
}
