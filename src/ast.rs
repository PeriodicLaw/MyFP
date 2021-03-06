use crate::lexer::Op;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub struct AST {
	/// 变量名，可选
	pub id: Option<String>,
	/// 声明的类型变量，可为空
	pub tyvars: Vec<String>,
	/// 声明的类型，可选
	pub ty: Option<Type>,
	/// 表达式
	pub expr: Expr,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
	Int,
	Bool,
	Func(Box<Type>, Box<Type>),
	Tuple(Vec<Type>),
	Union(Vec<Type>),
	List(Box<Type>),
	// 匿名变量以_为名字
	Var(String),
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
			for x in it {
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
			Type::Union(tys) => fmt_list(f, tys, "(", ")", " | ", |f, ty| write!(f, "{}", ty)),
			Type::List(ty) => write!(f, "[{}]", ty),
			Type::Var(s) => write!(f, "{}", s),
		}
	}
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
	Add,
	Minus,
	Not,
}

impl UnaryOp {
	pub fn to_str(&self) -> &str {
		match self {
			UnaryOp::Add => "+",
			UnaryOp::Minus => "-",
			UnaryOp::Not => "!",
		}
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BinOp {
	Add,
	Minus,
	Mult,
	Divide,
	Eq,
	NotEq,
	Less,
	LessEq,
	Gre,
	GreEq,
	And,
	Or,
	Cons,
	Concat,
}

impl BinOp {
	pub fn to_str(&self) -> &str {
		match self {
			BinOp::Add => "+",
			BinOp::Minus => "-",
			BinOp::Mult => "*",
			BinOp::Divide => "/",
			BinOp::Eq => "==",
			BinOp::NotEq => "!=",
			BinOp::Less => "<",
			BinOp::LessEq => "<=",
			BinOp::Gre => ">",
			BinOp::GreEq => ">=",
			BinOp::And => "&&",
			BinOp::Or => "||",
			BinOp::Cons => "::",
			BinOp::Concat => "++",
		}
	}

	pub fn from_op(op: &Op) -> Option<BinOp> {
		match op {
			Op::Add => Some(BinOp::Add),
			Op::Minus => Some(BinOp::Minus),
			Op::Mult => Some(BinOp::Mult),
			Op::Divide => Some(BinOp::Divide),
			Op::Eq => Some(BinOp::Eq),
			Op::NotEq => Some(BinOp::NotEq),
			Op::Less => Some(BinOp::Less),
			Op::LessEq => Some(BinOp::LessEq),
			Op::Gre => Some(BinOp::Gre),
			Op::GreEq => Some(BinOp::GreEq),
			Op::And => Some(BinOp::And),
			Op::Or => Some(BinOp::Or),
			Op::Cons => Some(BinOp::Cons),
			Op::Concat => Some(BinOp::Concat),
			_ => None,
		}
	}

	pub fn is_binop(op: &Op) -> bool {
		if let Some(_) = BinOp::from_op(op) {
			true
		} else {
			false
		}
	}

	fn prior(&self) -> i32 {
		match self {
			BinOp::Or => 0,
			BinOp::And => 1,
			BinOp::Eq | BinOp::NotEq | BinOp::Less | BinOp::LessEq | BinOp::Gre | BinOp::GreEq => 2,
			BinOp::Cons | BinOp::Concat => 3,
			BinOp::Add | BinOp::Minus => 4,
			BinOp::Mult | BinOp::Divide => 5,
		}
	}

	/// op0是栈顶，op1是新运算符，返回是否需要弹出栈顶（是否左结合）
	pub fn greater(op0: &BinOp, op1: &BinOp) -> bool {
		if *op0 == BinOp::Cons && *op1 == BinOp::Cons {
			false
		} else {
			op0.prior() >= op1.prior()
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expr {
	Lambda(String, Type, Box<Expr>),
	Apply(Box<Expr>, Box<Expr>),
	Fix(Box<Expr>),

	UnaryOp(UnaryOp, Box<Expr>),
	BinOp(BinOp, Box<Expr>, Box<Expr>),
	Tuple(Vec<Expr>),
	TupleIndex(Box<Expr>, usize),
	Union(Vec<(Type, Option<Expr>)>),
	CaseOf(Box<Expr>, Vec<(Type, String, Expr)>),
	List(Vec<Expr>),
	MatchOf(Box<Expr>, Box<Expr>, String, String, Box<Expr>),
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
					box Expr::BinOp(_, _, _)
					| box Expr::UnaryOp(_, _)
					| box Expr::Lambda(_, _, _) => write!(f, "({})", expr0)?,
					_ => write!(f, "{}", expr0)?,
				}
				write!(f, " ")?;
				match expr1 {
					box Expr::BinOp(_, _, _)
					| box Expr::UnaryOp(_, _)
					| box Expr::Apply(_, _)
					| box Expr::Lambda(_, _, _) => write!(f, "({})", expr1),
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
				if match expr0 {
					box Expr::BinOp(op0, _, _) => op0.prior() < op.prior(),
					box Expr::UnaryOp(_, _) | box Expr::Apply(_, _) => true,
					_ => false,
				} {
					write!(f, "({})", expr0)?;
				} else {
					write!(f, "{}", expr0)?;
				}
				write!(f, " {} ", op.to_str())?;
				if match expr1 {
					box Expr::BinOp(op1, _, _) => op1.prior() < op.prior(),
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
			Expr::TupleIndex(expr, i) => write!(f, "{}.{}", expr, i),
			Expr::Union(unions) => fmt_list(f, unions, "union (", ")", " | ", |f, x| {
				write!(f, "{}", x.0)?;
				if let Some(ref expr) = x.1 {
					write!(f, " {}", expr)?;
				}
				Ok(())
			}),
			Expr::CaseOf(expr, cases) => {
				write!(f, "case {} of ", expr)?;
				fmt_list(f, cases, "(", ")", " | ", |f, x| {
					write!(f, "{} {} ⇒ {}", x.0, x.1, x.2)
				})
			}
			Expr::List(exprs) => {
				fmt_list(f, exprs, "[", "]", ", ", |f, expr| write!(f, "{}", expr))
			}
			Expr::MatchOf(expr0, expr1, id0, id1, expr2) => write!(
				f,
				"match {} of ([] ⇒ {} | {}::{} ⇒ {})",
				expr0, expr1, id0, id1, expr2
			),
			Expr::IfThenElse(cond, expr0, expr1) => {
				write!(f, "if {} then {} else {}", cond, expr0, expr1)
			}
			Expr::Identifier(id) => write!(f, "{}", id),
			Expr::Int(x) => write!(f, "{}", x),
			Expr::Bool(x) => write!(f, "{}", x),
		}
	}
}
