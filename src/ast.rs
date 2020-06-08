use crate::lexer::Op;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub struct AST {
	pub id: Option<String>,
	pub tyct: TypeContext,
	pub ty: Option<Type>,
	pub expr: Expr,
}

//----类型----

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
	Int,
	Bool,
	Func(Box<Type>, Box<Type>),
	Tuple(Vec<Type>),
	Union(Vec<Type>),
	List(Box<Type>),
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

//----运算符与表达式----

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

#[derive(Debug, Clone)]
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
			BinOp::Cons => "++",
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
			_ => None,
		}
	}

	pub fn prior(&self) -> i32 {
		match self {
			BinOp::Or => 0,
			BinOp::And => 1,
			BinOp::Eq | BinOp::NotEq | BinOp::Less | BinOp::LessEq | BinOp::Gre | BinOp::GreEq => 2,
			BinOp::Cons => 3,
			BinOp::Add | BinOp::Minus => 4,
			BinOp::Mult | BinOp::Divide => 5,
		}
	}
}

impl Op {
	pub fn is_binop(&self) -> bool {
		if let Some(_) = BinOp::from_op(self) {
			true
		} else {
			false
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

//----变量上下文、类型上下文----

#[derive(Debug, Clone)]
pub struct TypeContext {
	pub free: BTreeSet<String>,
	bounded: BTreeMap<String, Type>,
	counter: i32,
}

impl TypeContext {
	pub fn new() -> Self {
		TypeContext {
			free: BTreeSet::new(),
			bounded: BTreeMap::new(),
			counter: 0,
		}
	}

	pub fn new_with_free(free: BTreeSet<String>) -> Self {
		TypeContext {
			free,
			bounded: BTreeMap::new(),
			counter: 0,
		}
	}

	/// 添加新的自由变量
	pub fn add_free(&mut self, id: String) -> bool {
		self.free.insert(id)
	}

	/// 添加代换
	pub fn add_bound(&mut self, id: String, ty: Type) {
		self.free.remove(&id);
		self.bounded.insert(id, ty);
	}

	/// 生成一个新的自由变量，使用_tk命名
	pub fn gen_free(&mut self) -> String {
		let s = format!("_t{}", self.counter);
		self.counter += 1;
		self.free.insert(s.clone());
		s
	}

	/// 查询是否有对应的变量
	pub fn has_var(&self, id: &String) -> bool {
		self.free.get(id).is_some() || self.bounded.get(id).is_some()
	}

	/// 查询是否是被代换的受限变量
	pub fn get_bound(&self, id: &String) -> Option<&Type> {
		self.bounded.get(id)
	}

	/// 将类型参数重命名（数量较少时按照希腊字母，较多时按照_k），并自动生成新命名的替换规则。
	/// 由于lexer的id不能带有希腊字母或以下划线开头，所以这样的替换第一次总是不会冲突的；但要注意不能多次使用。
	pub fn rename(&mut self) {
		let ids: Vec<_> = self.free.iter().cloned().collect();
		self.free.clear();
		if ids.len() <= 25 {
			for (id, nid) in ids.into_iter().zip("αβγδεζηθικλμνξοπρςστυφχψω".chars())
			{
				self.free.insert(nid.to_string());
				self.add_bound(id, Type::Var(nid.to_string()));
			}
		} else {
			for (k, id) in ids.into_iter().enumerate() {
				let nid = format!("_{}", k);
				self.free.insert(nid.to_string());
				self.add_bound(id, Type::Var(nid.to_string()));
			}
		}
	}

	pub fn eprint_bound(&self) {
		for (id, ty) in &self.bounded {
			eprintln!("              {} = {}", id, ty);
		}
	}

	/// 根据一个被调用函数的类型上下文，将其类型进行实例化
	/// 通过替换该上下文中的自由变量，到一组新产生的自由变量，然后再将其类型简化得到
	/// 由于被调用函数一定是经过参数重命名和简化后的表达式，所以不会有变量名的冲突
	pub fn instant(&mut self, mut tyct: TypeContext, ty: Type) -> Type {
		let ids: Vec<_> = tyct.free.iter().cloned().collect();
		for id in ids {
			let nid = self.gen_free();
			tyct.add_bound(id, Type::Var(nid));
		}
		ty.simpl(&tyct)
	}

	pub fn flush_bounds(&mut self) {
		self.bounded.clear();
	}
}

impl Type {
	/// 将TypeContext中的所有代换应用到类型变量上，只剩下自由的类型变量
	pub fn simpl(self, tyct: &TypeContext) -> Self {
		match self {
			Type::Int | Type::Bool => self,
			Type::Func(ty0, ty1) => {
				Type::Func(Box::new(ty0.simpl(tyct)), Box::new(ty1.simpl(tyct)))
			}
			Type::Tuple(tys) => Type::Tuple(tys.into_iter().map(|x| x.simpl(tyct)).collect()),
			Type::Union(tys) => Type::Union(tys.into_iter().map(|x| x.simpl(tyct)).collect()),
			Type::List(ty) => Type::List(Box::new(ty.simpl(tyct))),
			Type::Var(ref id) => {
				if let Some(ty) = tyct.get_bound(id) {
					ty.clone().simpl(tyct)
				} else {
					self
				}
			}
		}
	}
}

impl Expr {
	/// 将TypeContext中的所有代换应用到类型变量上，只剩下自由的类型变量
	pub fn simpl(self, tyct: &TypeContext) -> Expr {
		match self {
			Expr::Lambda(id, ty, expr) => {
				Expr::Lambda(id, ty.simpl(tyct), Box::new(expr.simpl(tyct)))
			}
			Expr::Apply(expr0, expr1) => {
				Expr::Apply(Box::new(expr0.simpl(tyct)), Box::new(expr1.simpl(tyct)))
			}
			Expr::Fix(expr) => Expr::Fix(Box::new(expr.simpl(tyct))),
			Expr::UnaryOp(op, expr) => Expr::UnaryOp(op, Box::new(expr.simpl(tyct))),
			Expr::BinOp(op, expr0, expr1) => {
				Expr::BinOp(op, Box::new(expr0.simpl(tyct)), Box::new(expr1.simpl(tyct)))
			}
			Expr::Tuple(exprs) => Expr::Tuple(exprs.into_iter().map(|x| x.simpl(tyct)).collect()),
			Expr::TupleIndex(expr, index) => Expr::TupleIndex(Box::new(expr.simpl(tyct)), index),
			Expr::Union(unions) => Expr::Union(
				unions
					.into_iter()
					.map(|x| (x.0.simpl(tyct), x.1.map(|x| x.simpl(tyct))))
					.collect(),
			),
			Expr::CaseOf(expr, cases) => Expr::CaseOf(
				Box::new(expr.simpl(tyct)),
				cases
					.into_iter()
					.map(|x| (x.0.simpl(tyct), x.1, x.2.simpl(tyct)))
					.collect(),
			),
			Expr::List(exprs) => Expr::List(exprs.into_iter().map(|x| x.simpl(tyct)).collect()),
			Expr::Nil(expr) => Expr::Nil(Box::new(expr.simpl(tyct))),
			Expr::Head(expr) => Expr::Head(Box::new(expr.simpl(tyct))),
			Expr::Tail(expr) => Expr::Tail(Box::new(expr.simpl(tyct))),
			Expr::IfThenElse(cond, expr0, expr1) => Expr::IfThenElse(
				Box::new(cond.simpl(tyct)),
				Box::new(expr0.simpl(tyct)),
				Box::new(expr1.simpl(tyct)),
			),
			Expr::Identifier(_) | Expr::Int(_) | Expr::Bool(_) => self,
		}
	}
}

impl Display for TypeContext {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		if !self.free.is_empty() {
			for id in &self.free {
				write!(f, "∀ {} ", id)?;
			}
		}
		Ok(())
	}
}

pub struct Context {
	global: BTreeMap<String, (BTreeSet<String>, Type, Expr)>,
	local: Vec<(String, Type)>,
}

impl Context {
	pub fn new() -> Context {
		Context {
			global: BTreeMap::new(),
			local: Vec::new(),
		}
	}

	pub fn add(&mut self, id: String, tyct: BTreeSet<String>, ty: Type, expr: Expr) {
		self.global.insert(id, (tyct, ty, expr));
	}

	pub fn push(&mut self, id: String, ty: Type) {
		self.local.push((id, ty));
	}

	pub fn pop(&mut self) {
		self.local.pop();
	}

	pub fn pop_all(&mut self) {
		self.local.clear();
	}

	/// 获取某个标识符的类型，如果是全局标识符那么还会实例化
	pub fn get_type(&self, id0: &String, tyct0: &mut TypeContext) -> Option<Type> {
		for (id, ty) in self.local.iter().rev() {
			if id == id0 {
				return Some(ty.clone());
			}
		}
		self.global
			.get(id0)
			.map(|x| tyct0.instant(TypeContext::new_with_free(x.0.clone()), x.1.clone()))
	}

	pub fn get_expr(&self, id: &String) -> Option<&Expr> {
		self.global.get(id).map(|x| &x.2)
	}
}
