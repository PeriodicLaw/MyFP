use crate::ast::{Expr, Type};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter, Result};

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

	/// 根据一个被调用函数的类型上下文，将其类型进行实例化
	/// 通过替换该上下文中的自由变量，到一组新产生的自由变量，然后再将其类型简化得到
	/// 由于被调用函数一定是经过参数重命名和简化后的表达式，所以不会有变量名的冲突
	pub fn instant(&mut self, mut tyct: TypeContext, ty: Type, expr: Expr) -> (Type, Expr) {
		let ids: Vec<_> = tyct.free.iter().cloned().collect();
		for id in ids {
			let nid = self.gen_free();
			tyct.add_bound(id, Type::Var(nid));
		}
		(ty.simpl(&tyct), expr.simpl(&tyct))
	}

	pub fn flush_bounds(&mut self) {
		self.bounded.clear();
	}

	pub fn display_forall_vars(&self) -> String {
		let mut s = String::new();
		for id in &self.free {
			s.push_str(format!("∀ {} ", id).as_str());
		}
		s
	}
}

impl Display for TypeContext {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		if !self.free.is_empty() {
			for id in &self.free {
				write!(f, "{} ", id)?;
			}
			write!(f, "are free type variables")?;
		} else {
			write!(f, "there is no free type variables")?;
		}
		if !self.bounded.is_empty() {
			writeln!(f, ", and:")?;
			for (id, ty) in &self.bounded {
				writeln!(f, "                {} = {}", id, ty)?;
			}
		} else {
			writeln!(f)?;
		}
		Ok(())
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
			Expr::MatchOf(expr0, expr1, id0, id1, expr2) => Expr::MatchOf(
				Box::new(expr0.simpl(tyct)),
				Box::new(expr1.simpl(tyct)),
				id0,
				id1,
				Box::new(expr2.simpl(tyct)),
			),
			Expr::IfThenElse(cond, expr0, expr1) => Expr::IfThenElse(
				Box::new(cond.simpl(tyct)),
				Box::new(expr0.simpl(tyct)),
				Box::new(expr1.simpl(tyct)),
			),
			Expr::Identifier(_) | Expr::Int(_) | Expr::Bool(_) => self,
		}
	}
}

pub struct VarContext {
	global: BTreeMap<String, (BTreeSet<String>, Type, Expr)>,
	local: Vec<(String, Type)>,
}

impl VarContext {
	pub fn new() -> VarContext {
		VarContext {
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

	/// 获取局部标识符的类型
	pub fn get_local(&self, id: &String) -> Option<Type> {
		for (_id, ty) in self.local.iter().rev() {
			if id == _id {
				return Some(ty.clone());
			}
		}
		None
	}

	/// 获取全局标识符的类型和表达式，并且实例化
	pub fn get_global(&self, id0: &String, tyct0: &mut TypeContext) -> Option<(Type, Expr)> {
		self.global.get(id0).map(|x| {
			tyct0.instant(
				TypeContext::new_with_free(x.0.clone()),
				x.1.clone(),
				x.2.clone(),
			)
		})
	}

	pub fn get_expr(&self, id: &String) -> Option<&Expr> {
		self.global.get(id).map(|x| &x.2)
	}
}
