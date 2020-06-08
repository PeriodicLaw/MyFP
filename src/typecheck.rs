use crate::ast::{BinOp, Expr, Type, UnaryOp, AST};
use crate::context::{TypeContext, VarContext};
use std::mem;

impl AST {
	/// 类型上下文生成，为语句中的类型变量生成类型上下文
	pub fn gen_type_context(&mut self) -> Result<TypeContext, ()> {
		let mut tyct = TypeContext::new();
		let tyvars = mem::replace(&mut self.tyvars, vec![]);
		for tyvar in tyvars {
			if !tyct.add_free(tyvar.clone()) {
				eprintln!("type checker: type variable '{}' repeated", tyvar);
				return Err(());
			}
		}
		if let Some(ty) = &mut self.ty {
			ty.gen_type_context(&mut tyct)?;
		}
		self.expr.gen_type_context(&mut tyct)?;
		Ok(tyct)
	}

	/// 类型推导，给定类型上下文，推导出该语句中表达式的类型
	pub fn typecheck(&mut self, ct: &mut VarContext, tyct: &mut TypeContext) -> Result<Type, ()> {
		let AST {
			id: _,
			tyvars: _,
			ty,
			expr,
		} = self;
		let tyc = expr.typecheck(ct, tyct)?;
		if let Some(ty) = ty {
			if !unify(tyct, ty, &tyc) {
				eprintln!(
					"type checker: unifing because '{}' defined as '{}'",
					expr, ty
				);
				return Err(());
			}
		}
		Ok(tyc.simpl(tyct))
	}
}

impl Type {
	fn gen_type_context(&mut self, tyct: &mut TypeContext) -> Result<(), ()> {
		match self {
			Type::Int | Type::Bool => Ok(()),
			Type::Func(ty0, ty1) => {
				ty0.gen_type_context(tyct)?;
				ty1.gen_type_context(tyct)
			}
			Type::Tuple(tys) | Type::Union(tys) => {
				for ty in tys {
					ty.gen_type_context(tyct)?;
				}
				Ok(())
			}
			Type::List(ty) => ty.gen_type_context(tyct),
			Type::Var(id) => {
				if id == "_" {
					let _ = mem::replace(id, tyct.gen_free());
					Ok(())
				} else if tyct.has_var(id) {
					Ok(())
				} else {
					eprintln!("type checker: type variable '{}' not defined", id);
					Err(())
				}
			}
		}
	}
}

impl Expr {
	fn gen_type_context(&mut self, tyct: &mut TypeContext) -> Result<(), ()> {
		match self {
			Expr::Lambda(_, ty, expr) => {
				ty.gen_type_context(tyct)?;
				expr.gen_type_context(tyct)
			}
			Expr::Fix(expr) | Expr::UnaryOp(_, expr) | Expr::TupleIndex(expr, _) => {
				expr.gen_type_context(tyct)
			}
			Expr::Apply(expr0, expr1) | Expr::BinOp(_, expr0, expr1) => {
				expr0.gen_type_context(tyct)?;
				expr1.gen_type_context(tyct)
			}
			Expr::MatchOf(expr0, expr1, _, _, expr2) | Expr::IfThenElse(expr0, expr1, expr2) => {
				expr0.gen_type_context(tyct)?;
				expr1.gen_type_context(tyct)?;
				expr2.gen_type_context(tyct)
			}
			Expr::Tuple(exprs) | Expr::List(exprs) => {
				for expr in exprs {
					expr.gen_type_context(tyct)?;
				}
				Ok(())
			}
			Expr::Union(unions) => {
				for (ty, expr) in unions {
					ty.gen_type_context(tyct)?;
					if let Some(expr) = expr {
						expr.gen_type_context(tyct)?;
					}
				}
				Ok(())
			}
			Expr::CaseOf(expr, cases) => {
				expr.gen_type_context(tyct)?;
				for (ty, _, expr) in cases {
					ty.gen_type_context(tyct)?;
					expr.gen_type_context(tyct)?;
				}
				Ok(())
			}
			Expr::Identifier(_) | Expr::Int(_) | Expr::Bool(_) => Ok(()),
		}
	}

	/// 类型检查，推断出表达式的类型
	/// 与此同时，需要进行函数调用的实例化
	fn typecheck(&mut self, ct: &mut VarContext, tyct: &mut TypeContext) -> Result<Type, ()> {
		match self {
			Expr::Lambda(id, ty0, expr) => {
				ct.push(id.clone(), ty0.clone());
				let ty1 = expr.typecheck(ct, tyct)?;
				ct.pop();
				Ok(Type::Func(Box::new(ty0.clone()), Box::new(ty1)).simpl(tyct))
			}
			Expr::Apply(expr0, expr1) => {
				let ty0 = expr0.typecheck(ct, tyct)?;
				let ty1 = expr1.typecheck(ct, tyct)?;
				// 构造类型T1 -> α，然后与T0合一
				let _id = tyct.gen_free();
				let ty = Type::Func(Box::new(ty1), Box::new(Type::Var(_id.clone())));
				if !unify(tyct, &ty0, &ty) {
					eprintln!(
						"type checker: unifing because applying '{}' into '{}'",
						expr0, expr1
					);
					return Err(());
				}
				Ok(Type::Var(_id).simpl(tyct))
			}
			Expr::Fix(expr) => {
				let ty = expr.typecheck(ct, tyct)?;
				// 构造类型α -> α，然后与T合一
				let _id = tyct.gen_free();
				let _ty = Type::Func(
					Box::new(Type::Var(_id.clone())),
					Box::new(Type::Var(_id.clone())),
				);
				if !unify(tyct, &ty, &_ty) {
					eprintln!("type checker: unifing because fix used on '{}'", expr);
					return Err(());
				}
				Ok(Type::Var(_id).simpl(tyct))
			}
			Expr::UnaryOp(op, expr) => {
				let ty = expr.typecheck(ct, tyct)?;
				match op {
					UnaryOp::Add | UnaryOp::Minus => {
						if !unify(tyct, &ty, &Type::Int) {
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}'",
								op.to_str(),
								expr
							);
							return Err(());
						}
						Ok(Type::Int)
					}
					UnaryOp::Not => {
						if !unify(tyct, &ty, &Type::Bool) {
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}'",
								op.to_str(),
								expr
							);
							return Err(());
						}
						Ok(Type::Bool)
					}
				}
			}
			Expr::BinOp(op, expr0, expr1) => {
				let ty0 = expr0.typecheck(ct, tyct)?;
				let ty1 = expr1.typecheck(ct, tyct)?;
				match op {
					BinOp::Add | BinOp::Minus | BinOp::Mult | BinOp::Divide => {
						if !unify(tyct, &ty0, &Type::Int) || !unify(tyct, &ty1, &Type::Int) {
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}' and '{}'",
								op.to_str(),
								expr0,
								expr1
							);
							return Err(());
						}
						Ok(Type::Int)
					}
					BinOp::Eq
					| BinOp::NotEq
					| BinOp::Less
					| BinOp::LessEq
					| BinOp::Gre
					| BinOp::GreEq => {
						if !unify(tyct, &ty0, &Type::Int) || !unify(tyct, &ty1, &Type::Int) {
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}' and '{}'",
								op.to_str(),
								expr0,
								expr1
							);
							return Err(());
						}
						Ok(Type::Bool)
					}
					BinOp::And | BinOp::Or => {
						if !unify(tyct, &ty0, &Type::Bool) || !unify(tyct, &ty1, &Type::Bool) {
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}' and '{}'",
								op.to_str(),
								expr0,
								expr1
							);
							return Err(());
						}
						Ok(Type::Bool)
					}
					BinOp::Cons => {
						// 构造类型[α]，然后T0与α、T1与[α]合一
						let _ty = Type::Var(tyct.gen_free());
						if !unify(tyct, &ty0, &_ty)
							|| !unify(tyct, &ty1, &Type::List(Box::new(_ty)))
						{
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}' and '{}'",
								op.to_str(),
								expr0,
								expr1
							);
							return Err(());
						}
						Ok(ty1.simpl(tyct))
					}
					BinOp::Concat => {
						// 构造类型[α]，然后与T0和T1合一
						let _ty = Type::List(Box::new(Type::Var(tyct.gen_free())));
						if !unify(tyct, &ty0, &_ty) || !unify(tyct, &ty1, &_ty) {
							eprintln!(
								"type checker: unifing because operator '{}' used on '{}' and '{}'",
								op.to_str(),
								expr0,
								expr1
							);
							return Err(());
						}
						Ok(_ty.simpl(tyct))
					}
				}
			}
			Expr::Tuple(exprs) => {
				let mut tys = vec![];
				for expr in exprs {
					tys.push(expr.typecheck(ct, tyct)?)
				}
				Ok(Type::Tuple(tys).simpl(tyct))
			}
			Expr::TupleIndex(expr, index) => {
				let ty = expr.typecheck(ct, tyct)?;
				if let Type::Tuple(tys) = &ty {
					if (0..tys.len()).contains(index) {
						Ok(tys[*index].clone().simpl(tyct))
					} else {
						eprintln!(
							"type checker: '{}' has type '{}', which is a tuple of lenght {} and can not be indexed by {}",
							expr, ty, tys.len(), index
						);
						Err(())
					}
				} else {
					// 我们的HM类型系统是无法推断x.0(其中x:α)的，因为无法表达变长的类型表达式
					eprintln!("type checker: can not infer '{}' as a specific tuple type, which is the type of '{}'", ty, expr);
					Err(())
				}
			}
			Expr::Union(unions) => {
				let mut tys = vec![];
				let mut has_expr = false;
				for (ty, expr) in unions {
					if let Some(expr) = expr {
						if has_expr {
							eprintln!("type checker: '{}' has more than 1 case", self);
							return Err(());
						} else {
							let tyc = expr.typecheck(ct, tyct)?;
							if !unify(tyct, ty, &tyc) {
								eprintln!(
									"type checker: unifing because '{}' defined as '{}' in union",
									expr, ty
								);
								return Err(());
							}
							has_expr = true;
						}
					}
					tys.push(ty.clone());
				}
				if !has_expr {
					eprintln!("type checker: '{}' has no case", self);
					return Err(());
				}
				Ok(Type::Union(tys).simpl(tyct))
			}
			Expr::CaseOf(expr, cases) => {
				let mut tycs = vec![]; // 每个case分支对应的类型
				let ty = Type::Var(tyct.gen_free()); // 返回的类型
				for (tyc, id, expr) in cases {
					tycs.push(tyc.clone());
					ct.push(id.clone(), tyc.clone());
					let _ty = expr.typecheck(ct, tyct)?;
					if !unify(tyct, &ty, &_ty) {
						eprintln!("type checker: unifing because expression '{}' should have the same type as other cases", expr);
						return Err(());
					}
					ct.pop();
				}
				let _ty = expr.typecheck(ct, tyct)?;
				if !unify(tyct, &ty, &_ty) {
					eprintln!("type checker: unifing because '{}' defined after case should have the same type as types in cases", expr);
					return Err(());
				}
				Ok(ty.simpl(tyct))
			}
			Expr::List(exprs) => {
				// 构造类型α，然后与列表中的每一项合一
				let ty = Type::Var(tyct.gen_free());
				for expr in exprs {
					let tye = expr.typecheck(ct, tyct)?;
					if !unify(tyct, &ty, &tye) {
						eprintln!("type checker: unifing because expression '{}' should have the same type as other list items", expr);
						return Err(());
					}
				}
				Ok(Type::List(Box::new(ty)).simpl(tyct))
			}
			Expr::MatchOf(expr0, expr1, id0, id1, expr2) => {
				let ty0 = expr0.typecheck(ct, tyct)?;
				// 构造类型[α]，然后与T0合一
				let _id = tyct.gen_free();
				let _ty = Type::List(Box::new(Type::Var(_id.clone())));
				if !unify(tyct, &ty0, &_ty) {
					eprintln!(
						"type checker: unifing because expression '{}' in match should be a list",
						expr0
					);
					return Err(());
				}
				// 让T1和T2合一
				let ty1 = expr1.typecheck(ct, tyct)?;
				ct.push(id0.clone(), Type::Var(_id));
				ct.push(id1.clone(), _ty);
				let ty2 = expr2.typecheck(ct, tyct)?;
				if !unify(tyct, &ty1, &ty2) {
					eprintln!("type checker: unifing because expression '{}' and '{}' in 2 match branches should have the same type", expr1, expr2);
					return Err(());
				}
				Ok(ty1.simpl(tyct))
			}
			Expr::IfThenElse(cond, expr0, expr1) => {
				let tyc = cond.typecheck(ct, tyct)?;
				if !unify(tyct, &tyc, &Type::Bool) {
					eprintln!(
						"type checker: unifing because the condition '{}' should be a boolean",
						cond
					);
					return Err(());
				}
				let ty0 = expr0.typecheck(ct, tyct)?;
				let ty1 = expr1.typecheck(ct, tyct)?;
				if !unify(tyct, &ty0, &ty1) {
					eprintln!("type checker: unifing because the expression '{}' in then branch and '{}' in else branch should have the same type", expr0, expr1);
					return Err(());
				}
				Ok(ty0.simpl(tyct))
			}
			Expr::Identifier(id) => {
				if let Some(ty) = ct.get_local(id) {
					Ok(ty.simpl(tyct))
				} else if let Some((ty, expr)) = ct.get_global(id, tyct) {
					let _ = mem::replace(self, expr);
					Ok(ty.simpl(tyct))
				} else {
					eprintln!("type checker: there is not an identifier called '{}'", id);
					Err(())
				}
			}
			Expr::Int(_) => Ok(Type::Int),
			Expr::Bool(_) => Ok(Type::Bool),
		}
	}
}

/// 合一代换：为ty0和ty1中出现的自由变量寻找一种代换，使得ty0和ty1相同
/// 完成合一代换后会自动为tyct添加代换规则
/// 为了方便报错，这里会调用一个递归版本的函数，如果出错则给出完整错误信息
fn unify(tyct: &mut TypeContext, ty0: &Type, ty1: &Type) -> bool {
	if _unify(tyct, ty0, ty1) {
		true
	} else {
		eprintln!(
			"              occurs when unifing type '{}' and '{}'",
			ty0, ty1
		);
		eprint!("              where {}", tyct);
		false
	}
}

// 递归版本
fn _unify(tyct: &mut TypeContext, ty0: &Type, ty1: &Type) -> bool {
	match (ty0, ty1) {
		(Type::Var(id0), Type::Var(id1)) if id0 == id1 => true,
		// 我们可能遇到一个自由变量自己与自己合一的情况，为了避免错误地产生代换，最开始就要将可能出现的代换全部做完
		(Type::Var(id), _) if tyct.get_bound(id).is_some() => {
			_unify(tyct, &tyct.get_bound(id).unwrap().clone(), ty1)
		}
		(_, Type::Var(id)) if tyct.get_bound(id).is_some() => {
			_unify(tyct, ty0, &tyct.get_bound(id).unwrap().clone())
		}
		(Type::Var(id), _) => {
			if occur_check(tyct, ty1, id) {
				eprintln!(
					"type checker: occuring check failed, because '{}' occurs in '{}'",
					id, ty1
				);
				false
			} else {
				tyct.add_bound(id.clone(), ty1.clone());
				true
			}
		}
		(_, Type::Var(id)) => {
			if occur_check(tyct, ty0, id) {
				eprintln!(
					"type checker: occuring check failed, because '{}' occurs in '{}'",
					id, ty0
				);
				false
			} else {
				tyct.add_bound(id.clone(), ty0.clone());
				true
			}
		}
		(Type::Int, Type::Int) => true,
		(Type::Bool, Type::Bool) => true,
		(Type::Func(ty00, ty01), Type::Func(ty10, ty11)) => {
			_unify(tyct, ty00, ty10) && _unify(tyct, ty01, ty11)
		}
		(Type::Tuple(tys0), Type::Tuple(tys1)) | (Type::Union(tys0), Type::Union(tys1)) => {
			if tys0.len() != tys1.len() {
				return false;
			}
			for (ty0, ty1) in tys0.iter().zip(tys1) {
				if !_unify(tyct, ty0, ty1) {
					return false;
				}
			}
			true
		}
		(Type::List(ty0), Type::List(ty1)) => _unify(tyct, ty0, ty1),
		_ => {
			eprintln!("type checker: '{}' and '{}' have different type", ty0, ty1);
			false
		}
	}
}

// 发生检查
fn occur_check(tyct: &TypeContext, ty: &Type, id: &String) -> bool {
	match ty {
		Type::Int | Type::Bool => false,
		Type::Func(ty0, ty1) => occur_check(tyct, ty0, id) || occur_check(tyct, ty1, id),
		Type::Tuple(tys) | Type::Union(tys) => {
			for ty in tys {
				if occur_check(tyct, ty, id) {
					return true;
				}
			}
			false
		}
		Type::List(ty) => occur_check(tyct, ty, id),
		Type::Var(_id) => {
			if let Some(ty) = tyct.get_bound(_id) {
				occur_check(tyct, ty, id)
			} else {
				_id == id
			}
		}
	}
}
