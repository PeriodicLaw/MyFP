use crate::ast::{BinOp, Context, Expr, Type, TypeContext, UnaryOp, AST};

impl AST {
	pub fn typecheck(&mut self, ct: &mut Context) -> Result<Type, ()> {
		let AST {
			id: _,
			tyct,
			ty,
			expr,
		} = self;
		let tyc = expr.typecheck(ct, tyct)?;
		if let Some(ty) = ty {
			unify(tyct, ty, &tyc)?;
		}
		Ok(tyc.simpl(tyct))
	}
}

impl Expr {
	/// 类型检查，推断出表达式的类型
	fn typecheck(&self, ct: &mut Context, tyct: &mut TypeContext) -> Result<Type, ()> {
		// println!("checking {} with {:?}", self, tyct);
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
				unify(tyct, &ty0, &ty)?;
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
				unify(tyct, &ty, &_ty)?;
				Ok(Type::Var(_id).simpl(tyct))
			}
			Expr::UnaryOp(op, expr) => {
				let ty = expr.typecheck(ct, tyct)?;
				match op {
					UnaryOp::Add | UnaryOp::Minus => {
						unify(tyct, &ty, &Type::Int)?;
						Ok(Type::Int)
					}
					UnaryOp::Not => {
						unify(tyct, &ty, &Type::Bool)?;
						Ok(Type::Bool)
					}
				}
			}
			Expr::BinOp(op, expr0, expr1) => {
				let ty0 = expr0.typecheck(ct, tyct)?;
				let ty1 = expr1.typecheck(ct, tyct)?;
				match op {
					BinOp::Add | BinOp::Minus | BinOp::Mult | BinOp::Divide => {
						unify(tyct, &ty0, &Type::Int)?;
						unify(tyct, &ty1, &Type::Int)?;
						Ok(Type::Int)
					}
					BinOp::Eq
					| BinOp::NotEq
					| BinOp::Less
					| BinOp::LessEq
					| BinOp::Gre
					| BinOp::GreEq => {
						unify(tyct, &ty0, &Type::Int)?;
						unify(tyct, &ty1, &Type::Int)?;
						Ok(Type::Bool)
					}
					BinOp::And | BinOp::Or => {
						unify(tyct, &ty0, &Type::Bool)?;
						unify(tyct, &ty1, &Type::Bool)?;
						Ok(Type::Bool)
					}
					BinOp::Cons => {
						// 构造类型[α]，然后与T0和T1合一
						let _ty = Type::List(Box::new(Type::Var(tyct.gen_free())));
						unify(tyct, &ty0, &_ty)?;
						unify(tyct, &ty1, &_ty)?;
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
						eprintln!("type checker: '{}' has type '{}', which is a tuple of lenght {} and can not be indexed by {}", expr, ty, tys.len(), index);
						Err(())
					}
				} else {
					// 我们的HM类型系统是无法推断x.0(其中x:α)的，因为无法表达变长的类型表达式
					eprintln!(
						"type checker: can not infer '{}' as a specific tuple type, which is the type of '{}'",
						ty, expr
					);
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
							unify(tyct, ty, &tyc)?;
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
				// expr: tye = Union(tys) 为被展开的union
				// tyc id => exprc: tyec 为其中某一个case，其在tye中的对应项为tyu
				let tye = expr.typecheck(ct, tyct)?;
				if let Type::Union(tys) = &tye {
					if cases.len() == tys.len() {
						let it = cases.iter().zip(tys.iter());
						let mut ty = None;
						for ((tyc, id, exprc), tyu) in it {
							unify(tyct, tyc, tyu)?;
							ct.push(id.clone(), tyc.clone());
							let tyec = exprc.typecheck(ct, tyct)?;
							if let Some(ty) = &ty {
								unify(tyct, ty, &tyec)?;
							} else {
								// 第一个case
								ty = Some(tyec);
							}
							ct.pop();
						}
						Ok(ty.unwrap().simpl(tyct))
					} else {
						eprintln!("type checker: '{}' has type '{}', which doesn't has the same lengths as cases in {}", expr, tye, self);
						Err(())
					}
				} else {
					// 同样，这里也无法推断case x of (...)，其中x:α
					eprintln!(
						"type checker: can not infer '{}' as a specific union type, which is the type of '{}'",
						tye, expr
					);
					Err(())
				}
			}
			Expr::List(exprs) => {
				// 构造类型α，然后与列表中的每一项合一
				let ty = Type::Var(tyct.gen_free());
				for expr in exprs {
					let tye = expr.typecheck(ct, tyct)?;
					unify(tyct, &ty, &tye)?;
				}
				Ok(Type::List(Box::new(ty)).simpl(tyct))
			}
			Expr::Nil(expr) => {
				let ty = expr.typecheck(ct, tyct)?;
				// 构造类型[α]，然后与T合一
				let _ty = Type::List(Box::new(Type::Var(tyct.gen_free())));
				unify(tyct, &ty, &_ty)?;
				Ok(Type::Bool)
			}
			Expr::Head(expr) => {
				let ty = expr.typecheck(ct, tyct)?;
				// 构造类型[α]，然后与T合一
				let _id = tyct.gen_free();
				let _ty = Type::List(Box::new(Type::Var(_id.clone())));
				unify(tyct, &ty, &_ty)?;
				Ok(Type::Var(_id).simpl(tyct))
			}
			Expr::Tail(expr) => {
				let ty = expr.typecheck(ct, tyct)?;
				// 构造类型[α]，然后与T合一
				let _ty = Type::List(Box::new(Type::Var(tyct.gen_free())));
				unify(tyct, &ty, &_ty)?;
				Ok(ty.simpl(tyct))
			}
			Expr::IfThenElse(cond, expr0, expr1) => {
				let tyc = cond.typecheck(ct, tyct)?;
				unify(tyct, &tyc, &Type::Bool)?;
				let ty0 = expr0.typecheck(ct, tyct)?;
				let ty1 = expr1.typecheck(ct, tyct)?;
				unify(tyct, &ty0, &ty1)?;
				Ok(ty0.simpl(tyct))
			}
			Expr::Identifier(id) => {
				if let Some(ty) = ct.get_type(id, tyct) {
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
fn unify(tyct: &mut TypeContext, ty0: &Type, ty1: &Type) -> Result<(), ()> {
	// println!("unifing {} and {} in {:?} where", ty0, ty1, tyct);tyct.eprint_bound();
	if _unify(tyct, ty0, ty1) {
		Ok(())
	} else {
		eprintln!(
			"              occurs when unifing type '{}' and '{}'",
			ty0, ty1
		);
		eprintln!("              where: ");
		tyct.eprint_bound();
		Err(())
	}
}

// 递归版本
fn _unify(tyct: &mut TypeContext, ty0: &Type, ty1: &Type) -> bool {
	// println!("unifing {} and {}", ty0, ty1);
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
			if occurs_check(tyct, ty1, id) {
				eprintln!(
					"type checker: occurs check failed, because '{}' occurs in '{}'",
					id, ty1
				);
				false
			} else {
				tyct.add_bound(id.clone(), ty1.clone());
				true
			}
		}
		(_, Type::Var(id)) => {
			if occurs_check(tyct, ty0, id) {
				eprintln!(
					"type checker: occurs check failed, because '{}' occurs in '{}'",
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
fn occurs_check(tyct: &TypeContext, ty: &Type, id: &String) -> bool {
	// println!("occurs checking {} in {}", id, ty);
	match ty {
		Type::Int | Type::Bool => false,
		Type::Func(ty0, ty1) => occurs_check(tyct, ty0, id) || occurs_check(tyct, ty1, id),
		Type::Tuple(tys) | Type::Union(tys) => {
			for ty in tys {
				if occurs_check(tyct, ty, id) {
					return true;
				}
			}
			false
		}
		Type::List(ty) => occurs_check(tyct, ty, id),
		Type::Var(_id) => {
			if let Some(ty) = tyct.get_bound(_id) {
				occurs_check(tyct, ty, id)
			} else {
				_id == id
			}
		}
	}
}
