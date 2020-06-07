use crate::ast::{BinOp, Context, Expr, Type, UnaryOp, AST};

impl Expr {
	fn typecheck(&self, context: &mut Context) -> Result<Type, ()> {
		match self {
			Expr::Lambda(id, ty0, expr) => {
				context.push(id.clone(), ty0.clone());
				let ty1 = expr.typecheck(context)?;
				context.pop();
				Ok(Type::Func(Box::new(ty0.clone()), Box::new(ty1)))
			}
			Expr::Apply(expr0, expr1) => {
				let ty0 = expr0.typecheck(context)?;
				let ty1 = expr1.typecheck(context)?;
				match ty0 {
					Type::Func(box tya, box tyb) if ty1 == tya => Ok(tyb),
					_ => {
						eprintln!("type checker: '{}' has type '{}', which can not applied to '{}' with type '{}'", expr0, ty0, expr1, ty1);
						Err(())
					}
				}
			}
			Expr::Fix(expr) => {
				let ty = expr.typecheck(context)?;
				match ty {
					Type::Func(box ty0, box ty1) if ty0 == ty1 => Ok(ty0),
					_ => {
						eprintln!("type checker: '{}' has type '{}', which is not like 'α→α' and can not be fixed", expr, ty);
						Err(())
					}
				}
			}
			Expr::UnaryOp(op, expr) => {
				let ty = expr.typecheck(context)?;
				match op {
					UnaryOp::Add | UnaryOp::Minus => {
						if ty == Type::Int {
							Ok(Type::Int)
						} else {
							eprintln!(
								"type checker: '{}' has type '{}', which is not integer",
								expr, ty
							);
							Err(())
						}
					}
					UnaryOp::Not => {
						if ty == Type::Bool {
							Ok(Type::Bool)
						} else {
							eprintln!(
								"type checker: '{}' has type '{}', which is not boolean",
								expr, ty
							);
							Err(())
						}
					}
				}
			}
			Expr::BinOp(op, expr0, expr1) => {
				let ty0 = expr0.typecheck(context)?;
				let ty1 = expr1.typecheck(context)?;
				match op {
					BinOp::Add | BinOp::Minus | BinOp::Mult | BinOp::Divide => {
						if ty0 == Type::Int && ty0 == Type::Int {
							Ok(Type::Int)
						} else {
							eprintln!("type checker: '{}' has type '{}', '{}' has type '{}', which are not both integer", expr0, ty0, expr1, ty1);
							Err(())
						}
					}
					BinOp::Eq
					| BinOp::NotEq
					| BinOp::Less
					| BinOp::LessEq
					| BinOp::Gre
					| BinOp::GreEq => {
						if (ty0 == Type::Int && ty1 == Type::Int)
							|| (ty0 == Type::Bool && ty1 == Type::Bool)
						{
							Ok(Type::Bool)
						} else {
							eprintln!("type checker: '{}' has type '{}', '{}' has type '{}', which are not both integer or both boolean", expr0, ty0, expr1, ty1);
							Err(())
						}
					}
					BinOp::And | BinOp::Or => {
						if ty0 == Type::Bool && ty1 == Type::Bool {
							Ok(ty0)
						} else {
							eprintln!("type checker: '{}' has type '{}', '{}' has type '{}', which are not both boolean", expr0, ty0, expr1, ty1);
							Err(())
						}
					}
					BinOp::Cons => {
						if let Type::List(_) = ty0 {
							if ty0 == ty1 {
								return Ok(ty0);
							}
						}
						eprintln!("type checker: '{}' has type '{}', '{}' has type '{}', which are not the same list type", expr0, ty0, expr1, ty1);
						Err(())
					}
				}
			}
			Expr::Tuple(exprs) => {
				let mut tys = vec![];
				for expr in exprs {
					tys.push(expr.typecheck(context)?)
				}
				Ok(Type::Tuple(tys))
			}
			Expr::TupleIndex(expr, index) => {
				let ty = expr.typecheck(context)?;
				if let Type::Tuple(tys) = &ty {
					if (0..tys.len()).contains(index) {
						Ok(tys[*index].clone())
					} else {
						eprintln!("type checker: '{}' has type '{}', which is a tuple of lenght {} and can not be indexed by {}", expr, ty, tys.len(), index);
						Err(())
					}
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', which is not a tuple",
						expr, ty
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
							let tyc = expr.typecheck(context)?;
							if ty != &tyc {
								eprintln!("type checker: '{}' has type '{}', but defined as '{}' in union", expr, tyc, ty);
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
				Ok(Type::Union(tys))
			}
			Expr::CaseOf(expr, cases) => {
				// expr: tye = Union(tys) 为被展开的union
				// tyc id => exprc: tyec 为其中某一个case，其在tye中的对应项为tyu
				let tye = expr.typecheck(context)?;
				if let Type::Union(tys) = &tye {
					if cases.len() == tys.len() {
						let it = cases.iter().zip(tys.iter());
						let mut ty = None;
						for ((tyc, id, exprc), tyu) in it {
							if tyc != tyu {
								eprintln!("type checker: type '{}' is not the same as corresponding type {} in '{}'", tyc, tyu, tye);
								return Err(());
							}
							context.push(id.clone(), tyc.clone());
							let tyec = exprc.typecheck(context)?;
							if let Some(ty) = &ty {
								if ty != &tyec {
									eprintln!("type checker: cases in '{}' have different types: '{}' and '{}'", self, ty, tyec);
									return Err(());
								}
							} else {
								// 第一个case
								ty = Some(tyec);
							}
							context.pop();
						}
						Ok(ty.unwrap())
					} else {
						eprintln!("type checker: '{}' has type '{}', which doesn't has the same lengths as cases in {}", expr, tye, self);
						Err(())
					}
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', which is not an union",
						expr, tye
					);
					Err(())
				}
			}
			Expr::List(exprs) => {
				let mut ty = None;
				for expr in exprs {
					let tye = expr.typecheck(context)?;
					if let Some(ty) = &ty {
						if ty != &tye {
							eprintln!(
								"type checker: terms in {} have different types: '{}' and '{}'",
								self, ty, tye
							);
							return Err(());
						}
					} else {
						ty = Some(tye);
					}
				}
				if ty == None {
					eprintln!("type checker: can not infer empty list's type (todo!)");
					todo!();
				}
				Ok(Type::List(Box::new(ty.unwrap())))
			}
			Expr::Nil(expr) => {
				let ty = expr.typecheck(context)?;
				if let Type::List(_) = ty {
					Ok(Type::Bool)
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', which is not a list",
						expr, ty
					);
					Err(())
				}
			}
			Expr::Head(expr) => {
				let ty = expr.typecheck(context)?;
				if let Type::List(box ty) = ty {
					Ok(ty)
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', which is not a list",
						expr, ty
					);
					Err(())
				}
			}
			Expr::Tail(expr) => {
				let ty = expr.typecheck(context)?;
				if let Type::List(_) = ty {
					Ok(ty)
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', which is not a list",
						expr, ty
					);
					Err(())
				}
			}
			Expr::IfThenElse(cond, expr0, expr1) => {
				let tyc = cond.typecheck(context)?;
				if tyc == Type::Bool {
					let ty0 = expr0.typecheck(context)?;
					let ty1 = expr1.typecheck(context)?;
					if ty0 == ty1 {
						Ok(ty0)
					} else {
						eprintln!("type checker: '{}' has type '{}', '{}' has type '{}', which are not the same", expr0, ty0, expr1, ty1);
						Err(())
					}
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', which is not a boolean",
						cond, tyc
					);
					Err(())
				}
			}
			Expr::Identifier(id) => {
				if let Some(ty) = context.get_type(id) {
					Ok(ty.clone())
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

impl AST {
	pub fn typecheck(&self, context: &mut Context) -> Result<Type, ()> {
		match self {
			AST::Let(_, Some(ty), expr) => {
				let tyc = expr.typecheck(context)?;
				if ty == &tyc {
					Ok(tyc)
				} else {
					eprintln!(
						"type checker: '{}' has type '{}', but defined as type '{}'",
						expr, tyc, ty
					);
					Err(())
				}
			}
			AST::Let(_, None, expr) | AST::Expr(expr) => expr.typecheck(context),
		}
	}
}
