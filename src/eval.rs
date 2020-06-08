use crate::ast::{BinOp, Context, Expr, UnaryOp};

impl Expr {
	/// 化简求值，按照惰性求值的原则将表达式尽可能化简
	pub fn eval(self, ct: &Context) -> Result<Self, ()> {
		match self {
			Expr::Lambda(_, _, _) => Ok(self), // 惰性求值
			Expr::Apply(expr0, expr1) => {
				let expr0 = expr0.eval(ct)?;
				let expr1 = expr1.eval(ct)?;
				if let Expr::Lambda(id, _, expr) = expr0 {
					expr.subst(&id, &expr1).eval(ct)
				} else if let Expr::Fix(expr) = expr0 {
					// (fix F) x = F (fix F) x
					let expr = expr.eval(ct)?;
					Ok(Expr::Apply(
						Box::new(Expr::Apply(
							Box::new(expr.clone()),
							Box::new(Expr::Fix(Box::new(expr))),
						)),
						Box::new(expr1),
					)
					.eval(ct)?)
				} else {
					unreachable!()
				}
			}
			Expr::Fix(_) => Ok(self),
			Expr::UnaryOp(op, expr) => {
				let expr = expr.eval(ct)?;
				match op {
					UnaryOp::Add => Ok(expr),
					UnaryOp::Minus => {
						if let Expr::Int(x) = expr {
							Ok(Expr::Int(-x))
						} else {
							eprintln!("evaler: unable to eval '{}'", expr);
							Err(())
						}
					}
					UnaryOp::Not => {
						if let Expr::Bool(x) = expr {
							Ok(Expr::Bool(!x))
						} else {
							eprintln!("evaler: unable to eval '{}'", expr);
							Err(())
						}
					}
				}
			}
			Expr::BinOp(op, expr0, expr1) => {
				let expr0 = expr0.eval(ct)?;
				let expr1 = expr1.eval(ct)?;
				match op {
					BinOp::Add | BinOp::Minus | BinOp::Mult | BinOp::Divide => {
						if let Expr::Int(x0) = expr0 {
							if let Expr::Int(x1) = expr1 {
								match op {
									BinOp::Add => Ok(Expr::Int(x0 + x1)),
									BinOp::Minus => Ok(Expr::Int(x0 - x1)),
									BinOp::Mult => Ok(Expr::Int(x0 * x1)),
									BinOp::Divide => {
										if x1 == 0 {
											eprintln!("evaler: divider can not be zero");
											Err(())
										} else {
											Ok(Expr::Int(x0 / x1))
										}
									}
									_ => unreachable!(),
								}
							} else {
								eprintln!("evaler: unable to eval '{}'", expr1);
								Err(())
							}
						} else {
							eprintln!("evaler: unable to eval '{}'", expr0);
							Err(())
						}
					}
					BinOp::And | BinOp::Or => {
						if let Expr::Bool(x0) = expr0 {
							if let Expr::Bool(x1) = expr1 {
								match op {
									BinOp::And => Ok(Expr::Bool(x0 && x1)),
									BinOp::Or => Ok(Expr::Bool(x0 || x1)),
									_ => unreachable!(),
								}
							} else {
								eprintln!("evaler: unable to eval '{}'", expr1);
								Err(())
							}
						} else {
							eprintln!("evaler: unable to eval '{}'", expr0);
							Err(())
						}
					}
					BinOp::Eq
					| BinOp::NotEq
					| BinOp::Less
					| BinOp::LessEq
					| BinOp::Gre
					| BinOp::GreEq => {
						if let Expr::Int(x0) = expr0 {
							if let Expr::Int(x1) = expr1 {
								match op {
									BinOp::Eq => Ok(Expr::Bool(x0 == x1)),
									BinOp::NotEq => Ok(Expr::Bool(x0 != x1)),
									BinOp::Less => Ok(Expr::Bool(x0 < x1)),
									BinOp::LessEq => Ok(Expr::Bool(x0 >= x1)),
									BinOp::Gre => Ok(Expr::Bool(x0 > x1)),
									BinOp::GreEq => Ok(Expr::Bool(x0 >= x1)),
									_ => unreachable!(),
								}
							} else {
								eprintln!("evaler: unable to eval '{}'", expr1);
								Err(())
							}
						} else {
							eprintln!("evaler: unable to eval '{}'", expr0);
							Err(())
						}
					}
					BinOp::Cons => {
						if let Expr::List(mut exprs0) = expr0 {
							if let Expr::List(mut exprs1) = expr1 {
								exprs0.append(&mut exprs1);
								Ok(Expr::List(exprs0))
							} else {
								eprintln!("evaler: unable to eval '{}'", expr1);
								Err(())
							}
						} else {
							eprintln!("evaler: unable to eval '{}'", expr0);
							Err(())
						}
					}
				}
			}
			Expr::Tuple(exprs) => {
				let mut exprs0 = vec![];
				for expr in exprs {
					exprs0.push(expr.eval(ct)?);
				}
				Ok(Expr::Tuple(exprs0))
			}
			Expr::TupleIndex(expr, index) => {
				let expr = expr.eval(ct)?;
				if let Expr::Tuple(exprs) = expr {
					Ok(exprs[index].clone())
				} else {
					eprintln!(
						"evaler: unable to eval '{}'",
						Expr::TupleIndex(Box::new(expr), index)
					);
					Err(())
				}
			}
			Expr::Union(unions) => {
				let mut unions0 = vec![];
				for (ty, expr) in unions {
					if let Some(expr) = expr {
						unions0.push((ty, Some(expr.eval(ct)?)));
					} else {
						unions0.push((ty, None));
					}
				}
				Ok(Expr::Union(unions0))
			}
			Expr::CaseOf(expr, cases) => {
				let expr = expr.eval(ct)?;
				if let Expr::Union(unions) = expr {
					for ((_, expru), (_, id, exprc)) in unions.into_iter().zip(cases.into_iter()) {
						if let Some(expru) = expru {
							return Ok(exprc.subst(&id, &expru).eval(ct)?);
						}
					}
					unreachable!()
				} else {
					eprintln!(
						"evaler: unable to eval '{}'",
						Expr::CaseOf(Box::new(expr), cases)
					);
					Err(())
				}
			}
			Expr::List(exprs) => {
				let mut exprs0 = vec![];
				for expr in exprs {
					exprs0.push(expr.eval(ct)?);
				}
				Ok(Expr::List(exprs0))
			}
			Expr::Nil(expr) => {
				let expr = expr.eval(ct)?;
				if let Expr::List(exprs) = expr {
					Ok(Expr::Bool(exprs.is_empty()))
				} else {
					eprintln!("evaler: unable to eval '{}'", Expr::Nil(Box::new(expr)));
					Err(())
				}
			}
			Expr::Head(expr) => {
				let expr = expr.eval(ct)?;
				if let Expr::List(exprs) = expr {
					match exprs.into_iter().next() {
						Some(expr) => Ok(expr),
						_ => {
							eprintln!("evaler: cannot get the head of nil");
							Err(())
						}
					}
				} else {
					eprintln!("evaler: unable to eval '{}'", Expr::Head(Box::new(expr)));
					Err(())
				}
			}
			Expr::Tail(expr) => {
				let expr = expr.eval(ct)?;
				if let Expr::List(exprs) = expr {
					Ok(Expr::List(exprs.into_iter().skip(1).collect()))
				} else {
					eprintln!("evaler: unable to eval '{}'", Expr::Tail(Box::new(expr)));
					Err(())
				}
			}
			Expr::IfThenElse(cond, expr0, expr1) => {
				if let Expr::Bool(x) = cond.eval(ct)? {
					if x {
						expr0.eval(ct)
					} else {
						expr1.eval(ct)
					}
				} else {
					unreachable!()
				}
			}
			Expr::Identifier(id) => {
				if let Some(expr) = ct.get_expr(&id) {
					Ok(expr.clone())
				} else {
					eprintln!("evaler: cannot find expression named by {}", id);
					Err(())
				}
			}
			Expr::Int(_) => Ok(self),
			Expr::Bool(_) => Ok(self),
		}
	}

	/// 替换，将某一标识符所有自由出现的地方，全部替换为某个表达式
	fn subst(self, idsub: &String, exprsub: &Expr) -> Self {
		match self {
			Expr::Lambda(id, ty, expr) => {
				if &id == idsub {
					Expr::Lambda(id, ty, expr)
				} else {
					Expr::Lambda(id, ty, Box::new(expr.subst(idsub, exprsub)))
				}
			}
			Expr::Apply(expr0, expr1) => Expr::Apply(
				Box::new(expr0.subst(idsub, exprsub)),
				Box::new(expr1.subst(idsub, exprsub)),
			),
			Expr::Fix(expr) => Expr::Fix(Box::new(expr.subst(idsub, exprsub))),
			Expr::UnaryOp(op, expr) => {
				Expr::UnaryOp(op.clone(), Box::new(expr.subst(idsub, exprsub)))
			}
			Expr::BinOp(op, expr0, expr1) => Expr::BinOp(
				op.clone(),
				Box::new(expr0.subst(idsub, exprsub)),
				Box::new(expr1.subst(idsub, exprsub)),
			),
			Expr::Tuple(exprs) => {
				Expr::Tuple(exprs.into_iter().map(|x| x.subst(idsub, exprsub)).collect())
			}
			Expr::TupleIndex(expr, index) => {
				Expr::TupleIndex(Box::new(expr.subst(idsub, exprsub)), index)
			}
			Expr::Union(unions) => Expr::Union(
				unions
					.into_iter()
					.map(|x| (x.0, x.1.map(|x| x.subst(idsub, exprsub))))
					.collect(),
			),
			Expr::CaseOf(expr, cases) => Expr::CaseOf(
				Box::new(expr.subst(idsub, exprsub)),
				cases
					.into_iter()
					.map(|x| {
						if &x.1 == idsub {
							x
						} else {
							(x.0, x.1, x.2.subst(idsub, exprsub))
						}
					})
					.collect(),
			),
			Expr::List(exprs) => {
				Expr::List(exprs.into_iter().map(|x| x.subst(idsub, exprsub)).collect())
			}
			Expr::Nil(expr) => Expr::Nil(Box::new(expr.subst(idsub, exprsub))),
			Expr::Head(expr) => Expr::Head(Box::new(expr.subst(idsub, exprsub))),
			Expr::Tail(expr) => Expr::Tail(Box::new(expr.subst(idsub, exprsub))),
			Expr::IfThenElse(cond, expr0, expr1) => Expr::IfThenElse(
				Box::new(cond.subst(idsub, exprsub)),
				Box::new(expr0.subst(idsub, exprsub)),
				Box::new(expr1.subst(idsub, exprsub)),
			),
			Expr::Identifier(id) => {
				if &id == idsub {
					exprsub.clone()
				} else {
					Expr::Identifier(id)
				}
			}
			Expr::Int(_) => self,
			Expr::Bool(_) => self,
		}
	}
}
