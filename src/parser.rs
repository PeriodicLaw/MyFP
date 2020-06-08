use crate::ast::{BinOp, Expr, Type, TypeContext, UnaryOp, AST};
use crate::lexer::{KeyWord, Op, Token, TokenStream};
use std::iter::Peekable;

pub struct Parser<'a> {
	lexer: Peekable<TokenStream<'a>>,
}

impl Parser<'_> {
	pub fn new<'a>(input: &'a String) -> Parser<'a> {
		Parser {
			lexer: TokenStream::new(input).peekable(),
		}
	}

	/// 解析Token流为AST
	pub fn parse(&mut self) -> Result<AST, Option<usize>> {
		let mut tyct = TypeContext::new();
		let mut id = None;
		let mut ty = None;
		if let Some((_, Token::KeyWord(KeyWord::Let))) = self.peek() {
			self.consume();
			id = Some(self.expect_identifier()?);
			if let Some((_, Token::Symbol(':'))) = self.peek() {
				self.consume();
				while let Some((_, Token::KeyWord(KeyWord::Forall))) = self.peek() {
					self.consume();
					let tid = self.expect_identifier()?;
					if !tyct.add_free(tid.clone()) {
						// 这一步实际上已经在parser里做了一些语义的事情了，不是很合适
						eprintln!("parser: type variable identifier '{}' repeated", tid);
						return Err(self.peek().map(|x| x.0));
					}
				}
				ty = Some(self.parse_type(&mut tyct)?);
			}
			self.expect(Token::Symbol('='))?;
		}
		let expr = self.parse_expr(&mut tyct)?;
		Ok(AST { id, tyct, ty, expr })
	}

	/// 解析外层类型，由若干函数类型组成
	fn parse_type(&mut self, tyct: &mut TypeContext) -> Result<Type, Option<usize>> {
		let ty = self.parse_atom_type(tyct)?;
		if let Some((_, Token::Op(Op::To))) = self.peek() {
			self.consume();
			Ok(Type::Func(Box::new(ty), Box::new(self.parse_type(tyct)?)))
		} else {
			Ok(ty)
		}
	}

	/// 解析原子类型
	fn parse_atom_type(&mut self, tyct: &mut TypeContext) -> Result<Type, Option<usize>> {
		match self.peek() {
			Some((_, Token::KeyWord(KeyWord::Int))) => {
				self.consume();
				Ok(Type::Int)
			}
			Some((_, Token::KeyWord(KeyWord::Bool))) => {
				self.consume();
				Ok(Type::Bool)
			}
			Some((_, Token::Symbol('('))) => {
				self.consume();
				if let Some((_, Token::Symbol(')'))) = self.peek() {
					self.consume();
					return Ok(Type::Tuple(Vec::new()));
				}
				let ty = self.parse_type(tyct)?;
				match self.peek() {
					Some((_, Token::Symbol(')'))) => {
						self.consume();
						return Ok(ty);
					}
					Some((_, Token::Symbol(','))) => {
						self.consume();
						if let Some((_, Token::Symbol(')'))) = self.peek() {
							self.consume();
							return Ok(Type::Tuple(vec![ty]));
						}
						let mut tys = vec![ty, self.parse_type(tyct)?];
						while let Some((_, Token::Symbol(','))) = self.peek() {
							self.consume();
							tys.push(self.parse_type(tyct)?);
						}
						self.expect(Token::Symbol(')'))?;
						Ok(Type::Tuple(tys))
					}
					Some((_, Token::Symbol('|'))) => {
						let mut tys = vec![ty];
						while let Some((_, Token::Symbol('|'))) = self.peek() {
							self.consume();
							tys.push(self.parse_type(tyct)?);
						}
						self.expect(Token::Symbol(')'))?;
						Ok(Type::Union(tys))
					}
					x => {
						eprintln!("parser: expected , or | or ) in type list");
						Err(x.map(|x| x.0))
					}
				}
			}
			Some((_, Token::Symbol('['))) => {
				self.consume();
				let ty = self.parse_type(tyct)?;
				self.expect(Token::Symbol(']'))?;
				Ok(Type::List(Box::new(ty)))
			}
			Some((_, Token::Symbol('_'))) => {
				self.consume();
				Ok(Type::Var(tyct.gen_free()))
			}
			Some((x, Token::Identifier(s))) => {
				let s = s.clone();
				if tyct.has_var(&s) {
					self.consume();
					Ok(Type::Var(s))
				} else {
					eprintln!("parser: type variable '{}' not found in ct", s); // 这里也是做了一点语义上的分析
					Err(Some(*x))
				}
			}
			x => {
				eprintln!("parser: expected a type");
				Err(x.map(|x| x.0))
			}
		}
	}

	/// 解析最外层的表达式，由二元运算组成
	fn parse_expr(&mut self, tyct: &mut TypeContext) -> Result<Expr, Option<usize>> {
		let mut expr_stack = vec![self.parse_term_expr(tyct)?];
		let mut op_stack: Vec<BinOp> = vec![];
		loop {
			match self.peek() {
				// 遇到一个新的二元运算符，将栈中优先级更高的出栈
				Some((_, Token::Op(op))) if op.is_binop() => {
					let op = BinOp::from_op(op).unwrap();
					self.consume();

					while let Some(op0) = op_stack.last() {
						if op0.prior() >= op.prior() {
							let op0 = op_stack.pop().unwrap();
							let op2 = expr_stack.pop().unwrap();
							let op1 = expr_stack.pop().unwrap();
							expr_stack.push(Expr::BinOp(op0, Box::new(op1), Box::new(op2)));
							continue;
						}
						break;
					}
					op_stack.push(op);
					expr_stack.push(self.parse_term_expr(tyct)?);
					continue;
				}
				// 遇到非二元运算符，表明表达式结束
				_ => {
					while let Some(op0) = op_stack.pop() {
						let op2 = expr_stack.pop().unwrap();
						let op1 = expr_stack.pop().unwrap();
						expr_stack.push(Expr::BinOp(op0, Box::new(op1), Box::new(op2)));
					}
					return Ok(expr_stack.pop().unwrap());
				}
			}
		}
	}

	/// 解析项表达式，由前缀的一元运算符、函数调用组成
	fn parse_term_expr(&mut self, tyct: &mut TypeContext) -> Result<Expr, Option<usize>> {
		match self.peek() {
			Some((_, Token::Op(Op::Not))) => {
				self.consume();
				Ok(Expr::UnaryOp(
					UnaryOp::Not,
					Box::new(self.parse_term_expr(tyct)?),
				))
			}
			Some((_, Token::Op(Op::Add))) => {
				self.consume();
				Ok(Expr::UnaryOp(
					UnaryOp::Add,
					Box::new(self.parse_term_expr(tyct)?),
				))
			}
			Some((_, Token::Op(Op::Minus))) => {
				self.consume();
				Ok(Expr::UnaryOp(
					UnaryOp::Minus,
					Box::new(self.parse_term_expr(tyct)?),
				))
			}

			// Some((_, Token::KeyWord(KeyWord::Nil))) => {
			// 	self.consume();
			// 	Ok(Expr::Nil(Box::new(self.parse_term_expr(tyct)?)))
			// }
			// Some((_, Token::KeyWord(KeyWord::Head))) => {
			// 	self.consume();
			// 	Ok(Expr::Head(Box::new(self.parse_term_expr(tyct)?)))
			// }
			// Some((_, Token::KeyWord(KeyWord::Tail))) => {
			// 	self.consume();
			// 	Ok(Expr::Tail(Box::new(self.parse_term_expr(tyct)?)))
			// }
			Some((_, Token::KeyWord(KeyWord::Fix))) => {
				self.consume();
				Ok(Expr::Fix(Box::new(self.parse_term_expr(tyct)?)))
			}

			_ => {
				let mut expr = self.parse_suffix_expr(tyct)?;
				loop {
					if match self.peek() {
						// 这里特别指定了什么符号意味着一个项的结束，可能不太合适
						Some((_, token)) => match token {
							Token::Op(op) => op.is_binop(),
							Token::End | Token::Err => true,
							Token::Symbol(sym) => match sym {
								'|' | ',' | ')' | ']' => true,
								_ => false,
							},
							Token::KeyWord(key) => match key {
								KeyWord::Then | KeyWord::Else | KeyWord::Of => true,
								_ => false,
							},
							_ => false,
						},
						None => true,
					} {
						return Ok(expr);
					} else {
						expr = Expr::Apply(Box::new(expr), Box::new(self.parse_suffix_expr(tyct)?));
					}
				}
			}
		}
	}

	/// 解析后缀表达式
	fn parse_suffix_expr(&mut self, tyct: &mut TypeContext) -> Result<Expr, Option<usize>> {
		let mut expr = self.parse_atom_expr(tyct)?;
		while let Some((_, Token::Symbol('.'))) = self.peek() {
			self.consume();
			let x = self.expect_int()?;
			expr = Expr::TupleIndex(Box::new(expr), x as usize);
		}
		Ok(expr)
	}

	/// 解析原子表达式，由关键字表达式、字面值、括号、列表、标识符等组成
	fn parse_atom_expr(&mut self, tyct: &mut TypeContext) -> Result<Expr, Option<usize>> {
		match self.peek() {
			Some((_, Token::Int(x))) => {
				let x = *x;
				self.consume();
				Ok(Expr::Int(x))
			}
			Some((_, Token::Bool(x))) => {
				let x = *x;
				self.consume();
				Ok(Expr::Bool(x))
			}
			Some((_, Token::Identifier(id))) => {
				let id = id.clone();
				self.consume();
				Ok(Expr::Identifier(id))
			}

			Some((_, Token::Symbol('('))) => {
				self.consume();
				if let Some((_, Token::Symbol(')'))) = self.peek() {
					self.consume();
					return Ok(Expr::Tuple(Vec::new()));
				}
				let expr = self.parse_expr(tyct)?;
				match self.peek() {
					Some((_, Token::Symbol(')'))) => {
						self.consume();
						return Ok(expr);
					}
					Some((_, Token::Symbol(','))) => {
						self.consume();
						if let Some((_, Token::Symbol(')'))) = self.peek() {
							self.consume();
							return Ok(Expr::Tuple(vec![expr]));
						}
						let mut exprs = vec![expr, self.parse_expr(tyct)?];
						while let Some((_, Token::Symbol(','))) = self.peek() {
							self.consume();
							exprs.push(self.parse_expr(tyct)?);
						}
						self.expect(Token::Symbol(')'))?;
						Ok(Expr::Tuple(exprs))
					}
					x => {
						eprintln!("parser: expected , or ) in tuple expression");
						Err(x.map(|x| x.0))
					}
				}
			}
			Some((_, Token::Symbol('['))) => {
				self.consume();
				if let Some((_, Token::Symbol(']'))) = self.peek() {
					self.consume();
					return Ok(Expr::List(vec![]));
				}
				let mut exprs = vec![self.parse_expr(tyct)?];
				while let Some((_, Token::Symbol(','))) = self.peek() {
					self.consume();
					exprs.push(self.parse_expr(tyct)?);
				}
				self.expect(Token::Symbol(']'))?;
				Ok(Expr::List(exprs))
			}

			Some((_, Token::KeyWord(KeyWord::Union))) => {
				self.consume();
				self.expect(Token::Symbol('('))?;
				let mut unions = vec![];
				loop {
					let ty = self.parse_type(tyct)?;
					let expr = match self.peek() {
						Some((_, Token::Symbol('|'))) | Some((_, Token::Symbol(')'))) => None,
						_ => Some(self.parse_expr(tyct)?),
					};
					unions.push((ty, expr));
					match self.peek() {
						Some((_, Token::Symbol('|'))) => {
							self.consume();
							continue;
						}
						Some((_, Token::Symbol(')'))) => {
							self.consume();
							break;
						}
						x => {
							eprintln!("parser: expected | or ) in union expression");
							return Err(x.map(|x| x.0));
						}
					}
				}
				Ok(Expr::Union(unions))
			}
			Some((_, Token::KeyWord(KeyWord::Case))) => {
				self.consume();
				let expr = self.parse_expr(tyct)?;
				self.expect(Token::KeyWord(KeyWord::Of))?;
				self.expect(Token::Symbol('('))?;
				let mut cases = vec![];
				loop {
					let ty = self.parse_type(tyct)?;
					let id = self.expect_identifier()?;
					self.expect(Token::Op(Op::CaseTo))?;
					let expr = self.parse_expr(tyct)?;
					cases.push((ty, id, expr));
					match self.peek() {
						Some((_, Token::Symbol('|'))) => {
							self.consume();
							continue;
						}
						Some((_, Token::Symbol(')'))) => {
							self.consume();
							break;
						}
						x => {
							eprintln!("parser: expected | or ) in case expression");
							return Err(x.map(|x| x.0));
						}
					}
				}
				Ok(Expr::CaseOf(Box::new(expr), cases))
			}
			Some((_, Token::KeyWord(KeyWord::Match))) => {
				self.consume();
				let expr0 = self.parse_expr(tyct)?;
				self.expect(Token::KeyWord(KeyWord::Of))?;
				self.expect(Token::Symbol('('))?;
				self.expect(Token::Symbol('['))?;
				self.expect(Token::Symbol(']'))?;
				self.expect(Token::Op(Op::CaseTo))?;
				let expr1 = self.parse_expr(tyct)?;
				self.expect(Token::Symbol('|'))?;
				let id0 = self.expect_identifier()?;
				self.expect(Token::Op(Op::Cons))?;
				let id1 = self.expect_identifier()?;
				self.expect(Token::Op(Op::CaseTo))?;
				let expr2 = self.parse_expr(tyct)?;
				Ok(Expr::MatchOf(Box::new(expr0), Box::new(expr1), id0, id1, Box::new(expr2)))
			}

			Some((_, Token::KeyWord(KeyWord::If))) => {
				self.consume();
				let cond = self.parse_expr(tyct)?;
				self.expect(Token::KeyWord(KeyWord::Then))?;
				let expr0 = self.parse_expr(tyct)?;
				self.expect(Token::KeyWord(KeyWord::Else))?;
				let expr1 = self.parse_expr(tyct)?;
				Ok(Expr::IfThenElse(
					Box::new(cond),
					Box::new(expr0),
					Box::new(expr1),
				))
			}

			Some((_, Token::Op(Op::Lambda))) => {
				self.consume();
				let id = self.expect_identifier()?;
				let ty = if let Some((_, Token::Symbol(':'))) = self.peek() {
					self.consume();
					self.parse_type(tyct)?
				} else {
					Type::Var(tyct.gen_free())
				};
				let expr = self.parse_expr(tyct)?;
				Ok(Expr::Lambda(id, ty, Box::new(expr)))
			}

			x => {
				eprintln!("parser: expected an expression without prefix and suffix");
				Err(x.map(|x| x.0))
			}
		}
	}

	// 一些用于简化词法处理的函数
	fn consume(&mut self) {
		self.lexer.next();
	}

	fn peek(&mut self) -> Option<&(usize, Token)> {
		self.lexer.peek()
	}

	fn expect(&mut self, token: Token) -> Result<(), Option<usize>> {
		if let Some((p, t)) = self.peek() {
			if *t == token {
				self.consume();
				Ok(())
			} else {
				eprintln!("parser: expected token {:?}", token);
				Err(Some(*p))
			}
		} else {
			eprintln!("parser: expected token {:?}", token);
			Err(None)
		}
	}

	fn expect_identifier(&mut self) -> Result<String, Option<usize>> {
		match self.peek() {
			Some((_, Token::Identifier(s))) => {
				let s = s.clone();
				self.consume();
				Ok(s)
			}
			x => {
				eprintln!("parser: expected an identifier");
				Err(x.map(|x| x.0))
			}
		}
	}

	fn expect_int(&mut self) -> Result<i32, Option<usize>> {
		match self.peek() {
			Some((_, Token::Int(x))) => {
				let x = *x;
				self.consume();
				Ok(x)
			}
			x => {
				eprintln!("parser: expected an int");
				Err(x.map(|x| x.0))
			}
		}
	}
}
