use crate::lexer::{Op, KeyWord, Token, TokenStream};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Type {
	Int, Bool,
	Func(Box<Type>, Box<Type>),
	Tuple(Vec<Type>),
	Union(Vec<Type>),
	List(Box<Type>)
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
	Bool(bool)
}

#[derive(Debug)]
pub enum AST {
	Let(String, Type, Expr),
	Expr(Expr)
}

pub struct Parser<'a> {
	lexer: Peekable<TokenStream<'a>>
}

impl Parser<'_> {
	pub fn new<'a>(input: &'a String) -> Parser<'a> {
		Parser{
			lexer: TokenStream::new(input).peekable()
		}
	}
	
	pub fn parse(&mut self) -> Result<AST, Option<usize>> {
		self.expect(Token::KeyWord(KeyWord::Let))?;
		let var = self.expect_identifier()?;
		self.expect(Token::Symbol(':'))?;
		let ty = self.parse_type()?;
		self.expect(Token::Symbol('='))?;
		let expr = self.parse_expr()?;
		Ok(AST::Let(var, ty, expr))
	}
	
	/// 解析外层类型，由若干函数类型组成
	fn parse_type(&mut self) -> Result<Type, Option<usize>> {
		let ty = self.parse_atom_type()?;
		if let Some((_, Token::Op(Op::To))) = self.peek() {
			self.consume();
			Ok(Type::Func(Box::new(ty), Box::new(self.parse_type()?)))
		} else {
			Ok(ty)
		}
	}
	
	/// 解析原子类型
	fn parse_atom_type(&mut self) -> Result<Type, Option<usize>> {
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
				let ty = self.parse_type()?;
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
						let mut tys = vec![ty, self.parse_type()?];
						while let Some((_, Token::Symbol(','))) = self.peek() {
							self.consume();
							tys.push(self.parse_type()?);
						}
						self.expect(Token::Symbol(')'))?;
						Ok(Type::Tuple(tys))
					}
					Some((_, Token::Symbol('|'))) => {
						let mut tys = vec![ty];
						while let Some((_, Token::Symbol('|'))) = self.peek() {
							self.consume();
							tys.push(self.parse_type()?);
						}
						self.expect(Token::Symbol(')'))?;
						Ok(Type::Union(tys))
					}
					x => {
						eprintln!("parser: expected , or | or ) in type list");
						Err(x.map(|x|{x.0}))
					}
				}
			}
			Some((_, Token::Symbol('['))) => {
				self.consume();
				let ty = self.parse_type()?;
				self.expect(Token::Symbol(']'))?;
				Ok(Type::List(Box::new(ty)))
			}
			x=> {
				eprintln!("parser: expected a type");
				Err(x.map(|x|{x.0}))
			}
		}
	}
	
	/// 解析最外层的表达式，由二元运算组成
	fn parse_expr(&mut self) -> Result<Expr, Option<usize>> {
		let mut expr_stack = vec![self.parse_term_expr()?];
		let mut op_stack: Vec<Op> = vec![];
		loop {
			match self.peek() {
				Some((_, Token::Op(op))) if op.prior().is_some() => {
					let op = op.clone();
					self.consume();
					
					while let Some(op0) = op_stack.last() {
						if let Some(p0) = op0.prior() {
							if let Some(p) = op.prior() {
								if p0 >= p {
									let op0 = op_stack.pop().unwrap();
									let op2 = expr_stack.pop().unwrap();
									let op1 = expr_stack.pop().unwrap();
									expr_stack.push(Expr::BinOp(op0, Box::new(op1), Box::new(op2)));
									continue;
								}
							}
						}
						break;
					}
					op_stack.push(op);
					expr_stack.push(self.parse_term_expr()?);
				}
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
	fn parse_term_expr(&mut self) -> Result<Expr, Option<usize>> {
		match self.peek() {
			Some((_, Token::Op(Op::Not))) => {
				self.consume();
				Ok(Expr::UnaryOp(Op::Not, Box::new(self.parse_term_expr()?)))
			}
			Some((_, Token::Op(Op::Add))) => {
				self.consume();
				Ok(Expr::UnaryOp(Op::Add, Box::new(self.parse_term_expr()?)))
			}
			Some((_, Token::Op(Op::Minus))) => {
				self.consume();
				Ok(Expr::UnaryOp(Op::Minus, Box::new(self.parse_term_expr()?)))
			}
			
			Some((_, Token::KeyWord(KeyWord::Nil))) => {
				self.consume();
				Ok(Expr::Nil(Box::new(self.parse_term_expr()?)))
			}
			Some((_, Token::KeyWord(KeyWord::Head))) => {
				self.consume();
				Ok(Expr::Head(Box::new(self.parse_term_expr()?)))
			}
			Some((_, Token::KeyWord(KeyWord::Tail))) => {
				self.consume();
				Ok(Expr::Tail(Box::new(self.parse_term_expr()?)))
			}
			Some((_, Token::KeyWord(KeyWord::Fix))) => {
				self.consume();
				Ok(Expr::Fix(Box::new(self.parse_term_expr()?)))
			}
			
			_ => {
				let mut expr = self.parse_suffix_expr()?;
				loop {
					if match self.peek() {
						// 这里特别指定了什么符号意味着一个项的结束，可能不太合适
						Some((_, token)) => match token {
							Token::Op(op) if op.prior().is_some() => true,
							Token::End | Token::Err => true,
							Token::Symbol(sym) => match sym {
								'|' | ',' | ')' | ']' => true,
								_ => false
							}
							Token::KeyWord(key) => match key {
								KeyWord::Then | KeyWord::Else | KeyWord::Of => true,
								_ => false
							}
							_ => false
						}
						None => true
					} {
						return Ok(expr);
					} else {
						expr = Expr::Apply(Box::new(expr), Box::new(self.parse_suffix_expr()?));
					}
				}
				
			}
		}
	}
	
	/// 解析后缀表达式
	fn parse_suffix_expr(&mut self) -> Result<Expr, Option<usize>> {
		let mut expr = self.parse_atom_expr()?;
		while let Some((_, Token::Symbol('.'))) = self.peek() {
			self.consume();
			let x = self.expect_int()?;
			expr = Expr::TupleIndex(Box::new(expr), x);
		}
		Ok(expr)
	}
	
	/// 解析原子表达式，由关键字表达式、字面值、括号、列表、标识符等组成
	fn parse_atom_expr(&mut self) -> Result<Expr, Option<usize>> {
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
				let expr = self.parse_expr()?;
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
						let mut exprs = vec![expr, self.parse_expr()?];
						while let Some((_, Token::Symbol(','))) = self.peek() {
							self.consume();
							exprs.push(self.parse_expr()?);
						}
						self.expect(Token::Symbol(')'))?;
						Ok(Expr::Tuple(exprs))
					}
					x => {
						eprintln!("parser: expected , or ) in tuple expression");
						Err(x.map(|x|{x.0}))
					}
				}
			}
			Some((_, Token::Symbol('['))) => {
				self.consume();
				let mut exprs = vec![self.parse_expr()?];
				while let Some((_, Token::Symbol(','))) = self.peek() {
					self.consume();
					exprs.push(self.parse_expr()?);
				}
				self.expect(Token::Symbol(']'))?;
				Ok(Expr::List(exprs))
			}
			
			Some((_, Token::KeyWord(KeyWord::Union))) => {
				self.consume();
				self.expect(Token::Symbol('('))?;
				let mut unions = vec![];
				loop {
					let ty = self.parse_type()?;
					let expr = match self.peek() {
						Some((_, Token::Symbol('|'))) | Some((_, Token::Symbol(')'))) => None,
						_ => Some(self.parse_expr()?)
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
							return Err(x.map(|x|{x.0}));
						}
					}
				}
				Ok(Expr::Union(unions))
			}
			Some((_, Token::KeyWord(KeyWord::Case))) => {
				self.consume();
				let expr = self.parse_expr()?;
				self.expect(Token::KeyWord(KeyWord::Of))?;
				self.expect(Token::Symbol('('))?;
				let mut cases = vec![];
				loop {
					let ty = self.parse_type()?;
					let id = self.expect_identifier()?;
					self.expect(Token::Op(Op::CaseTo))?;
					let expr = self.parse_expr()?;
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
							return Err(x.map(|x|{x.0}));
						}
					}
				}
				Ok(Expr::CaseOf(Box::new(expr), cases))
			}
			
			Some((_, Token::KeyWord(KeyWord::If))) => {
				self.consume();
				let expr0 = self.parse_expr()?;
				self.expect(Token::KeyWord(KeyWord::Then))?;
				let expr1 = self.parse_expr()?;
				self.expect(Token::KeyWord(KeyWord::Else))?;
				let expr2 = self.parse_expr()?;
				Ok(Expr::IfThenElse(Box::new(expr0), Box::new(expr1), Box::new(expr2)))
			}
			
			Some((_, Token::Op(Op::Lambda))) => {
				self.consume();
				let id = self.expect_identifier()?;
				self.expect(Token::Symbol(':'))?;
				let ty = self.parse_type()?;
				let expr = self.parse_expr()?;
				Ok(Expr::Lambda(id, ty, Box::new(expr)))
			}
			
			x => {
				eprintln!("parser: expected an expression");
				Err(x.map(|x|{x.0}))
			}
		}
	}
	
	
	// 用于简化词法处理的函数
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
			x=> {
				eprintln!("parser: expected an identifier");
				Err(x.map(|x|{x.0}))
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
			x=> {
				eprintln!("parser: expected an int");
				Err(x.map(|x|{x.0}))
			}
		}
	}
}