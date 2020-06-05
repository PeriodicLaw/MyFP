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
	Abs(String, Type, Box<Expr>),
	Apply(Box<Expr>, Box<Expr>),
	UnaryOp(Op, Box<Expr>),
	BinOp(Op, Box<Expr>, Box<Expr>),
	Identifier(String)
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
	
	fn parse_type(&mut self) -> Result<Type, Option<usize>> {
		let ty = self.parse_atom_type()?;
		if let Some((_, Token::Op(Op::To))) = self.peek() {
			self.consume();
			Ok(Type::Func(Box::new(ty), Box::new(self.parse_type()?)))
		} else {
			Ok(ty)
		}
	}
	
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
						eprintln!("parser: expected , or | or )");
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
	
	fn parse_expr(&mut self) -> Result<Expr, Option<usize>> {
		Ok(Expr::Identifier("hahaha".to_string()))
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
}