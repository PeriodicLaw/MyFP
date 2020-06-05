use crate::lexer::{Op, Token, TokenStream};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Type {
	Int, Bool,
	Func(Box<Type>, Box<Type>),
	Tuple(Vec<Type>),
	Union(Vec<Type>)
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
		self.expect(Token::KeyWord("let".to_string()))?;
		let var = self.expect_identifier()?;
		self.expect(Token::Symbol(':'))?;
		let ty = self.parse_type()?;
		self.expect(Token::Symbol('='))?;
		let expr = self.parse_expr()?;
		Ok(AST::Let(var, ty, expr))
	}
	
	fn parse_type(&mut self) -> Result<Type, Option<usize>> {
		panic!()
	}
	
	fn parse_expr(&mut self) -> Result<Expr, Option<usize>> {
		panic!()
	}
	
	
	// 用于简化词法处理的函数
	
    fn expect(&mut self, token: Token) -> Result<(), Option<usize>> {
		if let Some((p, t)) = self.lexer.peek() {
			if *t == token {
				self.lexer.next();
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
		match self.lexer.peek() {
			Some((_, Token::Identifier(s))) => {
				let s = s.clone();
				self.lexer.next();
				Ok(s)
			}
			Some((p, _)) => {
				eprintln!("parser: expected an identifier");
				Err(Some(*p))
			}
			None => {
				eprintln!("parser: expected an identifer");
				Err(None)
			}
		}
	}
}