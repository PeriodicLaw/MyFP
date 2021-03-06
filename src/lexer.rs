use itertools::Itertools;
use std::iter::{Enumerate, Peekable};
use std::{fmt::Display, str::Chars};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
	// + - * /
	Add,
	Minus,
	Mult,
	Divide,
	// == != < <= > >=
	Eq,
	NotEq,
	Less,
	LessEq,
	Gre,
	GreEq,
	// && || ! :: ++
	And,
	Or,
	Not,
	Cons,
	Concat,
	// \ -> =>
	Lambda,
	To,
	DoubleTo,
}

impl Display for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Op::Add => write!(f, "'+'"),
			Op::Minus => write!(f, "'-'"),
			Op::Mult => write!(f, "'*'"),
			Op::Divide => write!(f, "'/'"),
			Op::Eq => write!(f, "'=='"),
			Op::NotEq => write!(f, "'!='"),
			Op::Less => write!(f, "'<'"),
			Op::LessEq => write!(f, "'<='"),
			Op::Gre => write!(f, "'>'"),
			Op::GreEq => write!(f, "'>='"),
			Op::And => write!(f, "'&&'"),
			Op::Or => write!(f, "'||'"),
			Op::Not => write!(f, "'!'"),
			Op::Cons => write!(f, "'::'"),
			Op::Concat => write!(f, "'++'"),
			Op::Lambda => write!(f, "'\' or 'lambda'"),
			Op::To => write!(f, "'->'"),
			Op::DoubleTo => write!(f, "'=>'"),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum KeyWord {
	Int,
	Bool,
	If,
	Then,
	Else,
	Union,
	Case,
	Match,
	Of,
	Fix,
	Let,
	Forall,
}

impl Display for KeyWord {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			KeyWord::Int => write!(f, "'Int'"),
			KeyWord::Bool => write!(f, "'Bool'"),
			KeyWord::If => write!(f, "'if'"),
			KeyWord::Then => write!(f, "'then'"),
			KeyWord::Else => write!(f, "'else'"),
			KeyWord::Union => write!(f, "'union'"),
			KeyWord::Case => write!(f, "'case'"),
			KeyWord::Match => write!(f, "'match'"),
			KeyWord::Of => write!(f, "'of'"),
			KeyWord::Fix => write!(f, "'fix'"),
			KeyWord::Let => write!(f, "'let'"),
			KeyWord::Forall => write!(f, "'forall'"),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	Identifier(String),
	KeyWord(KeyWord),
	Symbol(char), // ( ) [ ] = : , | _
	Op(Op),
	Int(i32),
	Bool(bool),
	End,
	Err,
}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::Identifier(id) => write!(f, "'{}'", id),
			Token::KeyWord(k) => write!(f, "{}", k),
			Token::Symbol(s) => write!(f, "'{}'", s),
			Token::Op(op) => write!(f, "{}", op),
			Token::Int(x) => write!(f, "'{}'", x),
			Token::Bool(x) => write!(f, "'{}'", x),
			Token::End => write!(f, "';'"),
			Token::Err => Ok(()),
		}
	}
}

pub struct TokenStream<'a> {
	chars: Peekable<Enumerate<Chars<'a>>>,
}

impl TokenStream<'_> {
	pub fn new<'a>(str: &'a String) -> TokenStream<'a> {
		TokenStream {
			chars: str.chars().enumerate().peekable(),
		}
	}
}

impl Iterator for TokenStream<'_> {
	type Item = (usize, Token); // 第一分量为该token对应到input的位置

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			return match self.chars.peek() {
				Some((_, c)) => match c {
					' ' | '\t' => {
						self.chars.next();
						continue;
					}
					'+' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '+')) => {
								self.chars.next();
								Some((p, Token::Op(Op::Concat)))
							}
							_ => Some((p, Token::Op(Op::Add))),
						}
					}
					'-' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '>')) => {
								self.chars.next();
								Some((p, Token::Op(Op::To)))
							}
							_ => Some((p, Token::Op(Op::Minus))),
						}
					}
					'*' => Some((self.chars.next()?.0, Token::Op(Op::Mult))),
					'/' => Some((self.chars.next()?.0, Token::Op(Op::Divide))),
					'<' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '=')) => {
								self.chars.next();
								Some((p, Token::Op(Op::LessEq)))
							}
							_ => Some((p, Token::Op(Op::Less))),
						}
					}
					'>' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '=')) => {
								self.chars.next();
								Some((p, Token::Op(Op::GreEq)))
							}
							_ => Some((p, Token::Op(Op::Gre))),
						}
					}
					'=' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '=')) => {
								self.chars.next();
								Some((p, Token::Op(Op::Eq)))
							}
							Some((_, '>')) => {
								self.chars.next();
								Some((p, Token::Op(Op::DoubleTo)))
							}
							_ => Some((p, Token::Symbol('='))),
						}
					}
					'&' => {
						let p = self.chars.next()?.0;
						if let Some((_, '&')) = self.chars.next() {
							Some((p, Token::Op(Op::And)))
						} else {
							eprintln!("lexer: expected &&, but found single &");
							Some((p, Token::Err))
						}
					}
					'|' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '|')) => {
								self.chars.next();
								Some((p, Token::Op(Op::Or)))
							}
							_ => Some((p, Token::Symbol('|'))),
						}
					}
					'!' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, '=')) => {
								self.chars.next();
								Some((p, Token::Op(Op::NotEq)))
							}
							_ => Some((p, Token::Op(Op::Not))),
						}
					}
					'\\' => Some((self.chars.next()?.0, Token::Op(Op::Lambda))),
					'_' => Some((self.chars.next()?.0, Token::Symbol('_'))),
					':' => {
						let p = self.chars.next()?.0;
						match self.chars.peek() {
							Some((_, ':')) => {
								self.chars.next();
								Some((p, Token::Op(Op::Cons)))
							}
							_ => Some((p, Token::Symbol(':'))),
						}
					}
					'[' | ']' | '(' | ')' | ',' | '.' => {
						let (p, c) = self.chars.next()?;
						Some((p, Token::Symbol(c)))
					}
					// 标识符必须由字母开头，但可以包括字母、数字、下划线
					'a'..='z' | 'A'..='Z' => {
						let p = self.chars.peek()?.0;
						let s: String = self
							.chars
							.by_ref()
							.peeking_take_while(|x| {
								let ref c = x.1;
								('a'..='z').contains(c)
									|| ('A'..='Z').contains(c) || ('0'..'9').contains(c)
									|| *c == '_'
							})
							.map(|x| x.1)
							.collect();
						Some((
							p,
							match &s[..] {
								"lambda" => Token::Op(Op::Lambda),
								"Int" => Token::KeyWord(KeyWord::Int),
								"Bool" => Token::KeyWord(KeyWord::Bool),
								"if" => Token::KeyWord(KeyWord::If),
								"then" => Token::KeyWord(KeyWord::Then),
								"else" => Token::KeyWord(KeyWord::Else),
								"union" => Token::KeyWord(KeyWord::Union),
								"case" => Token::KeyWord(KeyWord::Case),
								"of" => Token::KeyWord(KeyWord::Of),
								"match" => Token::KeyWord(KeyWord::Match),
								"fix" => Token::KeyWord(KeyWord::Fix),
								"let" => Token::KeyWord(KeyWord::Let),
								"forall" => Token::KeyWord(KeyWord::Forall),
								"true" => Token::Bool(true),
								"false" => Token::Bool(false),
								_ => Token::Identifier(s),
							},
						))
					}
					'0'..='9' => Some((
						self.chars.peek()?.0,
						Token::Int(
							self.chars
								.by_ref()
								.peeking_take_while(|x| {
									let ref c = x.1;
									('0'..='9').contains(c)
								})
								.map(|x| x.1)
								.collect::<String>()
								.parse::<i32>()
								.unwrap(),
						),
					)),
					';' => Some((self.chars.next()?.0, Token::End)),
					_ => {
						eprintln!("lexer: unrecognized char {}", c);
						Some((self.chars.peek()?.0, Token::Err))
					}
				},
				None => None,
			};
		}
	}
}
