use std::str::Chars;
use std::iter::{Enumerate, Peekable};
use peeking_take_while::PeekableExt;

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add, Minus, Mult, Divide, // + - * /
    Eq, NotEq, Less, LessEq, Gre, GreEq, // == != < <= > >=
    And, Or, Not, // && || !
    Cons, // ++
    Lambda, To, CaseTo // \ -> =>
}

impl Op {
    pub fn prior(&self) -> Option<i32> {
        match self {
            Op::Or => Some(0),
            Op::And => Some(1),
            Op::Eq | Op::NotEq | Op::Less | Op::LessEq | Op::Gre | Op::GreEq => Some(2),
            Op::Cons => Some(3),
            Op::Add | Op::Minus => Some(4),
            Op::Mult | Op::Divide => Some(5),
            _ => None
        }
    }
    
    // pub fn is_binop(&self) -> bool {
    //     match self {
    //         Op::Add | Op::Minus | Op::Mult | Op::Divide | Op::And | Op::Or | Op::Cons |
    //             Op::Eq | Op::NotEq |  => true,
    //         _ => false
    //     }
    // }
    
    // pub fn is_greater_or_eq(&self, other: &Op) -> bool {
    //     match other {
    //         Op::Or => true,
    //         Op::And => (*self != Op::Or),
    //         Op::Cons => (*self != Op::Or && *self != Op::And),
    //         Op::Add | Op::Minus => (*self != Op::Or && *self != Op::And && *self != Op::Cons),
    //         Op::Mult | Op::Divide => (*self == Op::Mult || *self == Op::Divide),
    //         _ => false
    //     }
    // }
}

#[derive(Debug, PartialEq, Clone)]
pub enum KeyWord {
    Int, Bool,
    If, Then, Else,
    Union, Case, Of,
    Nil, Head, Tail,
    Fix, Let
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    KeyWord(KeyWord),
    Symbol(char), // ( ) [ ] = : , |
    Op(Op),
    Int(i32),
    Bool(bool),
    End, Err
}
pub struct TokenStream<'a> {
    chars: Peekable<Enumerate<Chars<'a>>>
}

impl TokenStream<'_> {
    pub fn new<'a>(str: &'a String) -> TokenStream<'a> {
        TokenStream{
            chars: str.chars().enumerate().peekable()
        }
    }
}

impl Iterator for TokenStream<'_>{
    type Item = (usize, Token); // 第一分量为该token对应到input的位置
    
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            return match self.chars.peek() {
                Some((_, c)) => match c{
                    ' ' | '\t' => {
                        self.chars.next();
                        continue
                    }
                    
                    '+' => {
                        let p = self.chars.next()?.0;
                        match self.chars.peek() {
                            Some((_, '+')) => {
                                self.chars.next();
                                Some((p, Token::Op(Op::Cons)))
                            }
                            _ => Some((p, Token::Op(Op::Add)))
                        }
                    }
                    '-' => {
                        let p = self.chars.next()?.0;
                        match self.chars.peek() {
                            Some((_, '>')) => {
                                self.chars.next();
                                Some((p, Token::Op(Op::To)))
                            }
                            _ => Some((p, Token::Op(Op::Minus)))
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
                            _ => Some((p, Token::Op(Op::Less)))
                        }
                    }
                    '>' => {
                        let p = self.chars.next()?.0;
                        match self.chars.peek() {
                            Some((_, '=')) => {
                                self.chars.next();
                                Some((p, Token::Op(Op::GreEq)))
                            }
                            _ => Some((p, Token::Op(Op::Gre)))
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
                                Some((p, Token::Op(Op::CaseTo)))
                            }
                            _ => Some((p, Token::Symbol('=')))
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
                            _ => Some((p, Token::Symbol('|')))
                        }
                    }
                    '!' => {
                        let p = self.chars.next()?.0;
                        match self.chars.peek() {
                            Some((_, '=')) => {
                                self.chars.next();
                                Some((p, Token::Op(Op::NotEq)))
                            }
                            _ => Some((p, Token::Op(Op::Not)))
                        }
                    }
                    '\\' => Some((self.chars.next()?.0, Token::Op(Op::Lambda))),
                    
                    '[' | ']' | '(' | ')' | ':' | ',' | '.' => {
                        let (p, c) = self.chars.next()?;
                        Some((p, Token::Symbol(c)))
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let p = self.chars.peek()?.0;
                        let s: String = self.chars.by_ref().peeking_take_while(|x|{
                            let ref c = x.1;
                            ('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..'9').contains(c) || *c == '_'
                        }).map(|x|{x.1}).collect();
                        Some((p, match &s[..] {
                            "lambda" => Token::Op(Op::Lambda),
                            "Int" => Token::KeyWord(KeyWord::Int),
                            "Bool" => Token::KeyWord(KeyWord::Bool),
                            "if" => Token::KeyWord(KeyWord::If),
                            "then" => Token::KeyWord(KeyWord::Then),
                            "else" => Token::KeyWord(KeyWord::Else),
                            "union" => Token::KeyWord(KeyWord::Union),
                            "case" => Token::KeyWord(KeyWord::Case),
                            "of" => Token::KeyWord(KeyWord::Of),
                            "nil" => Token::KeyWord(KeyWord::Nil),
                            "head" => Token::KeyWord(KeyWord::Head),
                            "tail" => Token::KeyWord(KeyWord::Tail),
                            "fix" => Token::KeyWord(KeyWord::Fix),
                            "let" => Token::KeyWord(KeyWord::Let),
                            "true" => Token::Bool(true),
                            "false" => Token::Bool(false),
                            _ => Token::Identifier(s),
                        }))
                    }
                    '0'..='9' => {
                        Some((self.chars.peek()?.0, Token::Int(self.chars.by_ref().peeking_take_while(|x|{
                            let ref c = x.1;
                            ('0'..='9').contains(c)
                        }).map(|x|{x.1}).collect::<String>().parse::<i32>().unwrap())))
                    }
                    ';' => Some((self.chars.next()?.0, Token::End)),
                    _ => {
                        eprintln!("lexer: unrecognized char {}", c);
                        Some((self.chars.peek()?.0, Token::Err))
                    }
                }
                None => None
            }
        }
    }
}
