use std::str::Chars;
use std::iter::Peekable;
use peeking_take_while::PeekableExt;

#[derive(Debug)]
pub enum Op {
    Add, Minus, Mult, Divide, // + - * /
    Eq, NotEq, Less, LessEq, Gre, GreEq, // == != < <= > >=
    And, Or, Not, // && || !
    Lambda, To // \ ->
}

#[derive(Debug)]
pub enum Token{
    Identifier(String),
    KeyWord(String),
    Symbol(char), // ( ) [ ] = : , |
    Op(Op),
    Int(i32),
    Bool(bool)
}
pub struct TokenStream<'a>{
    chars: Peekable<Chars<'a>>
}

impl<'a> TokenStream<'a>{
    pub fn new(str: &'a String) -> TokenStream<'a>{
        TokenStream{
            chars: str.chars().peekable()
        }
    }
}

impl<'a> Iterator for TokenStream<'a>{
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            return match self.chars.peek() {
                Some(c) => match c{
                    ' ' | '\t' | '\n' => {
                        self.chars.next();
                        continue
                    }
                    
                    '+' => {
                        self.chars.next();
                        Some(Token::Op(Op::Add))
                    }
                    '-' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('>') => {
                                self.chars.next();
                                Some(Token::Op(Op::To))
                            }
                            _ => Some(Token::Op(Op::Minus))
                        }
                    }
                    '*' => {
                        self.chars.next();
                        Some(Token::Op(Op::Mult))
                    }
                    '/' => {
                        self.chars.next();
                        Some(Token::Op(Op::Divide))
                    }
                    '<' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('=') => {
                                self.chars.next();
                                Some(Token::Op(Op::LessEq))
                            }
                            _ => Some(Token::Op(Op::Less))
                        }
                    }
                    '>' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('=') => {
                                self.chars.next();
                                Some(Token::Op(Op::GreEq))
                            }
                            _ => Some(Token::Op(Op::Gre))
                        }
                    }
                    '=' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('=') => {
                                self.chars.next();
                                Some(Token::Op(Op::Eq))
                            }
                            _ => Some(Token::Symbol('='))
                        }
                    }
                    '&' => {
                        self.chars.next();
                        if self.chars.next() != Some('&') {
                            println!("lexer: expected &&, but found single &");
                            None
                        } else {
                            Some(Token::Op(Op::And))
                        }
                    }
                    '|' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('|') => {
                                self.chars.next();
                                Some(Token::Op(Op::Or))
                            }
                            _ => Some(Token::Symbol('|'))
                        }
                    }
                    '!' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('=') => {
                                self.chars.next();
                                Some(Token::Op(Op::NotEq))
                            }
                            _ => Some(Token::Op(Op::Not))
                        }
                    }
                    '\\' => {
                        self.chars.next();
                        Some(Token::Op(Op::Lambda))
                    }
                    
                    '[' | ']' | '(' | ')' | ':' | ',' | '.' => Some(Token::Symbol(self.chars.next().unwrap())),
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let s: String = self.chars.by_ref().peeking_take_while(
                            |c|{('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..'9').contains(c) || *c == '_'}
                        ).collect();
                        match &s[..] {
                            "lambda" => Some(Token::Op(Op::Lambda)),
                            "let" | "fix" | "Int" | "Bool" => Some(Token::KeyWord(s)),
                            "true" => Some(Token::Bool(true)),
                            "false" => Some(Token::Bool(false)),
                            _ => Some(Token::Identifier(s))
                        }
                    }
                    '0'..='9' => {
                        Some(Token::Int(self.chars.by_ref().peeking_take_while(|c|{('0'..='9').contains(c)}).collect::<String>().parse::<i32>().unwrap()))
                    }
                    ';' => None,
                    _ => {
                        println!("lexer: unrecognized char {}", c);
                        None
                    }
                }
                None => None
            }
        }
    }
}