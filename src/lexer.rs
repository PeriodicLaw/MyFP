use std::str::Chars;
use std::iter::Peekable;
use peeking_take_while::PeekableExt;

#[derive(Debug)]
pub enum Token{
    Identifier(String),
    KeyWord(String),
    Symbol(char),
    To, // ->
    Lambda, // \ or lambda
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
                    ' ' | '\t' => {
                        self.chars.next();
                        continue
                    }
                    '-' => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some('>') => {
                                self.chars.next();
                                Some(Token::To)
                            }
                            _ => Some(Token::Symbol('-'))
                        }
                    }
                    '+' | '*' | '/' | '[' | ']' | '(' | ')' | '|' | '=' | ':' | ',' | '.' => Some(Token::Symbol(self.chars.next().unwrap())),
                    '\\' => {
                        self.chars.next();
                        Some(Token::Lambda)
                    }
                    ';' | '\n' => None,
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let s: String = self.chars.by_ref().peeking_take_while(
                            |c|{('a'..='z').contains(c) || ('A'..='Z').contains(c) || ('0'..'9').contains(c) || *c == '_'}
                        ).collect();
                        match &s[..] {
                            "lambda" => Some(Token::Lambda),
                            "let" | "fix" | "Int" | "Bool" => Some(Token::KeyWord(s)),
                            "true" => Some(Token::Bool(true)),
                            "false" => Some(Token::Bool(false)),
                            _ => Some(Token::Identifier(s))
                        }
                    }
                    '0'..='9' => {
                        Some(Token::Int(self.chars.by_ref().peeking_take_while(|c|{('0'..='9').contains(c)}).collect::<String>().parse::<i32>().unwrap()))
                    }
                    _ => panic!("")
                }
                None => None
            }
        }
    }
}