use std::fmt;
use std::iter::Peekable;
use std::slice::Iter;

use log::debug;

use crate::error::{Error, report_stage_error};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    And,
    Bool(bool),
    Cond,
    Define,
    Else,
    Identifier(String),
    If,
    Lambda,
    Let,
    Not,
    Number(f64),
    Or,
    Paren(char),
    String(String),
    EOF,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
}

impl Token {
    pub fn new(token_type: TokenType, line: u32) -> Token {
        Token {
            token_type,
            line,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::And => write!(f, "and"),
            TokenType::Bool(b) => write!(f, "{}", b),
            TokenType::Cond => write!(f, "cond"),
            TokenType::Define => write!(f, "define"),
            TokenType::Else => write!(f, "else"),
            TokenType::If => write!(f, "if"),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::Lambda => write!(f, "lambda"),
            TokenType::Let => write!(f, "let"),
            TokenType::Not => write!(f, "not"),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::Or => write!(f, "or"),
            TokenType::Paren(p) => write!(f, "{}", p),
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

type IterToken<'a> = Iter<'a, Token>;
pub(crate) type PeekableToken<'a> = Peekable<IterToken<'a>>;

pub fn advance(it: &mut PeekableToken) -> Result<TokenType, Error> {
    match it.next() {
        Some(token) => {
            Ok(token.token_type.clone())
        }
        None => Err(Error::Parser(format!("Token not found.")))
    }
}

pub fn consume<S: Into<String>>(token_type: TokenType, it: &mut PeekableToken, message: S) -> Result<(), Error> {
    debug!("Consuming: {}", token_type);

    let t = peek(it);
    if t == token_type {
        debug!("Consumed: {}", token_type);
        advance(it).unwrap();
        return Ok(());
    }

    if t == TokenType::EOF {
        return Err(Error::UnterminatedInput);
    }

    debug!("Found: {}", t);
    report_error(message.into())
}

pub fn peek(it: &mut PeekableToken) -> TokenType {
    match it.peek() {
        Some(token) => token.token_type.clone(),
        None => TokenType::EOF,
    }
}

fn report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    report_stage_error(err, "analyzer")
}
