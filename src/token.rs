use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Bool(bool),
    Number(f64),
    Paren(char),
    Identifier(String),
    Define,
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

    pub fn is(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Bool(b) => write!(f, "{}", b),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::Paren(p) => write!(f, "{}", p),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::Define => write!(f, "define"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}
