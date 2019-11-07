use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    And,
    Bool(bool),
    Cond,
    Define,
    Else,
    Equal,
    Greater,
    GreaterEqual,
    Identifier(String),
    If,
    Lambda,
    Less,
    LessEqual,
    Let,
    Minus,
    Not,
    Number(f64),
    Or,
    Paren(char),
    Plus,
    Slash,
    Star,
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
            TokenType::Equal => write!(f, "="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::If => write!(f, "if"),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::Lambda => write!(f, "lambda"),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Let => write!(f, "let"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Not => write!(f, "not"),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::Or => write!(f, "or"),
            TokenType::Paren(p) => write!(f, "{}", p),
            TokenType::Plus => write!(f, "+"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}
