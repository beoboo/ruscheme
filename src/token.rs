#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Number(i64),
    Paren(char),
    Operator(String),
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
