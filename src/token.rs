#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Bool(bool),
    Number(f64),
    Paren(char),
    Symbol(String),
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
