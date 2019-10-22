use std::iter::Peekable;
use crate::token::*;

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
    line: u32,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn lex(&mut self, source: &str) -> Result<Vec<Token>, String> {
        let mut it = source.chars().peekable();

        while let Some(&c) = it.peek() {
            match c {
                '0'..='9' => {
                    self.number(&mut it);
                }
                '+' | '-' | '*' | '/' => {
                    self.operator(&mut it);
                }
                '(' | ')' => {
                    it.next();
                    self.add_token(TokenType::Paren(c));
                }
                ' ' | '\t' => {
                    it.next();
                }
                '\n' => {
                    self.line += 1;
                    it.next();
                }
                _ => {
                    return Err(format!("Unexpected char: {}", c));
                }
            }
        }

        self.add_token(TokenType::EOF);
        Ok(self.tokens.clone())
    }

    fn number<T: Iterator<Item=char>>(&mut self, it: &mut Peekable<T>) {
        let mut number = 0;

        while let Some(Ok(digit)) = it.peek().map(|c| c.to_string().parse::<i64>()) {
            number *= 10;
            number += digit;
            it.next();
        }

        self.add_token(TokenType::Number(number));
    }

    fn operator<T: Iterator<Item=char>>(&mut self, it: &mut Peekable<T>) {
        let c = it.next().unwrap();
        let mut op = c.to_string();
        match c {
            '>' | '<' | '=' | '!' => {
                if let Some(&c1) = it.peek() {
                    match c1 {
                        '=' => { op.push(c1); }
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        self.add_token(TokenType::Operator(op));
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(token_type, self.line));
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn lex_empty() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("").unwrap();

        assert_that!(&tokens, len(1));
        assert_token(&tokens[0], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_numbers() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("123 456\n").unwrap();

        assert_that!(&tokens, len(3));
        assert_token(&tokens[0], &TokenType::Number(123), 1);
        assert_token(&tokens[1], &TokenType::Number(456), 1);
        assert_token(&tokens[2], &TokenType::EOF, 2);
    }

    #[test]
    fn lex_operators() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("+ - * /").unwrap();

        assert_that!(&tokens, len(5));
        assert_token(&tokens[0], &TokenType::Operator("+".to_string()), 1);
        assert_token(&tokens[1], &TokenType::Operator("-".to_string()), 1);
        assert_token(&tokens[2], &TokenType::Operator("*".to_string()), 1);
        assert_token(&tokens[3], &TokenType::Operator("/".to_string()), 1);
        assert_token(&tokens[4], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_paren() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("()").unwrap();

        assert_that!(&tokens, len(3));
        assert_token(&tokens[0], &TokenType::Paren('('), 1);
        assert_token(&tokens[1], &TokenType::Paren(')'), 1);
        assert_token(&tokens[2], &TokenType::EOF, 1);
    }

    fn assert_token(token: &Token, token_type: &TokenType, line: u32) {
        assert_that!(&token.token_type, equal_to(token_type));
        assert_that!(token.line, equal_to(line));
    }
}