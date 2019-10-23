use std::slice::Iter;

use crate::expr::*;
use crate::token::*;

#[derive(Debug)]
pub struct Parser {}

impl Parser {
    pub fn new() -> Parser {
        Parser {}
    }

    pub fn parse(&self, tokens: Vec<Token>) -> Result<Vec<Expr>, String> {
        if tokens.len() == 0 {
            return Err(format!("No tokens available"));
        }
        let mut it = tokens.iter();
        let mut exprs = Vec::new();

        while let Some(token) = it.next() {
            if token.is(TokenType::EOF) {
                break;
            }

            match self.parse_token(token, &mut it) {
                Ok(expr) => exprs.push(expr),
                Err(e) => return Err(e)
            }
        }
        Ok(exprs)
    }

    fn parse_token(&self, token: &Token, it: &mut Iter<Token>) -> Result<Expr, String> {
        match &token.token_type {
            TokenType::Number(n) => Ok(Expr::Number(*n)),
            TokenType::Symbol(s) => Ok(Expr::Symbol(s.clone())),
            TokenType::Paren('(') => self.expression(it),
            t => Err(format!("Undefined token type: {:?}", t))
        }
    }

    fn expression(&self, it: &mut Iter<Token>) -> Result<Expr, String> {
        let mut list: Vec<Expr> = Vec::new();

        while let Some(token) = it.next() {
            match &token.token_type {
                TokenType::Paren(')') => break,
                _ => {
                    match self.parse_token(token, it) {
                        Ok(expr) => list.push(expr),
                        Err(e) => return Err(e)
                    }
                },
            };
        }

        Ok(Expr::Expression(list))
    }
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn parse_empty() {
        let parser = Parser::new();
        assert_that!(parser.parse(vec![]).is_err(), is(true));
    }

    #[test]
    fn parse_number() {
        let parser = Parser::new();
        let tokens = vec![Token::new(TokenType::Number(123.0), 0), Token::new(TokenType::EOF, 0)];
        let exprs = parser.parse(tokens).unwrap();

        assert_that!(&exprs, len(1));
    }

    #[test]
    fn parse_operation() {
        let parser = Parser::new();
        let tokens = vec![
            Token::new(TokenType::Symbol("+".to_string()), 0),
            Token::new(TokenType::Number(123.0), 0),
            Token::new(TokenType::Number(456.0), 0),
            Token::new(TokenType::EOF, 0)];
        let exprs = parser.parse(tokens).unwrap();

        assert_that!(&exprs, len(3));
        assert_that!(exprs, equal_to(vec![Expr::Symbol("+".to_string()), Expr::Number(123.0), Expr::Number(456.0)]));
    }
}