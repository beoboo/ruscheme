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
            TokenType::Bool(b) => Ok(Expr::Bool(*b)),
            TokenType::Number(n) => Ok(Expr::Number(*n)),
            TokenType::Identifier(s) => Ok(Expr::Identifier(s.clone())),
            TokenType::Define => self.definition(it),
            TokenType::Paren('(') => self.expression(it),
            t => Err(format!("Undefined token type: {}", t))
        }
    }

    fn definition(&self, it: &mut Iter<Token>) -> Result<Expr, String> {
        let token = match it.next() {
            Some(token) => token,
            None => return Err(format!("Expected name after define."))
        };

        let name = match &token.token_type {
            TokenType::Identifier(s) => s.to_string(),
            _ => return Err(format!("Expected name after define."))
        };

        let token = match it.next() {
            Some(token) => token,
            None => return Err(format!("Expected expression after name."))
        };

        let expr = match &token.token_type {
            TokenType::Number(n) => Expr::Number(*n),
            TokenType::Identifier(s) => Expr::Identifier(s.clone()),
            TokenType::Paren('(') => match self.expression(it) {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            _ => return Err(format!("Expected valid expression after name."))
        };

        Ok(Expr::Definition(name, Box::new(expr)))
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
                }
            };
        }

        Ok(Expr::Expression(list))
    }
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;
    use crate::lexer::Lexer;

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
    fn parse_definition() {
        let lexer = Lexer::new();
        let tokens = lexer.lex("(define a b)").unwrap();
        let parser = Parser::new();

        let exprs = parser.parse(tokens).unwrap();

        assert_that!(&exprs, len(1));
    }

    #[test]
    fn parse_operation() {
        let parser = Parser::new();
        let tokens = vec![
            Token::new(TokenType::Identifier("+".to_string()), 0),
            Token::new(TokenType::Number(123.0), 0),
            Token::new(TokenType::Number(456.0), 0),
            Token::new(TokenType::EOF, 0)];
        let exprs = parser.parse(tokens).unwrap();

        assert_that!(&exprs, len(3));
        assert_that!(exprs, equal_to(vec![Expr::Identifier("+".to_string()), Expr::Number(123.0), Expr::Number(456.0)]));
    }
}