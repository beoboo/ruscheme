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
        let mut exprs = Vec::new();
        let mut it = tokens.iter();

        while let Some(token) = it.next() {
            match &token.token_type {
                TokenType::Number(n) => exprs.push(Expr::Number(*n)),
                TokenType::Operator(op) => exprs.push(self.expression(op, &mut it)),
                TokenType::EOF => {}
                t => return Err(format!("Undefined token type: {:?}", t))
            };
        }
        Ok(exprs)
    }

    fn expression(&self, operator: &String, it: &mut Iter<Token>) -> Expr {
        let mut operands: Vec<Expr> = Vec::new();

        while let Some(token) = it.next() {
            match &token.token_type {
                TokenType::Number(n) => operands.push(Expr::Number(*n)),
                TokenType::EOF => {}
                t => panic!("Undefined token type: {:?}", t)
            };
        }

        Expr::Expression(operator.clone(), operands)
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
        let tokens = vec![Token::new(TokenType::Number(123), 0), Token::new(TokenType::EOF, 0)];
        let exprs = parser.parse(tokens).unwrap();

        assert_that!(&exprs, len(1));
    }

    #[test]
    fn parse_operation() {
        let parser = Parser::new();
        let tokens = vec![
            Token::new(TokenType::Operator("+".to_string()), 0),
            Token::new(TokenType::Number(123), 0),
            Token::new(TokenType::Number(456), 0),
            Token::new(TokenType::EOF, 0)];
        let exprs = parser.parse(tokens).unwrap();

        assert_that!(&exprs, len(1));
        assert_that!(exprs[0].clone(), equal_to(Expr::Expression("+".to_string(), vec![Expr::Number(123), Expr::Number(456)])));
    }
//
//    fn assert_expression(expression: &Expr, expression_type: &Expr) {
//        assert_that!(&expression, equal_to(expression_type));
//        assert_that!(token.line, equal_to(line));
//    }

}