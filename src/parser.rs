use std::iter::Peekable;
use std::slice::Iter;
use log::debug;

use crate::expr::*;
use crate::token::*;

#[derive(Debug)]
pub struct Parser {}

type IterToken<'a> = Iter<'a, Token>;
type PeekableToken<'a> = Peekable<IterToken<'a>>;

impl Parser {
    pub fn new() -> Parser {
        Parser {}
    }

    pub fn parse(&self, tokens: Vec<Token>) -> Result<Expr, String> {
        if tokens.len() == 0 {
            return Err(format!("No tokens available"));
        }

        let mut it = tokens.iter().peekable();
        let mut exprs = Vec::new();

        loop {
            let token_type = peek(&mut it);
//            println!("Token: {}", token_type);
            if token_type == TokenType::EOF {
                break;
            }

            match self.expression(&mut it) {
                Ok(expr) => exprs.push(expr),
                Err(e) => return Err(e)
            };
        }

        Ok(Expr::List(exprs))
    }

    fn expression(&self, it: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        debug!("expression");
        let token_type = match advance(it) {
            Ok(t) => t,
            Err(e) => return Err(e)
        };

        debug!("expression \"{}\"", token_type);
        match token_type {
            TokenType::Bool(b) => Ok(Expr::Bool(b)),
            TokenType::Identifier(i) => Ok(Expr::Identifier(i)),
            TokenType::Number(n) => Ok(Expr::Number(n)),

            TokenType::Paren('(') => self.form(it),
            t => Err(format!("Undefined token type: {}.", t))
        }
    }

    fn form(&self, it: &mut Peekable<Iter<Token>>) -> Result<Expr, String> {
        debug!("form");
        if peek(it) == TokenType::Paren(')') {
            // Consume the ')'.
            advance(it).unwrap();

            return Ok(Expr::Empty);
        }

        let res = match advance(it) {
            Ok(TokenType::Define) => self.definition(it),
            Ok(TokenType::Cond) => self.condition(it),
            Ok(TokenType::Identifier(i)) => self.function_call(i, it),
            Ok(t) => Err(format!("Undefined token type: {}.", t)),
            Err(e) => Err(e),
        };

        // Consume the ')'.
        if advance(it).is_err() {
            debug!("Consumung ')'");
            return Err(format!("Expected ')' after form."));
        }
        res
    }

    fn condition(&self, it: &mut PeekableToken) -> Result<Expr, String> {
        debug!("condition");
        let mut predicates = Vec::new();

        loop {
            match peek(it) {
                TokenType::Paren('(') => {
                    // Consume the '('.
                    advance(it).unwrap();

                    match self.predicate(it) {
                        Ok(predicate) => predicates.push(predicate),
                        Err(e) => return Err(e)
                    }
                }
                TokenType::Paren(')') => break,
                t => return Err(format!("Undefined token type: {}", t))
            };
        }

        debug!("Predicates {:?}", predicates);

        Ok(Expr::Conditional(predicates))
    }

    fn function_call(&self, name: String, it: &mut PeekableToken) -> Result<Expr, String> {
        debug!("function call: {}", name);
        let args = match self.build_arguments(it) {
            Ok(args) => args,
            Err(e) => return Err(e)
        };

        Ok(Expr::Expression(name, args))
    }

    fn predicate(&self, it: &mut PeekableToken) -> Result<Expr, String> {
        debug!("predicate");
        let mut exprs = Vec::new();
        let test = match self.expression(it) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };

        loop {
            match peek(it) {
                TokenType::Paren(')') => break,
                _ => match self.expression(it) {
                    Ok(expr) => exprs.push(expr),
                    Err(e) => return Err(e)
                }
            }
        }

        // Consume the ')'.
        if advance(it).is_err() {
            return Err(format!("Expected ')' after predicate."));
        }

        Ok(Expr::Predicate(Box::new(test), exprs))
    }

    fn definition(&self, it: &mut PeekableToken) -> Result<Expr, String> {
        debug!("definition");
        match advance(it) {
            Ok(TokenType::Identifier(i)) => self.define_expression(&i, it),
            Ok(TokenType::Paren('(')) => self.define_procedure(it),
            t => return Err(format!("Expected name or '(' after define (found: {:?}).", t))
        }
    }

    fn define_expression(&self, name: &str, it: &mut PeekableToken) -> Result<Expr, String> {
        debug!("define expression: {}", name);
        let expr = match advance(it) {
            Ok(TokenType::Number(n)) => Expr::Number(n),
            Ok(TokenType::Identifier(i)) => Expr::Identifier(i),
            Ok(TokenType::Paren('(')) => {
                let token_type = match advance(it) {
                    Ok(t) => t,
                    Err(e) => return Err(e),
                };

                let expr = match token_type {
                    TokenType::Identifier(i) => match self.function_call(i, it) {
                        Ok(e) => e,
                        Err(e) => return Err(e)
                    }
                    _ => return Err(format!("Expected expression name."))
                };

                // Consume the ')'.
                if advance(it).is_err() {
                    return Err(format!("Expected ')' after expression."));
                }

                expr
            }
            _ => return Err(format!("Expected expression after name."))
        };

        Ok(Expr::Definition(name.to_string(), Box::new(expr)))
    }

    fn define_procedure(&self, it: &mut PeekableToken) -> Result<Expr, String> {
        let name = match advance(it) {
            Ok(TokenType::Identifier(i)) => Expr::Identifier(i),
            _ => return Err(format!("Expected procedure name."))
        };
        debug!("procedure \"{}\"", name);

        let params = match self.build_arguments(it) {
            Ok(list) => list,
            Err(e) => return Err(e)
        };

        // Consume the ')'.
        if advance(it).is_err() {
            return Err(format!("Expected ')' after procedure parameters."));
        }

//
//        match peek(it) {
//            Some(token) => token,
//            None => return Err(format!("Expected procedure body."))
//        };

        let expr = match self.expression(it) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };
//
//        // Consume the ')'.
//        if advance(it).is_err() {
//            return Err(format!("Expected ')' after procedure."));
//        }

        let procedure = Expr::Procedure(name.to_string(), params, Box::new(expr));

        Ok(Expr::Definition(name.to_string(), Box::new(procedure)))
    }

    fn build_arguments(&self, it: &mut PeekableToken) -> Result<Vec<Expr>, String> {
        let mut args: Vec<Expr> = Vec::new();

        loop {
            let token_type = peek(it);

            match token_type {
                TokenType::Paren(')') => break,
                _ => match self.expression(it) {
                    Ok(expr) => args.push(expr),
                    Err(e) => return Err(e)
                }
            }
        }

        Ok(args)
    }
}

fn advance(it: &mut PeekableToken) -> Result<TokenType, String> {
    match it.next() {
        Some(token) => {
            debug!("Token: {}", token.token_type);
            Ok(token.token_type.clone())
        },
        None => Err(format!("Token not found."))
    }
}

fn peek(it: &mut PeekableToken) -> TokenType {
    match it.peek() {
        Some(token) => token.token_type.clone(),
        None => TokenType::EOF,
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn parse_empty() {
        let parser = Parser::new();
        assert_that!(parser.parse(vec ! []).is_err(), is(true));
    }

    #[test]
    fn parse_invalid() {
        assert_invalid("define", "Cannot execute \"1\".");
        assert_invalid("(1)", "Cannot execute \"1\".");
    }

    #[test]
    fn parse_primitives() {
        assert_parse("123", Expr::List(vec![Expr::Number(123.0)]));
        assert_parse("true", Expr::List(vec![Expr::Bool(true)]));
        assert_parse("false", Expr::List(vec![Expr::Bool(false)]));
        assert_parse("+", Expr::List(vec![Expr::Identifier("+".to_string())]));
    }

    #[test]
    fn parse_empty_list() {
        let expr = parse("()").unwrap();

        assert_that!(expr, equal_to(Expr::List(vec ! [Expr::Empty])));
    }

    #[test]
    fn parse_expressions() {
        assert_parse("(one)", Expr::List(vec![Expr::Expression("one".to_string(), vec![])]));

        assert_parse("(+ 1 2)", Expr::List(vec![Expr::Expression("+".to_string(), vec![
            Expr::Number(1.0), Expr::Number(2.0)
        ])]));
    }

    #[test]
    fn parse_definitions() {
//        assert_parse("(define a 1)", Expr::List(vec![Expr::Definition("a".to_string(), Box::new(Expr::Number(1.0)))]));
//        assert_parse("(define a b)", Expr::List(vec![Expr::Definition("a".to_string(), Box::new(Expr::Identifier("b".to_string())))]));
        assert_parse("(define a (+ 1))", Expr::List(vec![
            Expr::Definition("a".to_string(), Box::new(Expr::Expression("+".to_string(), vec![
                Expr::Number(1.0)
            ])))]));
        assert_parse("(define (one) 1)",
                     Expr::List(vec![
                         Expr::Definition(
                             "one".to_string(),
                             Box::new(Expr::Procedure(
                                 "one".to_string(),
                                 vec![],
                                 Box::new(Expr::Number(1.0))
                             ))
                         )
                     ])
        );
        assert_parse("(define (square x) (* x x))",
                     Expr::List(vec![
                         Expr::Definition(
                             "square".to_string(),
                             Box::new(Expr::Procedure(
                                 "square".to_string(),
                                 vec![Expr::Identifier("x".to_string())],
                                 Box::new(Expr::Expression(
                                     "*".to_string(),
                                     vec![Expr::Identifier("x".to_string()), Expr::Identifier("x".to_string())]
                                 ))
                             ))
                         )
                     ])
        );
    }

    #[test]
    fn parse_conditions() {
        parse("(cond (true 1))").unwrap();

//        assert_that!(expr, equal_to(vec![Expr::Identifier("+".to_string()), Expr::Number(123.0), Expr::Number(456.0)]));
    }

    #[test]
    fn parse_operations() {
        let expr = parse("+ 123 456").unwrap();

        assert_that!(expr, equal_to(Expr::List(vec![Expr::Identifier("+".to_string()), Expr::Number(123.0), Expr::Number(456.0)])));
    }

    fn parse(source: &str) -> Result<Expr, String> {
        let lexer = Lexer::new();
        let tokens = lexer.lex(source).unwrap();
        let parser = Parser::new();

        parser.parse(tokens)
    }

    fn assert_invalid(source: &str, message: &str) {
        let res = parse(source);

        assert!(res.is_err());
        assert_that!(res.err().unwrap(), equal_to(message));
    }

    fn assert_parse(source: &str, expr: Expr) {
        let res = parse(source);

        assert_that!(res.unwrap(), equal_to(expr));
    }
}