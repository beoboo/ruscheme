use std::iter::Peekable;
use std::slice::Iter;

use log::debug;

use crate::error::Error;
use crate::error::Error::UnterminatedInput;
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

    pub fn parse(&self, tokens: Vec<Token>) -> Result<Expr, Error> {
        if tokens.len() == 0 {
            return report_error("No tokens available.");
        }

        let mut it = tokens.iter().peekable();

        let token_type = peek(&mut it);
        if token_type == TokenType::EOF {
            return report_error("Unexpected EOF.");
        }

        match self.expression_list(&mut it) {
            Ok(expr) => Ok(expr),
            Err(e) => Err(e)
        }
    }

    fn expression_list(&self, it: &mut Peekable<Iter<Token>>) -> Result<Expr, Error> {
        let mut exprs = Vec::new();
        loop {
            if peek(it) == TokenType::EOF {
                break;
            }

            match self.primitive(it) {
                Ok(expr) => exprs.push(expr),
                Err(e) => return Err(e)
            }
        }

        match exprs.len() {
            0 => Ok(Expr::Empty),
            _ => Ok(Expr::List(exprs))
        }
    }

    fn primitive(&self, it: &mut Peekable<Iter<Token>>) -> Result<Expr, Error> {
        let token_type = match advance(it) {
            Ok(t) => t,
            Err(e) => return report_error(e)
        };

        debug!("primitive \"{}\"", &token_type);
        let t2 = token_type.clone();
        let res = match token_type {
//            TokenType::Paren(')') => Ok(Expr::Empty),
            TokenType::Bool(b) => Ok(Expr::Bool(b)),
            TokenType::Identifier(i) => Ok(Expr::Identifier(i)),
            TokenType::Number(n) => Ok(Expr::Number(n)),
            TokenType::Define => report_error("\"define\" cannot be used outside expressions."),
            TokenType::Paren('(') => self.expression(it),
            TokenType::EOF => Err(UnterminatedInput),
            t => report_error(format!("Undefined token type: \"{}\".", t))
        };
        debug!("end primitive \"{}\"", t2);

        res
    }

    fn build_expressions(&self, it: &mut PeekableToken) -> Result<Vec<Expr>, Error> {
        debug!("expressions");
        let mut exprs: Vec<Expr> = Vec::new();

        loop {
            let token_type = peek(it);

            match token_type {
                TokenType::Paren(')') => break,
                TokenType::EOF => break,
                _ => match self.primitive(it) {
                    Ok(expr) => exprs.push(expr),
                    Err(e) => return Err(e)
                }
            }
        }
        debug!("end expressions");

        Ok(exprs)
    }

    fn expression(&self, it: &mut Peekable<Iter<Token>>) -> Result<Expr, Error> {
        let token_type = match advance(it) {
            Ok(t) => t,
            Err(e) => return report_error(e)
        };

        debug!("expression: \"{}\"", token_type);
        let t2 = token_type.clone();

        let res = match token_type {
            TokenType::And => return self.form(token_type, it),
            TokenType::Cond => return self.form(token_type, it),
            TokenType::Define => return self.form(token_type, it),
            TokenType::If => return self.form(token_type, it),
            TokenType::Identifier(_) => return self.form(token_type, it),
            TokenType::Not => return self.form(token_type, it),
            TokenType::Or => return self.form(token_type, it),
            TokenType::Paren(')') => return Ok(Expr::Empty),
            TokenType::Paren('(') => self.expression(it),
            TokenType::EOF => Err(Error::UnterminatedInput),
            t => report_error(format!("\"{}\" is not callable.", t)),
        };

        let expr = match res {
            Ok(expr) => expr,
            Err(e) => return Err(e)
        };

        let mut exprs = Vec::new();

        loop {
            let token_type = peek(it);

            debug!("found: {}", &token_type);
            match token_type {
                TokenType::Paren(')') => break,
                TokenType::EOF => break,
                _ => match self.primitive(it) {
                    Ok(expr) => exprs.push(expr),
                    Err(e) => return Err(e)
                }
            }
        }
        debug!("end expression: \"{}\"", t2);

        consume(TokenType::Paren(')'), it, format!("Expected ')' after expression."))?;

        Ok(Expr::Expression(Box::new(expr), exprs))
    }

    fn form(&self, token_type: TokenType, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("form \"{}\"", token_type);

        let res = match token_type.clone() {
            TokenType::And => self.and(it),
            TokenType::Cond => self.condition(it),
            TokenType::Define => self.definition(it),
            TokenType::If => self.if_then(it),
            TokenType::Identifier(i) => self.function_call(i, it),
            TokenType::Not => self.not(it),
            TokenType::Or => self.or(it),
            t => return report_error(format!("Undefined form \"{}\".", t))
        };

        if res.is_err() {
            return res;
        }

        consume(TokenType::Paren(')'), it, format!("Expected ')' after \"{}\" form.", token_type))?;

        debug!("end form \"{}\"", token_type);

        res
    }

    fn and(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        match self.build_expressions(it) {
            Ok(exprs) => Ok(Expr::And(exprs)),
            Err(e) => Err(e),
        }
    }

    fn condition(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("condition");
        let mut predicate_branches = Vec::new();
        let mut else_branch = Vec::new();

        loop {
            match peek(it) {
                TokenType::Paren('(') => {
                    // Consume the '('.
                    advance(it).unwrap();

                    match peek(it) {
                        TokenType::Else => {
                            // Consume the 'else'.
                            it.next();

                            else_branch = match self.build_expressions(it) {
                                Ok(exprs) => exprs,
                                Err(e) => return Err(e)
                            };

                            consume(TokenType::Paren(')'), it, format!("Expected ')' after else."))?;
                        }
                        _ => {
                            if else_branch.len() > 0 {
                                return report_error(format!("Misplaced 'else' clause."));
                            }

                            match self.predicate(it) {
                                Ok(branch) => predicate_branches.push(branch),
                                Err(e) => return Err(e)
                            }
                        }
                    };
                }
                TokenType::Paren(')') => break,
                _ => return report_error(format!("Expected ')' after cond."))
            };
        }

        debug!("Predicates {:?}", predicate_branches);
        debug!("Else {:?}", else_branch);

        Ok(Expr::Cond(predicate_branches, else_branch))
    }

    fn definition(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("definition");
        match advance(it) {
            Ok(TokenType::Identifier(i)) => self.define_expression(&i, it),
            Ok(TokenType::Paren('(')) => self.define_procedure(it),
            t => return report_error(format!("Expected name or '(' after define (found: {:?}).", t))
        }
    }

    fn if_then(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("if_then");

        let predicate = match self.primitive(it) {
            Ok(e) => e,
            Err(UnterminatedInput) => return Err(UnterminatedInput),
            _ => return report_error(format!("Expected predicate after 'if'."))
        };

        let then_branch = match self.primitive(it) {
            Ok(e) => e,
            Err(UnterminatedInput) => return Err(UnterminatedInput),
            e => {
                println!("{:?}", e);
                return report_error(format!("Expected expression after 'if'."))
            }
        };

        let mut else_branch = None;

        match peek(it) {
            TokenType::Paren(')') => {}
            _ => else_branch = match self.primitive(it) {
                Ok(e) => Some(Box::new(e)),
                Err(e) => return Err(e)
            }
        }

        Ok(Expr::If(Box::new(predicate), Box::new(then_branch), else_branch))
    }

    fn not(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        match self.primitive(it) {
            Ok(expr) => Ok(Expr::Not(Box::new(expr))),
            Err(e) => Err(e),
        }
    }

    fn or(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        match self.build_expressions(it) {
            Ok(exprs) => Ok(Expr::Or(exprs)),
            Err(e) => Err(e),
        }
    }

    fn predicate(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("predicate");
        let test = match self.primitive(it) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };

        let exprs = match self.build_expressions(it) {
            Ok(exprs) => exprs,
            Err(e) => return Err(e)
        };

        consume(TokenType::Paren(')'), it, format!("Expected ')' after predicate."))?;

        Ok(Expr::Predicate(Box::new(test), exprs))
    }

    fn function_call(&self, name: String, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("function call: \"{}\"", name);
        let args = match self.build_expressions(it) {
            Ok(args) => args,
            Err(e) => return Err(e)
        };

        Ok(Expr::Expression(Box::new(Expr::Identifier(name)), args))
    }

    fn define_expression(&self, name: &str, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("define expression: {}", name);
        let expr = match advance(it) {
            Ok(TokenType::Number(n)) => Expr::Number(n),
            Ok(TokenType::Identifier(i)) => Expr::Identifier(i),
            Ok(TokenType::Paren('(')) => {
                let token_type = match advance(it) {
                    Ok(t) => t,
                    Err(e) => return report_error(e),
                };

                let expr = match token_type {
                    TokenType::Identifier(i) => match self.function_call(i, it) {
                        Ok(e) => e,
                        Err(e) => return Err(e)
                    }
                    _ => return report_error(format!("Expected expression name."))
                };

                consume(TokenType::Paren(')'), it, format!("Expected ')' after expression."))?;

                expr
            }
            _ => return report_error(format!("Expected expression after name."))
        };

        Ok(Expr::Define(name.to_string(), Box::new(expr)))
    }

    fn define_procedure(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let name = match advance(it) {
            Ok(TokenType::Identifier(i)) => Expr::Identifier(i),
            _ => return report_error(format!("Expected procedure name."))
        };
        debug!("procedure \"{}\"", name);

        let params = match self.build_expressions(it) {
            Ok(list) => list,
            Err(e) => return Err(e)
        };
        debug!("params \"{:?}\"", params);

        consume(TokenType::Paren(')'), it, format!("Expected ')' after procedure parameters."))?;

        let body = match self.build_expressions(it) {
            Ok(exprs) => exprs,
            Err(e) => return Err(e),
        };
        debug!("body \"{:?}\"", body);

        let procedure = Expr::Procedure(name.to_string(), params, body);

        Ok(Expr::Define(name.to_string(), Box::new(procedure)))
    }
}

fn report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    let error = err.into();
    if &error == "EOF" {
        return Err(Error::UnterminatedInput);
    }

    Err(Error::Parser(error))
}

fn advance(it: &mut PeekableToken) -> Result<TokenType, String> {
    match it.next() {
        Some(token) => {
//            debug!("Token: {}", token.token_type);
            Ok(token.token_type.clone())
        }
        None => Err(format!("Token not found."))
    }
}

fn consume<S: Into<String>>(token_type: TokenType, it: &mut PeekableToken, message: S) -> Result<(), Error> {
    debug!("Consuming: {}", token_type);

    let t = peek(it);
    if t == token_type {
        advance(it).unwrap();
        return Ok(());
    }
    if t == TokenType::EOF {
        return Err(UnterminatedInput);
    }

    report_error(message.into())
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
        assert_that!(parser.parse(vec![]).is_err(), is(true));
    }

    #[test]
    fn parse_unterminated() {
        let res = parse("(");

        assert_that!(res.err().unwrap(), equal_to(Error::UnterminatedInput));
    }

    #[test]
    fn parse_empty_list() {
        let expr = parse("()").unwrap();

        assert_that!(expr, equal_to(Expr::List(vec![Expr::Empty])));
    }

    #[test]
    fn parse_invalid() {
        assert_invalid("define", "\"define\" cannot be used outside expressions.");
        assert_invalid("(1)", "\"1\" is not callable.");
        assert_invalid("(cond (else 1)(true 2))", "Misplaced 'else' clause.");
    }

    #[test]
    fn parse_primitives() {
        assert_parse("123", Expr::List(vec![Expr::Number(123.0)]));
        assert_parse("true", Expr::List(vec![Expr::Bool(true)]));
        assert_parse("false", Expr::List(vec![Expr::Bool(false)]));
        assert_parse("+", Expr::List(vec![Expr::Identifier("+".to_string())]));
        assert_parse("+ 1 true", Expr::List(vec![
            Expr::Identifier("+".to_string()),
            Expr::Number(1.0),
            Expr::Bool(true)
        ]));

        assert_parse("\
                (define a (+ 1))\
                (define b a)\
                b",
                     Expr::List(vec![
                         Expr::Define("a".to_string(), Box::new(Expr::Expression(
                             Box::new(Expr::Identifier("+".to_string())), vec![
                                 Expr::Number(1.0)
                             ]))),
                         Expr::Define("b".to_string(), Box::new(Expr::Identifier("a".to_string()))),
                         Expr::Identifier("b".to_string()),
                     ]),
        );
    }

    #[test]
    fn parse_expressions() {
        assert_parse("(one)", Expr::List(vec![Expr::Expression(Box::new(Expr::Identifier("one".to_string())), vec![])]));

        assert_parse("(+ 1 2)", Expr::List(vec![Expr::Expression(Box::new(Expr::Identifier("+".to_string())), vec![
            Expr::Number(1.0), Expr::Number(2.0)
        ])]));

        assert_parse("(and true false)", Expr::List(vec![Expr::And(vec![
            Expr::Bool(true), Expr::Bool(false)
        ])]));

        assert_parse("(or true false)", Expr::List(vec![Expr::Or(vec![
            Expr::Bool(true), Expr::Bool(false)
        ])]));

        assert_parse("(not true)", Expr::List(vec![Expr::Not(
            Box::new(Expr::Bool(true))
        )]));

        assert_parse("(if true +)", Expr::List(vec![
            Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Identifier("+".to_string())), None)
        ]));

        assert_parse("((if true +) 1)", Expr::List(vec![Expr::Expression(
            Box::new(Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Identifier("+".to_string())), None)),
            vec![Expr::Number(1.0)],
        )]));
    }

    #[test]
    fn parse_conditions() {
        assert_parse("(cond (true 1))",
                     Expr::List(vec![
                         Expr::Cond(vec![
                             Expr::Predicate(Box::new(Expr::Bool(true)), vec![Expr::Number(1.0)])
                         ],
                                    vec![],
                         )
                     ]));
        assert_parse("(cond (true 1) (false 2))",
                     Expr::List(vec![
                         Expr::Cond(vec![
                             Expr::Predicate(Box::new(Expr::Bool(true)), vec![Expr::Number(1.0)]),
                             Expr::Predicate(Box::new(Expr::Bool(false)), vec![Expr::Number(2.0)]),
                         ],
                                    vec![],
                         )
                     ]));
        assert_parse("(cond (true 1) (else 2))",
                     Expr::List(vec![
                         Expr::Cond(vec![
                             Expr::Predicate(Box::new(Expr::Bool(true)), vec![Expr::Number(1.0)])
                         ],
                                    vec![Expr::Number(2.0)],
                         )
                     ]));
    }

    #[test]
    fn parse_ifs() {
        assert_parse("(if true 1)",
                     Expr::List(vec![
                         Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Number(1.0)), None)
                     ]));

        assert_parse("(if true 1 2)",
                     Expr::List(vec![
                         Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Number(1.0)), Some(Box::new(Expr::Number(2.0))))
                     ]));

        assert_parse("(if true +)",
                     Expr::List(vec![
                         Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Identifier("+".to_string())), None)
                     ]));
    }

    #[test]
    fn parse_definitions() {
//        env_logger::init();
        assert_parse("(define a 1)", Expr::List(vec![Expr::Define("a".to_string(), Box::new(Expr::Number(1.0)))]));
        assert_parse("(define a b)", Expr::List(vec![Expr::Define("a".to_string(), Box::new(Expr::Identifier("b".to_string())))]));
        assert_parse("(define a (+ 1))", Expr::List(vec![
            Expr::Define("a".to_string(), Box::new(Expr::Expression(
                Box::new(Expr::Identifier("+".to_string())), vec![
                    Expr::Number(1.0)
                ])))]));
        assert_parse("(define (one) 1)",
                     Expr::List(vec![
                         Expr::Define(
                             "one".to_string(),
                             Box::new(Expr::Procedure(
                                 "one".to_string(),
                                 vec![],
                                 vec![Expr::Number(1.0)],
                             )),
                         )
                     ]),
        );
        assert_parse("(define (square x) (* x x))",
                     Expr::List(vec![
                         Expr::Define(
                             "square".to_string(),
                             Box::new(Expr::Procedure(
                                 "square".to_string(),
                                 vec![Expr::Identifier("x".to_string())],
                                 vec![Expr::Expression(
                                     Box::new(Expr::Identifier("*".to_string())),
                                     vec![Expr::Identifier("x".to_string()), Expr::Identifier("x".to_string())],
                                 )],
                             )),
                         )
                     ]),
        );
        assert_parse("(define (a) ((if true +) 1))",
                     Expr::List(vec![
                         Expr::Define(
                             "a".to_string(),
                             Box::new(Expr::Procedure(
                                 "a".to_string(),
                                 vec![],
                                 vec![
                                     Expr::Expression(Box::new(
                                         Expr::If(Box::new(Expr::Bool(true)),
                                                  Box::new(Expr::Identifier("+".to_string())),
                                                  None)
                                     ),
                                                      vec![Expr::Number(1.0)],
                                     )
                                 ],
                             )),
                         )
                     ]),
        );
    }

    #[test]
    fn parse_operations() {
        let expr = parse("+ 123 456").unwrap();

        assert_that!(expr, equal_to(Expr::List(vec![Expr::Identifier("+".to_string()), Expr::Number(123.0), Expr::Number(456.0)])));
    }

    fn parse(source: &str) -> Result<Expr, Error> {
        debug!("Parsing: \"{}\"", source);
        let lexer = Lexer::new();
        let tokens = lexer.lex(source).unwrap();
        debug!("Tokens: \"{:#?}\"", tokens);
        let parser = Parser::new();

        parser.parse(tokens)
    }

    fn assert_parse(source: &str, expr: Expr) {
        let res = parse(source);

        assert_that!(res.unwrap(), equal_to(expr));
    }

    fn assert_invalid(source: &str, message: &str) {
        let res = parse(source);

        assert!(res.is_err());
        assert_that!(res.err().unwrap().to_string(), equal_to(message));
    }
}