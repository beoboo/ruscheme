use log::debug;

use crate::error::{Error, report_stage_error};
use crate::error::Error::UnterminatedInput;
use crate::expr::*;
use crate::token::*;

#[derive(Debug)]
pub struct Parser {}

impl Parser {
    pub fn new() -> Parser {
        Parser {}
    }

    pub fn parse(&self, tokens: Vec<Token>) -> Result<Vec<Expr>, Error> {
        if tokens.len() == 0 {
            return _report_error("No tokens available.");
        }

        let mut it = tokens.iter().peekable();

        let token_type = peek(&mut it);
        if token_type == TokenType::EOF {
            return _report_error("Unexpected EOF.");
        }

        self.expression_list(&mut it)
    }

    fn expression_list(&self, it: &mut PeekableToken) -> Result<Vec<Expr>, Error> {
        let mut exprs = Vec::new();
        loop {
            if peek(it) == TokenType::EOF {
                break;
            }

            let expr = self.primitive(it)?;
            exprs.push(expr);
        }

        Ok(exprs)
    }

    fn primitive(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let token_type = advance(it)?.token_type;

        debug!("primitive '{}'", &token_type);

        match token_type {
            TokenType::Bool(b) => Ok(Expr::Bool(b)),
            TokenType::Identifier(i) => Ok(Expr::Identifier(i)),
            TokenType::Number(n) => Ok(Expr::Number(n)),
            TokenType::String(s) => Ok(Expr::String(s)),
            TokenType::Define => _report_error("'define' cannot be used outside expressions."),
            TokenType::Paren('(') => self.expression(it),
            TokenType::EOF => Err(UnterminatedInput),
            t => _report_error(format!("Undefined token type: '{}'.", t))
        }
    }

    fn build_expressions(&self, it: &mut PeekableToken) -> Result<Vec<Expr>, Error> {
        debug!("expressions");
        let mut exprs: Vec<Expr> = Vec::new();

        loop {
            let token_type = peek(it);

            match token_type {
                TokenType::Paren(')') => break,
                TokenType::EOF => break,
                _ => exprs.push(self.primitive(it)?)
            }
        }
        debug!("end expressions");

        Ok(exprs)
    }

    fn expression(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let token_type = advance(it)?.token_type;

        debug!("expression: '{}'", token_type);
        let t2 = token_type.clone();

        let expr = match token_type {
            TokenType::And => return self.form(token_type, it),
            TokenType::Cond => return self.form(token_type, it),
            TokenType::Define => return self.form(token_type, it),
            TokenType::If => return self.form(token_type, it),
            TokenType::Identifier(_) => return self.form(token_type, it),
            TokenType::Lambda => return self.form(token_type, it),
            TokenType::Let => return self.form(token_type, it),
            TokenType::Not => return self.form(token_type, it),
            TokenType::Or => return self.form(token_type, it),
            TokenType::Paren(')') => return Ok(Expr::Empty),
            TokenType::Paren('(') => self.expression(it),
            TokenType::Quote => return self.form(token_type, it),
            TokenType::EOF => Err(Error::UnterminatedInput),
            t => _report_error(format!("'{}' is not callable.", t)),
        }?;

        let mut exprs = vec![expr];

        loop {
            let token_type = peek(it);

            debug!("found: {}", &token_type);
            match token_type {
                TokenType::Paren(')') => break,
                TokenType::EOF => break,
                _ => exprs.push(self.primitive(it)?)
            }
        }
        debug!("end expression: '{}'", t2);

        consume(TokenType::Paren(')'), it, format!("Expected ')' after expression."))?;

        Ok(Expr::Expression(exprs))
    }

    fn form(&self, token_type: TokenType, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("form '{}'", token_type);

        let res = match token_type.clone() {
            TokenType::And => self.and(it),
            TokenType::Cond => self.cond(it),
            TokenType::Define => self.define(it),
            TokenType::If => self.if_then(it),
            TokenType::Identifier(i) => self.call(i, it),
            TokenType::Lambda => {
                consume(TokenType::Paren('('), it, format!("Expected '(' after 'lambda'."))?;
                self.lambda(it)
            }
            TokenType::Let => self.define_let(it),
            TokenType::Not => self.not(it),
            TokenType::Or => self.or(it),
            TokenType::Quote => self.quote(it),
            t => return _report_error(format!("Undefined form '{}'.", t))
        };

        if res.is_err() {
            return res;
        }

        consume(TokenType::Paren(')'), it, format!("Expected ')' after '{}' form.", token_type))?;

        debug!("end form '{}'", token_type);

        res
    }

    fn and(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let exprs = self.build_expressions(it)?;

        Ok(Expr::And(exprs))
    }

    fn call(&self, name: String, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("call: '{}'", name);
        let mut args = self.build_expressions(it)?;

        args.insert(0, Expr::Identifier(name));

        Ok(Expr::Expression(args))
    }

    fn cond(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("condition");
        let mut predicate_branches = Vec::new();
        let mut else_branch = Vec::new();

        loop {
            match peek(it) {
                TokenType::Paren('(') => {
                    // Consume the '('.
                    advance(it)?;

                    match peek(it) {
                        TokenType::Else => {
                            // Consume the 'else'.
                            advance(it)?;

                            else_branch = self.build_expressions(it)?;

                            consume(TokenType::Paren(')'), it, format!("Expected ')' after else."))?;
                        }
                        _ => {
                            if else_branch.len() > 0 {
                                return _report_error(format!("Misplaced 'else' clause."));
                            }

                            let branch = self.predicate(it)?;
                            predicate_branches.push(branch);
                        }
                    };
                }
                TokenType::Paren(')') => break,
                TokenType::EOF => return Err(UnterminatedInput),
                _ => return _report_error("Expected ')' after cond.")
            };
        }

        if predicate_branches.len() == 0 && else_branch.len() == 0 {
            return _report_error("'cond' must have at least one clause.");
        }

        debug!("Predicates {:?}", predicate_branches);
        debug!("Else {:?}", else_branch);

        Ok(Expr::Cond(predicate_branches, else_branch))
    }

    fn define(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("definition");
        match advance(it)?.token_type {
            TokenType::Identifier(i) => self._define_expression(&i, it),
            TokenType::Paren('(') => self._define_lambda(it),
            TokenType::EOF => return Err(UnterminatedInput),
            t => return _report_error(format!("Expected name or '(' after define (found: {:?}).", t))
        }
    }

    fn _define_expression(&self, name: &str, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("define expression: {}", name);
        let expr = match advance(it)?.token_type {
            TokenType::Number(n) => Expr::Number(n),
            TokenType::Identifier(i) => Expr::Identifier(i),
            TokenType::Paren('(') => {
                let token_type = advance(it)?.token_type;

                let expr = match token_type {
                    TokenType::Identifier(i) => self.call(i, it)?,
                    TokenType::Lambda => {
                        consume(TokenType::Paren('('), it, format!("Expected '(' after 'lambda'."))?;
                        self.lambda(it)?
                    }
                    t => return _report_error(format!("Expected expression name (found '{}').", t))
                };

                consume(TokenType::Paren(')'), it, format!("Expected ')' after expression."))?;

                expr
            }
            _ => return _report_error(format!("Expected expression after name."))
        };

        Ok(Expr::Define(name.to_string(), Box::new(expr)))
    }

    fn _define_lambda(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let name = match advance(it)?.token_type {
            TokenType::Identifier(i) => Expr::Identifier(i),
            TokenType::EOF => return Err(UnterminatedInput),
            _ => return _report_error(format!("Expected lambda name."))
        };

        let lambda = self.lambda(it)?;

        Ok(Expr::Define(name.to_string(), Box::new(lambda)))
    }

    fn if_then(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("if_then");

        let predicate = match self.primitive(it) {
            Ok(e) => e,
            Err(UnterminatedInput) => return Err(UnterminatedInput),
            _ => return _report_error(format!("Expected predicate after 'if'."))
        };

        let then_branch = match self.primitive(it) {
            Ok(e) => e,
            Err(UnterminatedInput) => return Err(UnterminatedInput),
            e => {
                println!("{:?}", e);
                return _report_error(format!("Expected expression after 'if'."));
            }
        };

        let mut else_branch = None;

        match peek(it) {
            TokenType::Paren(')') => {}
            _ => else_branch = Some(Box::new(self.primitive(it)?)),
        }

        Ok(Expr::If(Box::new(predicate), Box::new(then_branch), else_branch))
    }

    fn lambda(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("lambda");

        let params = self.build_expressions(it)?;
        debug!("params '{:?}'", params);

        consume(TokenType::Paren(')'), it, format!("Expected ')' after lambda parameters."))?;

        if peek(it) == TokenType::Paren(')') {
            return _report_error(format!("Expected lambda body."));
        }

        let body = self.build_expressions(it)?;
        debug!("body '{:?}'", body);

        Ok(Expr::Lambda(params, body, None))
    }

    fn define_let(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        consume(TokenType::Paren('('), it, format!("Expected '(' after 'let'."))?;
        let mut args = Vec::new();
        let mut exprs = Vec::new();

        loop {
            match peek(it) {
                TokenType::Paren(')') => break,
                TokenType::Paren('(') => {
                    let (arg, expr) = self.let_variable(it)?;
                    args.push(arg);
                    exprs.push(expr);
                }
                t => {
                    return _report_error(format!("Found unexpected token '{}'", t));
                }
            };
        }

        consume(TokenType::Paren(')'), it, format!("Expected ')' after 'let' definitions."))?;

        let body = self.build_expressions(it)?;
        debug!("body '{:?}'", body);

        let lambda = Expr::Lambda(args, body, None);

        exprs.insert(0, lambda);

        Ok(Expr::Expression(exprs))
    }

    fn let_variable(&self, it: &mut PeekableToken) -> Result<(Expr, Expr), Error> {
        consume(TokenType::Paren('('), it, format!("Expected '(' before variable."))?;

        let arg = self.primitive(it)?;
        let expr = self.primitive(it)?;

        consume(TokenType::Paren(')'), it, format!("Expected ')' after variable."))?;

        Ok((arg, expr))
    }

    fn not(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let expr = self.primitive(it)?;

        Ok(Expr::Not(Box::new(expr)))
    }

    fn or(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let exprs = self.build_expressions(it)?;

        Ok(Expr::Or(exprs))
    }

    fn predicate(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("predicate");
        let test = self.primitive(it)?;

        let exprs = self.build_expressions(it)?;

        consume(TokenType::Paren(')'), it, format!("Expected ')' after predicate."))?;

        Ok(Expr::Predicate(Box::new(test), exprs))
    }

    fn quote(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        debug!("quote");

        let expr = match advance(it)?.token_type {
            TokenType::Bool(b) => Expr::Bool(b),
            TokenType::Identifier(i) => Expr::QuotedIdentifier(i),
            TokenType::Number(n) => Expr::Number(n),
            TokenType::String(s) => Expr::String(s),
            TokenType::Paren('(') => self._quote_expression(it)?,
            TokenType::EOF => return Err(UnterminatedInput),
            t => Expr::QuotedIdentifier(format!("{}", t))
        };

        debug!("end quote: {}", expr);
        Ok(Expr::Quote(Box::new(expr)))
    }

    fn _quote_expression(&self, it: &mut PeekableToken) -> Result<Expr, Error> {
        let mut exprs = vec![];
        debug!("quote_expression");

        loop {
            let expr = match peek(it) {
                TokenType::Paren(')') => break,
                TokenType::EOF => break,
                TokenType::Paren('(') => {
                    Expr::QuotedIdentifier(self._quote_subexpression(it)?)
                }
                t => {
                    advance(it)?;
                    Expr::QuotedIdentifier(t.to_string())
                }
            };

            exprs.push(expr);
        }

        consume(TokenType::Paren(')'), it, format!("Expected ')' after quoted expression."))?;

        debug!("end quoted expression: {:?}", exprs);
        Ok(build_cons(&exprs))
    }

    fn _quote_subexpression(&self, it: &mut PeekableToken) -> Result<String, Error> {
        advance(it)?;

        let mut tokens = vec![];
        debug!("quote_subexpression");

        loop {
            match peek(it) {
                TokenType::EOF => break,
                TokenType::Paren(')') => {
                    break;
                }
                TokenType::Paren('(') => {
                    advance(it)?;
                    tokens.push(self._quote_subexpression(it)?)
                }
                t => {
                    advance(it)?;
                    tokens.push(t.to_string())
                }
            }
        }
        consume(TokenType::Paren(')'), it, format!("Expected ')' after quoted subexpression."))?;

        debug!("end quoted subexpression: {}", tokens.join(" "));
        Ok(format!("({})", tokens.join(" ")))
    }
}

fn _report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    report_stage_error(err, "parser")
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::lexer::Lexer;

    use super::*;
    use crate::desugarizer::Desugarizer;

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
        assert_parse("()", Expr::Empty);
    }

    #[test]
    fn parse_comment() {
        assert_parse("\
        ; This is a comment\n\
        123", Expr::Number(123.0));
    }

    #[test]
    fn parse_invalid() {
        assert_invalid("define", "'define' cannot be used outside expressions.");
        assert_invalid("(1)", "'1' is not callable.");
        assert_invalid("(cond (else 1)(true 2))", "Misplaced 'else' clause.");
        assert_invalid("(cond)", "'cond' must have at least one clause.");
        assert_invalid("(lambda)", "Expected '(' after 'lambda'.");
        assert_invalid("(lambda ())", "Expected lambda body.");
    }

    #[test]
    fn parse_primitives() {
        assert_parse("123", Expr::Number(123.0));
        assert_parse("-123", Expr::Number(-123.0));
        assert_parse("true", Expr::Bool(true));
        assert_parse("false", Expr::Bool(false));
        assert_parse("\"string\"", Expr::String("string".to_string()));
        assert_parse("+", Expr::Identifier("+".to_string()));
        assert_parse_list("+ 1 true", vec![
            Expr::Identifier("+".to_string()),
            Expr::Number(1.0),
            Expr::Bool(true)
        ]);
    }

    #[test]
    fn parse_conditions() {
        assert_parse("(cond (true 1))",
                     Expr::Cond(vec![
                         Expr::Predicate(Box::new(Expr::Bool(true)), vec![Expr::Number(1.0)])
                     ],
                                vec![],
                     ),
        );
        assert_parse("(cond (true 1) (false 2))",
                     Expr::Cond(vec![
                         Expr::Predicate(Box::new(Expr::Bool(true)), vec![Expr::Number(1.0)]),
                         Expr::Predicate(Box::new(Expr::Bool(false)), vec![Expr::Number(2.0)]),
                     ],
                                vec![],
                     ),
        );
        assert_parse("(cond (true 1) (else 2))",
                     Expr::Cond(vec![
                         Expr::Predicate(Box::new(Expr::Bool(true)), vec![Expr::Number(1.0)])
                     ],
                                vec![Expr::Number(2.0)],
                     ),
        );
    }

    #[test]
    fn parse_definitions() {
//        env_logger::init();
        assert_parse("(define a 1)", Expr::Define("a".to_string(), Box::new(Expr::Number(1.0))));
        assert_parse("(define a b)", Expr::Define("a".to_string(), Box::new(Expr::Identifier("b".to_string()))));
        assert_parse("(define a (+ 1))",
                     Expr::Define("a".to_string(), Box::new(Expr::Expression(vec![
                         Expr::Identifier("+".to_string()),
                         Expr::Number(1.0)
                     ]))));
        assert_parse("(define (one) 1)",
                     Expr::Define(
                         "one".to_string(),
                         Box::new(Expr::Lambda(
                             vec![],
                             vec![Expr::Number(1.0)],
                             None,
                         )),
                     ),
        );
        assert_parse("(define (square x) (* x x))",
                     Expr::Define(
                         "square".to_string(),
                         Box::new(Expr::Lambda(
                             vec![Expr::Identifier("x".to_string())],
                             vec![Expr::Expression(vec![
                                 Expr::Identifier("*".to_string()),
                                 Expr::Identifier("x".to_string()), Expr::Identifier("x".to_string())
                             ])],
                             None,
                         )),
                     ),
        );
        assert_parse("(define square (lambda (x) (* x x)))",
                     Expr::Define(
                         "square".to_string(),
                         Box::new(Expr::Lambda(
                             vec![Expr::Identifier("x".to_string())],
                             vec![Expr::Expression(vec![
                                 Expr::Identifier("*".to_string()),
                                 Expr::Identifier("x".to_string()), Expr::Identifier("x".to_string())
                             ])],
                             None,
                         )),
                     ),
        );
        assert_parse("(define (a) ((if true +) 1))",
                     Expr::Define(
                         "a".to_string(),
                         Box::new(Expr::Lambda(
                             vec![],
                             vec![Expr::Expression(vec![
                                 Expr::If(Box::new(Expr::Bool(true)),
                                          Box::new(Expr::Identifier("+".to_string())),
                                          None),
                                 Expr::Number(1.0)
                             ])],
                             None,
                         )),
                     ),
        );

        assert_parse_list("\
                (define a (+ 1))\
                (define b a)\
                b",
                          vec![
                              Expr::Define("a".to_string(),
                                           Box::new(Expr::Expression(vec![
                                               Expr::Identifier("+".to_string()),
                                               Expr::Number(1.0)
                                           ]))),
                              Expr::Define("b".to_string(), Box::new(Expr::Identifier("a".to_string()))),
                              Expr::Identifier("b".to_string()),
                          ],
        );
    }

    #[test]
    fn parse_expressions() {
        assert_parse("(one)", Expr::Expression(vec![Expr::Identifier("one".to_string())]));

        assert_parse("(+ 1 2)", Expr::Expression(vec![
            Expr::Identifier("+".to_string()),
            Expr::Number(1.0),
            Expr::Number(2.0)
        ]));

        assert_parse("(and true false)", Expr::And(vec![
            Expr::Bool(true), Expr::Bool(false)
        ]));

        assert_parse("(or true false)", Expr::Or(vec![
            Expr::Bool(true), Expr::Bool(false)
        ]));

        assert_parse("(not true)", Expr::Not(
            Box::new(Expr::Bool(true))
        ));

        assert_parse("(if true +)",
                     Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Identifier("+".to_string())), None),
        );

        assert_parse("((if true +) 1)", Expr::Expression(vec![
            Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Identifier("+".to_string())), None),
            Expr::Number(1.0)
        ]));
    }

    #[test]
    fn parse_ifs() {
        assert_parse("(if true 1)",
                     Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Number(1.0)), None),
        );

        assert_parse("(if true 1 2)",
                     Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Number(1.0)), Some(Box::new(Expr::Number(2.0)))),
        );

        assert_parse("(if true +)",
                     Expr::If(Box::new(Expr::Bool(true)), Box::new(Expr::Identifier("+".to_string())), None),
        );
    }

    #[test]
    fn parse_lambdas() {
        assert_parse("(lambda () ())",
                     Expr::Lambda(
                         vec![],
                         vec![Expr::Empty],
                         None,
                     ),
        );
        assert_parse("(lambda (x) (+ x 4))",
                     Expr::Lambda(
                         vec![Expr::Identifier("x".to_string())],
                         vec![Expr::Expression(vec![
                             Expr::Identifier("+".to_string()),
                             Expr::Identifier("x".to_string()),
                             Expr::Number(4.0)
                         ])],
                         None,
                     ),
        );
    }

    #[test]
    fn parse_lets() {
//        env_logger::init();
        assert_parse("(let () 1)", Expr::Expression(vec![
            Expr::Lambda(
                vec![],
                vec![Expr::Number(1.0)],
                None,
            ),
        ]));

        assert_parse("(let ((x 3)) x)", Expr::Expression(vec![
            Expr::Lambda(
                vec![Expr::Identifier("x".to_string())],
                vec![Expr::Identifier("x".to_string())],
                None,
            ), Expr::Number(3.0)
        ]));
        assert_parse("(let () x)", Expr::Expression(vec![
            Expr::Lambda(
                vec![],
                vec![Expr::Identifier("x".to_string())],
                None,
            )]),
        );
    }

    #[test]
    fn parse_operations() {
        assert_parse_list("+ 123 456", vec![
            Expr::Identifier("+".to_string()),
            Expr::Number(123.0),
            Expr::Number(456.0)
        ]);
    }

    #[test]
    fn parse_quotes() {
//        env_logger::init();
        assert_parse("(quote a)",
                     Expr::Quote(Box::new(Expr::QuotedIdentifier("a".to_string()))),
        );
        assert_parse("'a",
                     Expr::Quote(Box::new(Expr::QuotedIdentifier("a".to_string()))),
        );
        assert_parse("(quote (a b c))",
                     Expr::Quote(Box::new(
                         Expr::Pair(
                             Box::new(Expr::QuotedIdentifier("a".to_string())),
                             Box::new(Expr::Pair(
                                 Box::new(Expr::QuotedIdentifier("b".to_string())),
                                 Box::new(Expr::Pair(
                                     Box::new(Expr::QuotedIdentifier("c".to_string())),
                                     Box::new(Expr::Empty),
                                 )),
                             )),
                         )
                     )),
        );
        assert_parse("(quote (lambda x y))",
                     Expr::Quote(Box::new(
                         Expr::Pair(
                             Box::new(Expr::QuotedIdentifier("lambda".to_string())),
                             Box::new(Expr::Pair(
                                 Box::new(Expr::QuotedIdentifier("x".to_string())),
                                 Box::new(Expr::Pair(
                                     Box::new(Expr::QuotedIdentifier("y".to_string())),
                                     Box::new(Expr::Empty),
                                 )),
                             )),
                         )
                     )),
        );
        assert_parse("(quote ((define x y) a ()))",
                     Expr::Quote(Box::new(
                         Expr::Pair(
                             Box::new(Expr::QuotedIdentifier("(define x y)".to_string())),
                             Box::new(Expr::Pair(
                                 Box::new(Expr::QuotedIdentifier("a".to_string())),
                                 Box::new(Expr::Pair(
                                     Box::new(Expr::QuotedIdentifier("()".to_string())),
                                     Box::new(Expr::Empty),
                                 )),
                             )),
                         )
                     )),
        );
        assert_parse("''a",
                     Expr::Quote(Box::new(
                         Expr::Pair(
                             Box::new(Expr::QuotedIdentifier("quote".to_string())),
                             Box::new(Expr::Pair(
                                 Box::new(Expr::QuotedIdentifier("a".to_string())),
                                 Box::new(Expr::Empty),
                             )),
                         )
                     )),
        );
    }

    fn assert_parse(source: &str, expr: Expr) {
        let exprs = parse(source).unwrap();

        assert_that!(&exprs[0], equal_to(&expr));
    }

    fn assert_parse_list(source: &str, exprs: Vec<Expr>) {
        let actual = parse(source).unwrap();

        assert_that!(actual, equal_to(exprs));
    }

    fn assert_invalid(source: &str, message: &str) {
        let error = parse(source).unwrap_err();

        assert_that!(error.to_string(), equal_to(message));
    }

    fn parse(source: &str) -> Result<Vec<Expr>, Error> {
        debug!("Parsing: '{}'", source);
        let lexer = Lexer::new();
        let desugarizer = Desugarizer::new();
        let parser = Parser::new();

        let tokens = lexer.lex(source)?;
        let tokens = desugarizer.desugar(tokens)?;

        parser.parse(tokens)
    }
}