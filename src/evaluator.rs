use crate::environment::Environment;
use crate::expr::Expr;
use std::ops::Deref;

#[derive(Debug, PartialEq, Clone)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn evaluate(&self, expr: &Expr, env: &mut Environment) -> Result<Expr, String> {
        match expr {
            Expr::Empty => Ok(expr.clone()),
            Expr::Bool(_) => Ok(expr.clone()),
            Expr::Number(_) => Ok(expr.clone()),
            Expr::Func(_, _) => Ok(expr.clone()),
            Expr::Definition(_, _) => return Err(format!("\"define\" cannot be used outside expression.")),
            Expr::Identifier(s) => {
                let res = env.get(s);

                match res {
                    Ok(expr) => {
                        self.evaluate(&expr.clone(), env)
                    },
                    _ => Err(format!("Undefined identifier: \"{}\".", s))
                }
            }
            Expr::Expression(list) => {
                if list.len() == 0 {
                    return Ok(Expr::Empty);
                }

                let (form, args) = list.split_first().unwrap();
                match form {
                    Expr::Definition(name, expr) => {
                        env.define(name, expr.as_ref().clone());
                        Ok(Expr::Identifier(name.to_string()))
                    }
                    Expr::Identifier(s) => self.form(&s, args.to_vec(), env),
                    _ => return Err(format!("Undefined form: \"{}\".", form))
                }
            }
            e => panic!("Unmapped expression: {}", e)
        }
    }

    fn form(&self, name: &str, args: Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        let res = env.get(name);

        let expr = match res {
            Ok(expr) => expr,
            _ => return Err(format!("Undefined procedure: \"{}\".", name))
        };

        let expr = expr.clone();

        match expr {
            Expr::Func(_, f) => {
                let mut evaluated_args = Vec::new();
                for arg in args {
                    match self.evaluate(&arg, env) {
                        Ok(e) => evaluated_args.push(e),
                        Err(e) => return Err(e),
                    }
                }
                f(evaluated_args)
            }
            Expr::Procedure(_, params, body) => {
                let parent = env.clone();
                let mut enclosing = Environment::new(Some(&parent));
                
                for (i, arg) in args.iter().enumerate() {
                    let param = match params.get(i) {
                        Some(p) => p,
                        None => return Err(format!("Wrong number of params"))
                    };

                    match self.evaluate(&arg, env) {
                        Ok(e) => {
                            enclosing.define(&param.to_string(), e)
                        },
                        Err(e) => return Err(e),
                    }
                }

                self.evaluate(body.deref(), &mut enclosing)
            }
            _ => Err(format!("Undefined procedure: \"{}\".", name))
        }
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn eval_number() {
        assert_eval("486", Expr::Number(486.0));
    }

    #[test]
    fn eval_identifier() {
//        assert_eval(Expr::Identifier("+".to_string()), Expr::Func("+".to_string()));
    }

    #[test]
    fn eval_expressions() {
        assert_eval("(+ 137 349)", Expr::Number(486.0));
        assert_eval("(- 1000)", Expr::Number(-1000.0));
        assert_eval("(- 1000 334)", Expr::Number(666.0));
        assert_eval("(* 5 99)", Expr::Number(495.0));
        assert_eval("(/ 10 5)", Expr::Number(2.0));
        assert_eval("(+ 2.7 10)", Expr::Number(12.7));
        assert_eval("(+ 1)", Expr::Number(1.0));
        assert_eval("(+1)", Expr::Number(1.0));
    }

    #[test]
    fn eval_booleans() {
        assert_eval("(= 1 2)", Expr::Bool(false));
        assert_eval("(< 1 2)", Expr::Bool(true));
        assert_eval("(> 1 2)", Expr::Bool(false));
        assert_eval("(<= 1 2)", Expr::Bool(true));
        assert_eval("(>= 1 2)", Expr::Bool(false));
    }

    #[test]
    fn eval_invalid_expression() {
        assert_invalid("define a 2", "\"define\" cannot be used outside expression.".to_string());
        assert_invalid("u", "Undefined identifier: \"u\".".to_string());
        assert_invalid("(123)", "Undefined form: \"123\".".to_string());
        assert_invalid("(+)", "At least 1 argument required.".to_string());
        assert_invalid("(-)", "At least 1 argument required.".to_string());
        assert_invalid("(*)", "At least 1 argument required.".to_string());
        assert_invalid("(/)", "At least 2 arguments required.".to_string());
    }

    #[test]
    fn definitions() {
        assert_definition("(define a2 2)",
                          Expr::Identifier("a2".to_string()),
                          Expr::Number(2.0),
        );

        assert_definition("(define plus_one (+ 1))",
                          Expr::Identifier("plus_one".to_string()),
                          Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Number(1.0)]),
        );

        assert_definition("(define (square x) (* x x))",
                          Expr::Identifier("square".to_string()),
                          Expr::Procedure(
                              "square".to_string(),
                              vec![Expr::Identifier("x".to_string())],
                              Box::new(Expr::Expression(vec![
                                  Expr::Identifier("*".to_string()),
                                  Expr::Identifier("x".to_string()),
                                  Expr::Identifier("x".to_string())
                              ])),
                          ),
        );
    }

    #[test]
    fn eval_definitions() {
        assert_eval("\
                    (define a (+ 1))\
                    (define b a)\
                    b",
                    Expr::Number(1.0),
        );
    }

    #[test]
    fn eval_procedures() {
        assert_eval("\
                    (define (one) 1)\
                    (one)",
                    Expr::Number(1.0),
        );
        assert_eval("\
                    (define (square x) (* x x))\
                    (square 2)",
                    Expr::Number(4.0),
        );
    }

    fn assert_definition(expr: &str, expected_name: Expr, expected_definition: Expr) {
        let mut globals = Environment::global();
        let res = eval(&expr, &mut globals);

        assert_that!(&res.unwrap(), equal_to(&expected_name));

        let definition = globals.get(&expected_name.to_string()).unwrap();
        assert_that!(definition.clone(), equal_to(expected_definition));
    }

    fn assert_eval(expr: &str, expected: Expr) {
        let mut globals = Environment::global();
        let res = eval(expr, &mut globals);

        assert_that!(res.unwrap(), equal_to(expected));
    }

    fn assert_invalid(expr: &str, err: String) {
        let mut globals = Environment::global();
        let res = eval(expr, &mut globals);

        assert_that!(res.err(), equal_to(Some(err)));
    }

    fn eval(source: &str, globals: &mut Environment) -> Result<Expr, String> {
        let lexer = Lexer::new();
        let parser = Parser::new();
        let evaluator = Evaluator::new();

        let tokens = match lexer.lex(source) {
            Ok(tokens) => tokens,
            Err(e) => return Err(format!("Lexing error: {}.", e))
        };

        let exprs = match parser.parse(tokens) {
            Ok(exprs) => exprs,
            Err(e) => return Err(format!("Parsing error: {}.", e))
        };

        let mut res = None;
        for expr in exprs {
            res = match evaluator.evaluate(&expr, globals) {
                Ok(expr) => Some(expr),
                Err(e) => return Err(e)
            }
        }

        match res {
            Some(result) => Ok(result),
            None => Err(format!("No expressions to evaluate."))
        }
    }
}
