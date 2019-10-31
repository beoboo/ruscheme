use crate::environment::Environment;
use crate::error::Error;
use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn evaluate(&self, expr: &Expr, env: &mut Environment) -> Result<Expr, Error> {
        match self.eval(expr, env) {
            Ok(res) => Ok(res),
            Err(e) => Err(Error::Evaluator(e))
        }
    }

    fn eval(&self, expr: &Expr, env: &mut Environment) -> Result<Expr, String> {
//        println!("eval: {}", expr);
//        println!("eval {}: {:?}", expr, expr);

//        let res= match expr {
        match expr {
            Expr::And(exprs) => self.eval_and(exprs, env),
            Expr::Bool(_) => Ok(expr.clone()),
            Expr::Cond(predicate_branches, else_branch) => self.eval_cond(predicate_branches, else_branch, env),
            Expr::Define(name, expr) => self.eval_definition(name, expr, env),
            Expr::Empty => Ok(expr.clone()),
            Expr::Expression(expr, args) => self.eval_expression(expr, args, env),
            Expr::Function(_, _) => Ok(expr.clone()),
            Expr::Identifier(s) => self.eval_identifier(s, env),
            Expr::If(predicate, then_branch, else_branch) => self.eval_if(predicate, then_branch, else_branch, env),
            Expr::List(list) => self.eval_list(list, env),
            Expr::Not(expr) => self.eval_not(expr, env),
            Expr::Number(_) => Ok(expr.clone()),
            Expr::Procedure(_, _, _) => Ok(expr.clone()),
            Expr::Or(exprs) => self.eval_or(exprs, env),
            e => panic!("Unmapped expression: {}", e)
//        };
        }

//        let r2 = res.clone();

//        println!("res: {:?}", res);
//        r2
    }

    fn eval_expression(&self, expr: &Box<Expr>, args: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        let expr = expr.as_ref();

        let name = match expr {
            Expr::Identifier(_) => Ok(expr.clone()),
            _ => self.eval(expr, env)
        };

        let name = match name {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };

        match self.eval_function_call(name.to_string(), args, env) {
            Ok(expr) => Ok(expr),
            Err(e) => Err(e)
        }
    }

    fn eval_and(&self, exprs: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        for expr in exprs {
            match self.eval(expr, env) {
                Ok(expr) => if !to_bool(expr) {
                    return Ok(Expr::Bool(false));
                }
                Err(e) => return Err(format!("Invalid expression: {}", e)),
            }
        }

        Ok(Expr::Bool(true))
    }

    fn eval_not(&self, expr: &Box<Expr>, env: &mut Environment) -> Result<Expr, String> {
        match self.eval(expr.as_ref(), env) {
            Ok(expr) => Ok(Expr::Bool(!to_bool(expr))),
            Err(e) => Err(format!("Invalid expression: {}", e)),
        }
    }

    fn eval_or(&self, exprs: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        for expr in exprs {
            match self.eval(expr, env) {
                Ok(expr) => if to_bool(expr) {
                    return Ok(Expr::Bool(true));
                }
                Err(e) => return Err(format!("Invalid expression: {}", e)),
            }
        }

        Ok(Expr::Bool(false))
    }

    fn eval_cond(&self, predicate_branches: &Vec<Expr>, _else_branch: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        for branch in predicate_branches {
            match branch {
                Expr::Predicate(predicate, exprs) => {
                    match self.eval(predicate, env) {
                        Ok(Expr::Bool(true)) => return self.eval_list(exprs, env),
                        Err(e) => return Err(e),
                        _ => {}
                    }
                }
                e => return Err(format!("Invalid predicate: {}", e))
            }
        }

        self.eval_list(_else_branch, env)
    }

    fn eval_definition(&self, name: &String, expr: &Box<Expr>, env: &mut Environment) -> Result<Expr, String> {
        env.define(name, expr.as_ref().clone());
        Ok(Expr::Identifier(name.to_string()))
    }

    fn eval_if(&self, predicate: &Box<Expr>, then_branch: &Box<Expr>, else_branch: &Option<Box<Expr>>, env: &mut Environment) -> Result<Expr, String> {
        match self.eval(predicate, env) {
            Ok(Expr::Bool(true)) => self.eval(then_branch.as_ref(), env),
            Err(e) => return Err(e),
            _ => match else_branch {
                Some(e) => self.eval(e, env),
                _ => Ok(Expr::Empty)
            }
        }
    }

    fn eval_list(&self, exprs: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        let mut res = Expr::Empty;

        for expr in exprs {
            res = match self.eval(expr, env) {
                Ok(expr) => expr,
                Err(e) => return Err(e)
            }
        }

        Ok(res)
    }

    fn eval_identifier(&self, s: &String, env: &mut Environment) -> Result<Expr, String> {
        let res = env.get(s);

        match res {
            Ok(expr) => {
                self.eval(&expr.clone(), env)
            }
            _ => Err(format!("Undefined identifier: '{}'.", s))
        }
    }

    fn eval_function_call(&self, name: String, args: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        let res = env.get(&name.to_string());

        let expr = match res {
            Ok(expr) => expr,
            _ => return Err(format!("Undefined procedure: '{}'.", name))
        };

        let expr = expr.clone();

        match expr {
            Expr::Function(_, f) => self.eval_function(f, args, env),
            Expr::Procedure(_, params, body) => self.eval_procedure(name, args, params, body, env),
            _ => Err(format!("Cannot execute: '{}'.", expr))
        }
    }

    fn eval_function(&self, f: fn(Vec<Expr>) -> Result<Expr, String>, args: &Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        let mut evaluated_args = Vec::new();
        for arg in args {
            match self.eval(&arg, env) {
                Ok(e) => evaluated_args.push(e),
                Err(e) => return Err(e),
            }
        }
        f(evaluated_args)
    }

    fn eval_procedure(&self, name: String, args: &Vec<Expr>, params: Vec<Expr>, body: Vec<Expr>, env: &mut Environment) -> Result<Expr, String> {
        let parent = env.clone();
        let mut enclosing = Environment::new(Some(&parent));

        if params.len() != args.len() {
            return Err(format!(
                "'{}': wrong number of arguments (required: {}, given: {}).",
                name,
                params.len(),
                args.len()
            ));
        }

        for (i, arg) in args.iter().enumerate() {
            let param = match params.get(i) {
                Some(p) => p,
                None => return Err(format!("Wrong number of params"))
            };

            match self.eval(&arg, env) {
                Ok(e) => enclosing.define(&param.to_string(), e),
                Err(e) => return Err(e),
            }
        }

        let mut res = Expr::Empty;

        for expr in body {
            res = match self.eval(&expr, &mut enclosing) {
                Ok(res) => res,
                Err(e) => return Err(e)
            }
        }
        Ok(res)
    }
}

fn to_bool(expr: Expr) -> bool {
    expr != Expr::Bool(false)
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn eval_primitives() {
        assert_eval("true", Expr::Bool(true));
        assert_eval("false", Expr::Bool(false));
        assert_eval("486", Expr::Number(486.0));
    }

    #[test]
    fn eval_identifiers() {
        let mut globals = Environment::global();
        let res = eval("+", &mut globals);
        assert_that!(&res.unwrap(), equal_to(globals.get("+").unwrap()));
    }

    #[test]
    fn eval_expressions() {
        assert_eval("(and true (< 1 2))", Expr::Bool(true));
        assert_eval("(or false (> 1 2))", Expr::Bool(false));
        assert_eval("(not true)", Expr::Bool(false));
    }

    #[test]
    fn eval_invalid() {
        assert_invalid("define a 2", "'define' cannot be used outside expressions.".to_string());
        assert_invalid("u", "Undefined identifier: 'u'.".to_string());
        assert_invalid("(123)", "'123' is not callable.".to_string());
        assert_invalid("(+)", "At least 1 argument required.".to_string());
        assert_invalid("(-)", "At least 1 argument required.".to_string());
        assert_invalid("(*)", "At least 1 argument required.".to_string());
        assert_invalid("(/)", "At least 2 arguments required.".to_string());
        assert_invalid("(define (a x) x)(a)", "'a': wrong number of arguments (required: 1, given: 0).".to_string());
        assert_invalid("(define (a x) x)(a 1 2)", "'a': wrong number of arguments (required: 1, given: 2).".to_string());
    }

    #[test]
    fn eval_symbols() {
        assert_definition("(define a2 2)",
                          Expr::Identifier("a2".to_string()),
                          Expr::Number(2.0),
        );

        assert_definition("(define plus_one (+ 1))",
                          Expr::Identifier("plus_one".to_string()),
                          Expr::Expression(Box::new(Expr::Identifier("+".to_string())), vec![Expr::Number(1.0)]),
        );

        assert_definition("(define (square x) (* x x))",
                          Expr::Identifier("square".to_string()),
                          Expr::Procedure(
                              "square".to_string(),
                              vec![Expr::Identifier("x".to_string())],
                              vec![(Expr::Expression(Box::new(Expr::Identifier("*".to_string())), vec![
                                  Expr::Identifier("x".to_string()),
                                  Expr::Identifier("x".to_string())
                              ]))],
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
        assert_eval("\
                    (define (a) (+ 1))\
                    a",
                    Expr::Procedure("a".to_string(), vec![], vec![
                        Expr::Expression(Box::new(Expr::Identifier("+".to_string())), vec![Expr::Number(1.0)])
                    ]),
        );
    }

    #[test]
    fn eval_conditions() {
        assert_eval("(cond ((< 1 2) 1))", Expr::Number(1.0));
        assert_eval("(cond ((< 1 2) 1 2))", Expr::Number(2.0));
        assert_eval("(cond ((> 1 2) 1) (else 2))", Expr::Number(2.0));
    }

    #[test]
    fn eval_ifs() {
        assert_eval("(if (< 1 2) 1)", Expr::Number(1.0));
        assert_eval("(if (> 1 2) 1 2)", Expr::Number(2.0));
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

        assert_that!(res.err().unwrap().to_string(), equal_to(err));
    }

    fn eval(source: &str, globals: &mut Environment) -> Result<Expr, Error> {
        let lexer = Lexer::new();
        let parser = Parser::new();
        let evaluator = Evaluator::new();

        let tokens = match lexer.lex(source) {
            Ok(tokens) => tokens,
            Err(e) => return Err(e)
        };

        let expr = match parser.parse(tokens) {
            Ok(e) => e,
            Err(e) => return Err(e)
        };

        let res = match evaluator.evaluate(&expr, globals) {
            Ok(expr) => Some(expr),
            Err(e) => return Err(e)
        };

        match res {
            Some(result) => Ok(result),
            None => Err(Error::Evaluator(format!("No expressions to eval.")))
        }
    }
}
