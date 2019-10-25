use crate::environment::Environment;
use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn evaluate(&self, expr: &Expr, env: &mut Environment) -> Result<Expr, String> {
        match expr {
            Expr::Definition(_, _) => return Err(format!("\"define\" cannot be used outside expression.")),
            Expr::Identifier(s) => {
                let res = env.get(s);

                match res {
                    Ok(expr) => self.evaluate(&expr.clone(), env),
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
            _ => Ok(expr.clone())
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
        assert_eval("486.0", Expr::Number(486.0));
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
    fn eval_definitions() {
        assert_definition(Expr::Expression(vec![
            Expr::Definition("a2".to_string(), Box::new(
                Expr::Number(2.0)
            ))]),
                          Expr::Identifier("a2".to_string()),
                          Expr::Number(2.0),
        );

        assert_definition(Expr::Expression(vec![
            Expr::Definition("+1".to_string(), Box::new(
                Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Number(1.0)])
            ))]),
                          Expr::Identifier("+1".to_string()),
                          Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Number(1.0)]),
        );
    }

    #[test]
    fn eval_multiple_definitions() {
        let exprs = vec![
            Expr::Expression(vec![Expr::Definition("a".to_string(), Box::new(
                Expr::Number(1.0)
            ))]),
            Expr::Expression(vec![Expr::Definition("b".to_string(), Box::new(
                Expr::Identifier("a".to_string())
            ))]),
            Expr::Identifier("b".to_string())
        ];

        assert_expressions(exprs, Expr::Number(1.0));
//
//        let exprs = vec![
//            Expr::Expression(vec![Expr::Definition("a".to_string(), Box::new(
//                Expr::Number(1.0)
//            ))]),
//            Expr::Expression(vec![Expr::Definition("b".to_string(), Box::new(
//                Expr::Expression(vec![Expr::Identifier("a".to_string())
//            ))]),
//            Expr::Identifier("b".to_string())
//        ];
//
//        assert_expressions(exprs, Expr::Number(1.0));
    }

    fn assert_expressions(exprs: Vec<Expr>, expected: Expr) {
        let evaluator = Evaluator::new();
        let mut globals = Environment::global();

        let mut res = None;
        for expr in exprs {
            res = Some(evaluator.evaluate(&expr, &mut globals).unwrap());
        }

        match res {
            Some(res) => assert_that!(res, equal_to(expected)),
            _ => panic!("Invalid expression")
        };
    }

    fn assert_definition(expr: Expr, expected_name: Expr, expected_definition: Expr) {
        let evaluator = Evaluator::new();
        let mut globals = Environment::global();

        let res = evaluator.evaluate(&expr, &mut globals);

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
