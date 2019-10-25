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
                    Ok(expr) => Ok(expr.clone()),
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

    use super::*;

    #[test]
    fn eval_number() {
        assert_eval(Expr::Number(486.0), Expr::Number(486.0));
    }

    #[test]
    fn eval_identifier() {
//        assert_eval(Expr::Identifier("+".to_string()), Expr::Func("+".to_string()));
    }

    #[test]
    fn eval_expressions() {
        assert_eval(Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Number(137.0), Expr::Number(349.0)]), Expr::Number(486.0));
        assert_eval(Expr::Expression(vec![Expr::Identifier("-".to_string()), Expr::Number(1000.0)]), Expr::Number(-1000.0));
        assert_eval(Expr::Expression(vec![Expr::Identifier("-".to_string()), Expr::Number(1000.0), Expr::Number(334.0)]), Expr::Number(666.0));
        assert_eval(Expr::Expression(vec![Expr::Identifier("*".to_string()), Expr::Number(5.0), Expr::Number(99.0)]), Expr::Number(495.0));
        assert_eval(Expr::Expression(vec![Expr::Identifier("/".to_string()), Expr::Number(10.0), Expr::Number(5.0)]), Expr::Number(2.0));
        assert_eval(Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Number(2.7), Expr::Number(10.0)]), Expr::Number(12.7));
        assert_eval(Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Expression(vec![Expr::Identifier("+".to_string()), Expr::Number(1.0)])]), Expr::Number(1.0));
    }

    #[test]
    fn eval_booleans() {
        assert_eval(Expr::Expression(vec![Expr::Identifier("=".to_string()), Expr::Number(1.0), Expr::Number(2.0)]), Expr::Bool(false));
        assert_eval(Expr::Expression(vec![Expr::Identifier("<".to_string()), Expr::Number(1.0), Expr::Number(2.0)]), Expr::Bool(true));
        assert_eval(Expr::Expression(vec![Expr::Identifier(">".to_string()), Expr::Number(1.0), Expr::Number(2.0)]), Expr::Bool(false));
        assert_eval(Expr::Expression(vec![Expr::Identifier("<=".to_string()), Expr::Number(1.0), Expr::Number(2.0)]), Expr::Bool(true));
        assert_eval(Expr::Expression(vec![Expr::Identifier(">=".to_string()), Expr::Number(1.0), Expr::Number(2.0)]), Expr::Bool(false));
    }

    #[test]
    fn eval_definitions() {
        assert_definition(Expr::Expression(vec![Expr::Definition("a".to_string(), Box::new(Expr::Number(2.0)))]), Expr::Identifier("a".to_string()));
    }

    #[test]
    fn eval_invalid_expression() {
        assert_invalid(Expr::Definition("a".to_string(), Box::new(Expr::Number(2.0))), "\"define\" cannot be used outside expression.".to_string());
        assert_invalid(Expr::Identifier("u".to_string()), "Undefined identifier: \"u\".".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Number(123.0)]), "Undefined form: \"123\".".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Identifier("+".to_string())]), "At least 1 argument required.".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Identifier("-".to_string())]), "At least 1 argument required.".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Identifier("*".to_string())]), "At least 1 argument required.".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Identifier("/".to_string())]), "At least 2 arguments required.".to_string());
    }

    fn assert_definition(expr: Expr, expected: Expr) {
        let evaluator = Evaluator::new();
        let mut globals = Environment::global();

        let res = evaluator.evaluate(&expr, &mut globals);

        assert_that!(&res.unwrap(), equal_to(&expected));

        let definition = globals.get(&expected.to_string()).unwrap();
        assert_that!(definition.clone(), equal_to(Expr::Number(2.0)));
    }

    fn assert_eval(expr: Expr, expected: Expr) {
        let res = eval(expr);

        assert_that!(res.unwrap(), equal_to(expected));
    }

    fn assert_invalid(expr: Expr, err: String) {
        let res = eval(expr);

        assert_that!(res.err(), equal_to(Some(err)));
    }

    fn eval(expr: Expr) -> Result<Expr, String> {
        let evaluator = Evaluator::new();
        let mut globals = Environment::global();

        evaluator.evaluate(&expr, &mut globals)
    }
}
