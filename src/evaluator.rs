use crate::expr::Expr;
use crate::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub struct Evaluator {
    environment: Environment
}

impl Evaluator {
    pub fn new(environment: Environment) -> Evaluator {
        Evaluator {
            environment
        }
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Expr, String> {
        if let Expr::Expression(list) = expr  {
            if list.len() == 0 {
                return Ok(Expr::Empty);
            }

            let (form, args) = list.split_first().unwrap();
            match form {
                Expr::Symbol(s) => self.form(&s, args.to_vec()),
                _ => return Err(format!("Undefined procedure \"{}\".", form))
            }
        } else {
            Ok(expr.clone())
        }
    }

    fn form(&self, name: &str, args: Vec<Expr>) -> Result<Expr, String> {
        if let Ok(expr) = self.environment.get(name) {
            match expr {
                Expr::Func(f) => {
                    let mut evaluated_args = Vec::new();
                    for arg in args {
                        match self.evaluate(&arg) {
                            Ok(e) => evaluated_args.push(e),
                            Err(e) => return Err(e),
                        }
                    }
                    f(evaluated_args)
                },
                _ => Err(format!("Undefined procedure \"{}\".", name))
            }
        } else {
            Err(format!("Undefined procedure \"{}\".", name))
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
    fn eval_symbol() {
        assert_eval(Expr::Symbol("+".to_string()), Expr::Symbol("+".to_string()));
    }

    #[test]
    fn eval_eval() {
        assert_eval(Expr::Expression(vec![Expr::Symbol("+".to_string()), Expr::Number(137.0), Expr::Number(349.0)]), Expr::Number(486.0));
        assert_eval(Expr::Expression(vec![Expr::Symbol("-".to_string()), Expr::Number(1000.0)]), Expr::Number(-1000.0));
        assert_eval(Expr::Expression(vec![Expr::Symbol("-".to_string()), Expr::Number(1000.0), Expr::Number(334.0)]), Expr::Number(666.0));
        assert_eval(Expr::Expression(vec![Expr::Symbol("*".to_string()), Expr::Number(5.0), Expr::Number(99.0)]), Expr::Number(495.0));
        assert_eval(Expr::Expression(vec![Expr::Symbol("/".to_string()), Expr::Number(10.0), Expr::Number(5.0)]), Expr::Number(2.0));
        assert_eval(Expr::Expression(vec![Expr::Symbol("+".to_string()), Expr::Number(2.7), Expr::Number(10.0)]), Expr::Number(12.7));
        assert_eval(Expr::Expression(vec![Expr::Symbol("+".to_string()), Expr::Expression(vec![Expr::Symbol("+".to_string()), Expr::Number(1.0)])]), Expr::Number(1.0));
    }

    #[test]
    fn eval_invalid_expression() {
        assert_invalid(Expr::Expression(vec![Expr::Number(123.0)]), "Undefined procedure \"123\".".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Symbol("+".to_string())]), "At least 1 argument required.".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Symbol("-".to_string())]), "At least 1 argument required.".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Symbol("*".to_string())]), "At least 1 argument required.".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Symbol("/".to_string())]), "At least 2 arguments required.".to_string());
    }

    fn assert_eval(expr: Expr, expected: Expr) {
        let evaluator = Evaluator::new(Environment::global());
        let res = evaluator.evaluate(&expr);

        assert_that!(res.unwrap(), equal_to(expected));
    }

    fn assert_invalid(expr: Expr, err: String) {
        let evaluator = Evaluator::new(Environment::global());
        let res = evaluator.evaluate(&expr);

        assert_that!(res.err(), equal_to(Some(err)));
    }
}
