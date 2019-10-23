use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Symbol(s) => {
                Ok(format!("{}", s))
            }
            Expr::Number(n) => {
                Ok(format!("{}", *n))
            }
            Expr::Expression(list) => {
                if list.len() == 0 {
                    return Ok("()".to_string());
                }

                let (procedure, args) = list.split_first().unwrap();
                match procedure {
//                    Expr::Number(n) => Ok(n.to_string()),
                    Expr::Symbol(s) => self.procedure(&s, args),
                    _ => return Err(format!("Undefined procedure \"{}\".", procedure))
                }
            }
//            e => return Err(format!("Undefined expression \"{}\".", e))
        }
    }

    fn procedure(&self, name: &str, args: &[Expr]) -> Result<String, String> {
        match name {
            "+" => {
                if args.len() == 0 {
                    return Err(format!("At least 1 argument required."));
                }

                let res = args.iter().fold(0.0, |sum, a| sum + self.number(a).unwrap());
                Ok(res.to_string())
            }
            "-" => {
                if args.len() == 0 {
                    return Err(format!("At least 1 argument required."));
                }

                let (first, rest) = args.split_first().unwrap();
                let mut res = self.number(first).unwrap();

                if rest.is_empty() {
                    res = -res
                } else {
                    res = rest.iter().fold(
                        res,
                        |sub, a | sub - self.number(a).unwrap()
                    );
                }
                Ok(res.to_string())
            }
            "*" => {
                if args.len() == 0 {
                    return Err(format!("At least 1 argument required."));
                }

                let res = args.iter().fold(1.0, |sum, a| sum * self.number(a).unwrap());
                Ok(res.to_string())
            }
            "/" => {
                if args.len() == 0 {
                    return Err(format!("At least 1 argument required."));
                }

                let (first, rest) = args.split_first().unwrap();
                let res = rest.iter().fold(
                    self.number(first).unwrap(),
                    |div, a | div / self.number(a).unwrap()
                );
                Ok(res.to_string())
            }
            _ => return Err(format!("Undefined procedure \"{}\".", name))
        }
    }

    fn number(&self, expr: &Expr) -> Result<f64, String> {
        match self.evaluate(expr) {
            Ok(val) => match val.parse::<f64>() {
                Ok(num) => Ok(num),
                Err(e) => Err(format!("Error: {}", e))
            },
            Err(e) => Err(e)
        }
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;
    use std::fmt::Display;

    #[test]
    fn eval_number() {
        assert_eval(Expr::Number(486.0), 486.0);
    }

    #[test]
    fn eval_symbol() {
        assert_eval(Expr::Symbol("+".to_string()), "+");
    }

//    #[test]
//    fn eval_expression() {
//        assert_list(vec![Expr::Expression("+".to_string(), Expr::Number(137.0), Expr::Number(349.0)]), vec!["procedure \"+\"", "137.0", "349"]);
//        assert_list(vec![Expr::Expression("-".to_string(), Expr::Number(1000.0)]), -1000.0);
//        assert_list(vec![Expr::Expression("-".to_string(), Expr::Number(1000.0), Expr::Number(334.0)]), 666.0);
//        assert_list(vec![Expr::Expression("*".to_string(), Expr::Number(5.0), Expr::Number(99.0)]), 495.0);
//        assert_list(vec![Expr::Expression("/".to_string(), Expr::Number(10.0), Expr::Number(5.0)]), 2.0);
//        assert_list(vec![Expr::Expression("+".to_string(), Expr::Number(2.7), Expr::Number(10.0)]), 12.7);
//    }

    #[test]
    fn eval_eval() {
        assert_eval(Expr::Expression(vec![Expr::Symbol("+".to_string()), Expr::Number(137.0), Expr::Number(349.0)]), 486.0);
        assert_eval(Expr::Expression(vec![Expr::Symbol("-".to_string()), Expr::Number(1000.0)]), -1000.0);
        assert_eval(Expr::Expression(vec![Expr::Symbol("-".to_string()), Expr::Number(1000.0), Expr::Number(334.0)]), 666.0);
        assert_eval(Expr::Expression(vec![Expr::Symbol("*".to_string()), Expr::Number(5.0), Expr::Number(99.0)]), 495.0);
        assert_eval(Expr::Expression(vec![Expr::Symbol("/".to_string()), Expr::Number(10.0), Expr::Number(5.0)]), 2.0);
        assert_eval(Expr::Expression(vec![Expr::Symbol("+".to_string()), Expr::Number(2.7), Expr::Number(10.0)]), 12.7);
    }

    #[test]
    fn eval_invalid_expression() {
        assert_invalid(Expr::Expression(vec![Expr::Number(123.0)]), "Undefined procedure \"123\".".to_string());
        assert_invalid(Expr::Expression(vec![Expr::Symbol("+".to_string())]), "At least 1 argument required.".to_string());
    }

    fn assert_eval<T: Display>(expr: Expr, value: T) {
        let evaluator = Evaluator::new();
        let res = evaluator.evaluate(&expr);

        assert_that!(res.unwrap(), equal_to(value.to_string()));
    }

    fn assert_invalid(expr: Expr, err: String) {
        let evaluator = Evaluator::new();
        let res = evaluator.evaluate(&expr);

        assert_that!(res.err(), equal_to(Some(err)));
    }
}
