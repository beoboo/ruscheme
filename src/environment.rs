use std::collections::HashMap;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    keys: HashMap<String, Expr>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            keys: HashMap::new()
        }
    }

    pub fn global() -> Environment {
        let mut environment = Environment::new();
        environment.define("+", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() == 0 {
                return Err(format!("At least 1 argument required."));
            }

            let mut res = 0.0;

            for arg in args {
                match arg.to_f64() {
                    Ok(n) => res += n,
                    Err(e) => return Err(e)
                }
            }

            Ok(Expr::Number(res))
        }));

        environment.define("-", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() == 0 {
                return Err(format!("At least 1 argument required."));
            }
            let (res, rest) = args.split_first().unwrap();
            let mut res = match res.to_f64() {
                Ok(n) => n,
                Err(e) => return Err(e)
            };

            if rest.len() == 0 {
                return Ok(Expr::Number(-res))
            }

            for arg in rest {
                res = res - match arg.to_f64() {
                    Ok(n) => n,
                    Err(e) => return Err(e)
                };
            }

            Ok(Expr::Number(res))

        }));

        environment.define("*", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() == 0 {
                return Err(format!("At least 1 argument required."));
            }

            let mut res = 1.0;

            for arg in args {
                match arg.to_f64() {
                    Ok(n) => res *= n,
                    Err(e) => return Err(e)
                }
            }

            Ok(Expr::Number(res))
        }));

        environment.define("/", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() < 2 {
                return Err(format!("At least 2 arguments required."));
            }

            let (res, rest) = args.split_first().unwrap();
            let mut res = match res.to_f64() {
                Ok(n) => n,
                Err(e) => return Err(e)
            };

            for arg in rest {
                res /= match arg.to_f64() {
                    Ok(n) => n,
                    Err(e) => return Err(e)
                }
            }

            Ok(Expr::Number(res))
        }));


        environment
    }

    pub fn define(&mut self, key: &str, expr: Expr) {
        self.keys.insert(key.to_string(), expr);
    }

    pub fn get(&self, key: &str) -> Result<&Expr, String> {
        match self.keys.get(key) {
            Some(v) => Ok(v),
            None => Err(format!("{} not found", key))
        }
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn check_empty() {
        let environment = Environment::new();
        assert_that!(environment.keys.len(), equal_to(0));
    }

    #[test]
    fn check_globals() {
        let environment = Environment::global();
        assert_that!(environment.keys.len(), equal_to(4));
    }

    #[test]
    fn define_number() {
        let mut environment = Environment::new();
        environment.define("a", Expr::Number(123.0));

        assert_that!(environment.get("a").unwrap(), equal_to(&Expr::Number(123.0)));
    }

    #[test]
    fn define_symbol() {
        let mut environment = Environment::new();
        let expr = Expr::Symbol("abc".to_string());
        let expected = expr.clone();
        environment.define("a", expr);

        assert_that!(environment.get("a").unwrap(), equal_to(&expected));
    }
}
