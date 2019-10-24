use std::collections::HashMap;

use crate::expr::Expr;

macro_rules! compare_floats {
    ($check_fn:expr) => {{
        |args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() < 2 {
                return Err(format!("At least 2 arguments required."));
            }

            let floats = match parse_floats(args) {
                Ok(nums) => nums,
                Err(e) => return Err(e)
            };

            let (first, rest) = floats.split_first().unwrap();
            let first = *first;

            fn compare(first: f64, rest: &[f64]) -> bool {
                match rest.first() {
                    Some(x) => $check_fn(first, *x) && compare(*x, &rest[1..]),
                    None => true
                }
            }

            Ok(Expr::Bool(compare(first, rest)))
        }
    }}
}


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

            match parse_floats(args) {
                Ok(nums) => Ok(Expr::Number(nums.iter().sum())),
                Err(e) => Err(e)
            }
        }));

        environment.define("-", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() == 0 {
                return Err(format!("At least 1 argument required."));
            }

            let floats = match parse_floats(args) {
                Ok(nums) => nums,
                Err(e) => return Err(e)
            };

            let (first, rest) = floats.split_first().unwrap();
            let first = *first;

            if floats.len() == 1 {
                return Ok(Expr::Number(-first));
            }

            let res = rest.iter().fold(first, |sum, a| sum - *a);

            Ok(Expr::Number(res))
        }));

        environment.define("*", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() == 0 {
                return Err(format!("At least 1 argument required."));
            }

            match parse_floats(args) {
                Ok(nums) => Ok(Expr::Number(nums.iter().fold(1.0, |sum, a| sum * *a))),
                Err(e) => Err(e)
            }
        }));

        environment.define("/", Expr::Func(|args: Vec<Expr>| -> Result<Expr, String> {
            if args.len() < 2 {
                return Err(format!("At least 2 arguments required."));
            }

            let floats = match parse_floats(args) {
                Ok(nums) => nums,
                Err(e) => return Err(e)
            };

            let (first, rest) = floats.split_first().unwrap();
            let first = *first;

            let res = rest.iter().fold(first, |sum, a| sum / *a);

            Ok(Expr::Number(res))
        }));

        environment.define("=", Expr::Func(compare_floats!(|a, b| a == b)));
        environment.define("<", Expr::Func(compare_floats!(|a, b| a < b)));
        environment.define(">", Expr::Func(compare_floats!(|a, b| a > b)));
        environment.define("<=", Expr::Func(compare_floats!(|a, b| a <= b)));
        environment.define(">=", Expr::Func(compare_floats!(|a, b| a >= b)));

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

fn parse_floats(args: Vec<Expr>) -> Result<Vec<f64>, String> {
    let mut floats = Vec::new();

    for arg in args {
        match arg.to_f64() {
            Ok(n) => floats.push(n),
            Err(e) => return Err(e)
        }
    }

    Ok(floats)
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
        assert_that!(environment.keys.len(), equal_to(9));
    }

    #[test]
    fn define_number() {
        let mut environment = Environment::new();
        environment.define("a", Expr::Number(123.0));

        assert_that!(environment.get("a").unwrap(), equal_to(& Expr::Number(123.0)));
    }

    #[test]
    fn define_symbol() {
        let mut environment = Environment::new();
        let expr = Expr::Symbol("abc".to_string());
        let expected = expr.clone();
        environment.define("a", expr);

        assert_that!(environment.get("a").unwrap(), equal_to( & expected));
    }
}
