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

    pub fn define(&mut self, key: &str, expr: Expr) {
        self.keys.insert(key.to_string(), expr);
    }

    pub fn get(&self, key: &str) -> Result<&Expr, String>{
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
    fn empty() {
        let environment = Environment::new();
        assert_that!(environment.keys.len(), equal_to(0));
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
