use std::collections::HashMap;
use std::fmt;
use std::fmt::{Error, Formatter};
use std::rc::Rc;
use std::time::SystemTime;

use log::debug;

use crate::expr::{BoxedAction, Callable, Expr};

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
                    Some(x) => $check_fn(first, *x) &&compare(*x, &rest[1..]),
                    None => true
                }
            }

            Ok(Expr::Bool(compare(first, rest)))
        }
    }}
}


#[derive(PartialEq, Clone)]
pub struct Environment {
    pub(crate) index: i32,
    parent: Option<Box<Environment>>,
    keys: HashMap<String, Expr>,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(_) = self.parent {
            writeln!(f, "")?;
            writeln!(f, "Current ({}):", self.index)?;
        }

        for (k, e) in self.keys.iter() {
            writeln!(f, "{}: {}", k, e)?;
        }

        if let Some(e) = &self.parent {
            writeln!(f, "")?;
            writeln!(f, "Parent: ")?;
            writeln!(f, "{:?}", e)?;
        }

        Ok(())
    }
}

impl Environment {
    pub fn new(parent: Option<Environment>) -> Environment {
        let (index, parent) = match parent {
            Some(p) => (p.index + 1, Some(Box::new(p))),
            _ => (0, None)
        };

        debug!("New environment {}", index);

        Environment {
            index,
            parent,
            keys: HashMap::new(),
        }
    }

    pub fn global() -> Environment {
        let mut environment = Environment::new(None);
        environment.define_func("+", add());
        environment.define_func("-", sub());
        environment.define_func("*", mul());
        environment.define_func("/", div());
        environment.define_func("=", compare_floats!(|a, b| a == b));
        environment.define_func("<", compare_floats!(|a, b| a < b));
        environment.define_func(">", compare_floats!(|a, b| a > b));
        environment.define_func("<=", compare_floats!(|a, b| a <= b));
        environment.define_func(">=", compare_floats!(|a, b| a >= b));
        environment.define_func("remainder", remainder());
        environment.define_func("display", display());
        environment.define_func("newline", |_| {
            println!();
            Ok(Expr::None)
        });

        let now = SystemTime::now();

        environment.define_callable("runtime", Box::new(move |args| runtime(now, args)));

        environment
    }

    pub fn define_func(&mut self, name: &str, func: fn(args: Vec<Expr>) -> Result<Expr, String>) {
        self.define(name, Expr::Function(name.to_string(), func));
    }

    pub fn define_callable(&mut self, name: &str, action: BoxedAction) {
        self.define(name, Expr::Callable(Callable::new(name.to_string(), Rc::new(action))));
    }

    pub fn define(&mut self, key: &str, expr: Expr) {
//        debug!("D{} {}: {}", self.index, key, expr);
        self.keys.insert(key.to_string(), expr);
    }

    pub fn get(&self, key: &str) -> Result<&Expr, String> {
        debug!("Looking for '{}' in {}", key, self.index);
        match self.keys.get(key) {
            Some(v) => {
//                debug!("Found");
                Ok(v)
            }
            None => {
                if let Some(p) = &self.parent {
//                    debug!("{:?}", self);
                    debug!("Looking for env {}", self.index - 1);
                    p.get(key)
                } else {
//                    debug!("***");
//                    debug!("'{}' not found ", key);
//                    for (k, e) in self.keys.iter() {
//                        debug!("{}: {}", k, e);
//                    }
                    Err(format!("'{}' not found", key))
                }
            }
        }
    }
}

fn add() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() == 0 {
            return Err(format!("At least 1 argument required."));
        }

        match parse_floats(args) {
            Ok(nums) => Ok(Expr::Number(nums.iter().sum())),
            Err(e) => Err(e)
        }
    }
}

fn sub() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() == 0 {
            return Err(format!("At least 1 argument required."));
        }

        let floats = parse_floats(args)?;

        let (first, rest) = floats.split_first().unwrap();
        let first = *first;

        if floats.len() == 1 {
            return Ok(Expr::Number(-first));
        }

        let res = rest.iter().fold(first, |sum, a| sum - *a);

        Ok(Expr::Number(res))
    }
}

fn mul() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() == 0 {
            return Err(format!("At least 1 argument required."));
        }

        match parse_floats(args) {
            Ok(nums) => Ok(Expr::Number(nums.iter().fold(1.0, |sum, a| sum * *a))),
            Err(e) => Err(e)
        }
    }
}

fn div() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() < 2 {
            return Err(format!("At least 2 arguments required."));
        }

        let floats = parse_floats(args)?;

        let (first, rest) = floats.split_first().unwrap();
        let first = *first;

        let res = rest.iter().fold(first, |sum, a| sum / *a);

        Ok(Expr::Number(res))
    }
}

fn remainder() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 2 {
            return Err(format!("Exactly 2 arguments required."));
        }

        let floats = match parse_floats(args) {
            Ok(nums) => nums,
            Err(e) => return Err(e)
        };

        let a = floats[0];
        let b = floats[1];
        if b == 0.0 {
            return Err(format!("Division by zero."));
        }

        Ok(Expr::Number(a % b))
    }
}

fn display() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        let arg = args.first().unwrap();
        println!("{}", arg);

        Ok(Expr::None)
    }
}

fn runtime(start: SystemTime, _args: Vec<Expr>) -> Result<Expr, String> {
    let now = SystemTime::now();
    Ok(Expr::Number(now.duration_since(start).unwrap().as_micros() as f64))
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

    use crate::error::Error;
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    fn check_empty() {
        let environment = Environment::new(None);
        assert_that!(environment.keys.len(), equal_to(0));
    }

    #[test]
    fn check_globals() {
        let environment = Environment::global();
        assert_that!(environment.keys.len(), equal_to(13));
    }

    #[test]
    fn define_number() {
        let mut environment = Environment::new(None);
        environment.define("a", Expr::Number(123.0));

        let expr = environment.get("a").unwrap();
        assert_that!(expr, equal_to(&Expr::Number(123.0)));
    }

    #[test]
    fn define_identifier() {
        let mut environment = Environment::new(None);
        let expr = Expr::Identifier("abc".to_string());
        let expected = expr.clone();
        environment.define("a", expr);

        assert_that!(environment.get("a").unwrap(), equal_to( &expected));
    }

    #[test]
    fn enclosing() {
        let mut environment1 = Environment::new(None);
        environment1.define("a", Expr::Number(123.0));

        let environment2 = Environment::new(Some(environment1));
        assert_that!(environment2.get("a").unwrap(), equal_to(&Expr::Number(123.0)));
    }

    #[test]
    fn test_functions() {
        assert_eval("(+ 1 2)", Expr::Number(3.0));
        assert_eval("(= 1 2)", Expr::Bool(false));
        assert_eval("(< 1 2)", Expr::Bool(true));
        assert_eval("(> 1 2)", Expr::Bool(false));
        assert_eval("(<= 1 2)", Expr::Bool(true));
        assert_eval("(>= 1 2)", Expr::Bool(false));
        assert_eval("(+ 137 349)", Expr::Number(486.0));
        assert_eval("(- 1000)", Expr::Number(-1000.0));
        assert_eval("(- 1000 334)", Expr::Number(666.0));
        assert_eval("(* 5 99)", Expr::Number(495.0));
        assert_eval("(/ 10 5)", Expr::Number(2.0));
        assert_eval("(+ 2.7 10)", Expr::Number(12.7));
        assert_eval("(+ 1)", Expr::Number(1.0));
        assert_eval("(+1)", Expr::Number(1.0));
        assert_eval("(remainder 0 2)", Expr::Number(0.0));
        assert_eval("(remainder 1 2)", Expr::Number(1.0));
        assert_eval("(remainder (+ 1) 2)", Expr::Number(1.0));
        assert_eval("(display \"test\")", Expr::None);
    }

    #[test]
    fn test_invalid() {
        assert_invalid("(+)", "At least 1 argument required.".to_string());
        assert_invalid("(-)", "At least 1 argument required.".to_string());
        assert_invalid("(*)", "At least 1 argument required.".to_string());
        assert_invalid("(/)", "At least 2 arguments required.".to_string());
        assert_invalid("(/ 1)", "At least 2 arguments required.".to_string());
        assert_invalid("(remainder 0)", "Exactly 2 arguments required.".to_string());
        assert_invalid("(remainder 1)", "Exactly 2 arguments required.".to_string());
        assert_invalid("(remainder 1 2 3)", "Exactly 2 arguments required.".to_string());
        assert_invalid("(remainder 1 0)", "Division by zero.".to_string());
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

