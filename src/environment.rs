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
        environment.define_func("accumulate", accumulate());
        environment.define_func("append", append());
        environment.define_func("car", car());
        environment.define_func("cdr", cdr());
        environment.define_func("cons", cons());
        environment.define_func("display", display());
        environment.define_func("filter", filter());
        environment.define_func("length", length());
        environment.define_func("list", list());
        environment.define_func("map", map());
        environment.define_func("newline", newline());
        environment.define_func("nil", nil());
        environment.define_func("null?", null());
        environment.define_func("pair?", pair());
        environment.define_func("remainder", remainder());

        let now = SystemTime::now();
        environment.define_callable("runtime", Box::new(move |args| runtime(now, args)));
        environment.define_func("square", square());

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
//        debug!("Looking for '{}' in {}", key, self.index);
        match self.keys.get(key) {
            Some(v) => {
                Ok(v)
            }
            None => {
                if let Some(p) = &self.parent {
//                    debug!("Looking for env {}", self.index - 1);
                    p.get(key)
                } else {
                    Err(format!("'{}' not found", key))
                }
            }
        }
    }
}

fn accumulate() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
//        if args.len() != 1 {
//            return Err(format!("Exactly 1 argument required."));
//        }
//
//        match parse_floats(args) {
//            Ok(nums) => Ok(Expr::Number(nums.iter().sum())),
//            Err(e) => Err(e)
//        }
        Ok(Expr::Empty)
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

fn append() -> fn(Vec<Expr>) -> Result<Expr, String> {
    fn append_iter(first: &Expr, second: Expr) -> Result<Expr, String> {
        if first.is_empty() {
            Ok(second)
        } else {
            match first {
                Expr::Pair(head, tail) => {
                    Ok(build_pair(head.as_ref().clone(), append_iter(tail.as_ref(), second)?))
                }
                _ => return Err(format!("Argument is not a list."))
            }
        }
    };

    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.is_empty() {
            Ok(Expr::Empty)
        } else if args.len() == 1 {
            Ok(args[0].clone())
        } else {
            Ok(append_iter(&args[0], args[1].clone())?)
        }
    }
}

fn car() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        match &args[0] {
            Expr::Pair(first, _) => Ok(first.as_ref().clone()),
            e => return Err(format!("Expected pair (found: {})", e))
        }
    }
}

fn cdr() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        match &args[0] {
            Expr::Pair(_, rest) => Ok(rest.as_ref().clone()),
            e => return Err(format!("Expected pair (found: {})", e))
        }
    }
}

fn cons() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 2 {
            return Err(format!("Exactly 2 arguments required."));
        }

        Ok(build_pair(args[0].clone(), args[1].clone()))
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

fn display() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        let arg = args.first().unwrap();
        print!("{}", arg);

        Ok(Expr::None)
    }
}

fn filter() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
//        if args.len() != 1 {
//            return Err(format!("Exactly 1 argument required."));
//        }
//
//        let arg = args.first().unwrap();
//        print!("{}", arg);
//
        Ok(Expr::None)
    }
}

fn length() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        let mut length = 0;
        let mut expr = &args[0];
        while expr != &Expr::Empty {
            match expr {
                Expr::Pair(_, rest) => {
                    length += 1;
                    expr = rest.as_ref()
                }
                _ => return Err(format!("Argument is not a list."))
            };
        }

        Ok(Expr::Number(length as f64))
    }
}

fn list() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        Ok(build_cons(args.as_slice()))
    }
}

fn map() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
//        if args.len() != 1 {
//            return Err(format!("Exactly 1 argument required."));
//        }
//
//        let arg = args.first().unwrap();
//        print!("{}", arg);
//
        Ok(Expr::None)
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

fn nil() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        Ok(Expr::Empty)
    }
}

fn newline() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |_| {
        println!();
        Ok(Expr::None)
    }
}

fn null() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        Ok(Expr::Bool(args[0] == Expr::Empty))
    }
}

fn pair() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        Ok(Expr::Bool(args[0].is_pair()))
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

fn runtime(start: SystemTime, _args: Vec<Expr>) -> Result<Expr, String> {
    let now = SystemTime::now();
    Ok(Expr::Number(now.duration_since(start).unwrap().as_micros() as f64))
}

fn square() -> fn(Vec<Expr>) -> Result<Expr, String> {
    |args: Vec<Expr>| -> Result<Expr, String> {
        if args.len() != 1 {
            return Err(format!("Exactly 1 argument required."));
        }

        let float = args[0].to_f64()?;

        Ok(Expr::Number(float * float))
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

fn build_cons(args: &[Expr]) -> Expr {
    if args.len() == 0 {
        Expr::Empty
    } else {
        build_pair(args[0].clone(), build_cons(&args[1..]))
    }
}

fn build_pair(head: Expr, tail: Expr) -> Expr {
    Expr::Pair(Box::new(head), Box::new(tail))
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
        env_logger::init();
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

        assert_eval("(display \"test\")", Expr::None);
        assert_eval("(cons () ())", Expr::Pair(Box::new(Expr::Empty), Box::new(Expr::Empty)));
        assert_eval("(cons () 1)", Expr::Pair(Box::new(Expr::Empty), Box::new(Expr::Number(1.0))));
        assert_eval("(cons 1 ())", Expr::Pair(Box::new(Expr::Number(1.0)), Box::new(Expr::Empty)));
        assert_eval("(cons 1 2)", Expr::Pair(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0))));
        assert_eval("(car (cons 1 2))", Expr::Number(1.0));
        assert_eval("(cdr (cons 1 2))", Expr::Number(2.0));
        assert_eval("(cdr (cons 1 2))", Expr::Number(2.0));
        assert_eval("(append)", Expr::Empty);
        assert_eval("(append ())", Expr::Empty);
        assert_eval("(append (list 1))", Expr::Pair(Box::new(Expr::Number(1.0)), Box::new(Expr::Empty)));
        assert_eval("(append (list 1) 2)", Expr::Pair(Box::new(Expr::Number(1.0)), Box::new(Expr::Number(2.0))));
        assert_eval("(remainder 0 2)", Expr::Number(0.0));
        assert_eval("(remainder 1 2)", Expr::Number(1.0));
        assert_eval("(remainder (+ 1) 2)", Expr::Number(1.0));
        assert_eval("(list 1 2)", Expr::Pair(
            Box::new(Expr::Number(1.0)),
            Box::new(Expr::Pair(
                Box::new(Expr::Number(2.0)),
                Box::new(Expr::Empty),
            ))),
        );
        assert_eval("(null? ())", Expr::Bool(true));
        assert_eval("(null? (list 1 2))", Expr::Bool(false));
        assert_eval("(pair? ())", Expr::Bool(false));
        assert_eval("(pair? (list 1 2))", Expr::Bool(true));
        assert_eval("(square 2)", Expr::Number(4.0));
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
        assert_invalid("(length)", "Exactly 1 argument required.".to_string());
        assert_invalid("(length 1)", "Argument is not a list.".to_string());
        assert_invalid("(remainder 1 0)", "Division by zero.".to_string());
        assert_invalid("(append 1 0)", "Argument is not a list.".to_string());
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
        debug!("{}", source);
        let lexer = Lexer::new();
        let parser = Parser::new();
        let evaluator = Evaluator::new();

        let tokens = lexer.lex(source)?;
        let exprs = parser.parse(tokens)?;

        let mut res = Err(Error::Evaluator(format!("No expressions to eval.")));

        for expr in exprs {
            res = evaluator.evaluate(&expr, globals);
        }

        res
    }
}

