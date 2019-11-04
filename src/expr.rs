use std::fmt;
use std::rc::Rc;
use std::fmt::{Formatter, Error};

pub type Action = dyn Fn(Vec<Expr>) -> Result<Expr, String>;
pub type BoxedAction = Box<Action>;
pub type RcAction = Rc<BoxedAction>;

#[derive(Clone)]
pub struct Callable {
    pub name: String,
    pub action: RcAction
}

impl Callable {
    pub fn new(name: String, action: RcAction) -> Callable {
        Callable{
            name,
            action
        }
    }
}

impl fmt::Debug for Callable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Empty,
    None,
    And(Vec<Expr>),
    Bool(bool),
    Cond(Vec<Expr>, Vec<Expr>),
    Define(String, Box<Expr>),
    Expression(Box<Expr>, Vec<Expr>),
    Function(String, fn(Vec<Expr>) -> Result<Expr, String>),
    Callable(Callable),
    Identifier(String),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    List(Vec<Expr>),
    Not(Box<Expr>),
    Number(f64),
    Or(Vec<Expr>),
    Predicate(Box<Expr>, Vec<Expr>),
    Lambda(Vec<Expr>, Vec<Expr>),
    String(String),
}

impl Expr {
    pub fn to_f64(&self) -> Result<f64, String> {
        match *self {
            Expr::Number(n) => Ok(n),
            _ => Err(format!("Cannot convert {} to f64", self))
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        return write!(f, "None");
        match self {
            Expr::And(exprs) => write!(f, "(and {})", build_str(exprs)),
            Expr::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Expr::Cond(predicate_branches, else_branch) => {
                let predicates = build_str(predicate_branches);

                if else_branch.len() == 0 {
                    write!(f, "(cond {})", predicates)
                } else {
                    write!(f, "(cond {} {})", predicates, build_str(else_branch))
                }
            }
            Expr::Define(name, _) => write!(f, "{}", name),
            Expr::Empty => write!(f, "()"),
            Expr::Expression(name, exprs) => {
                if exprs.len() == 0 {
                    write!(f, "({})", name)
                } else {
                    write!(f, "({} {})", name, build_str(exprs))
                }
            }
            Expr::Function(name, _) => write!(f, "native '{}'", name),
            Expr::Callable(callable) => write!(f, "native '{}'", callable.name),
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::If(predicate, then_branch, else_branch) => {
                let predicate = predicate.as_ref();
                let then_branch = then_branch.as_ref();

                match else_branch {
                    Some(else_branch) => write!(f, "(if {} {} {})", predicate, then_branch, else_branch.as_ref()),
                    None => write!(f, "(if {} {})", predicate, then_branch)
                }
            }
            Expr::List(exprs) => write!(f, "[{}]", build_str(exprs)),
            Expr::None => write!(f, "No result"),
            Expr::Not(expr) => write!(f, "(not {})", expr),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Or(exprs) => write!(f, "(or {})", build_str(exprs)),
            Expr::Predicate(test, exprs) => write!(f, "({} {})", test, build_str(exprs)),
            Expr::Lambda(_, _) => write!(f, "lambda"),
            Expr::String(s) => write!(f, "{}", s),
        }
    }
}

fn build_str(exprs: &Vec<Expr>) -> String {
    let res = exprs.iter().map(|e| e.to_string());
    let res: Vec<String> = res.collect();
    res.join(",")
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn test_fn() {
        let one = Expr::Function("one".to_string(), |_args: Vec<Expr>| -> Result<Expr, String> {
            Ok(Expr::Number(1.0))
        });

        match one {
            Expr::Function(name, f) => {
                let res = f(vec![]);
                assert_that!(name, equal_to("one".to_string()));
                assert_that!(res.unwrap().to_f64().unwrap(), equal_to(1.0));
            }
            _ => {}
        }
    }
}