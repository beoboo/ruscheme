use std::fmt;
use std::fmt::{Error, Formatter};
use std::rc::Rc;

use crate::environment::Environment;

pub type Action = dyn Fn(Vec<Expr>) -> Result<Expr, String>;
pub type BoxedAction = Box<Action>;
pub type RcAction = Rc<BoxedAction>;

#[derive(Clone)]
pub struct Callable {
    pub name: String,
    pub action: RcAction,
}

impl Callable {
    pub fn new(name: String, action: RcAction) -> Callable {
        Callable {
            name,
            action,
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
    Expression(Vec<Expr>),
    Function(String, fn(Vec<Expr>) -> Result<Expr, String>),
    Callable(Callable),
    Identifier(String),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    //    List(Vec<Expr>),
    Not(Box<Expr>),
    Number(f64),
    Or(Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
    Predicate(Box<Expr>, Vec<Expr>),
    Quote(Box<Expr>),
    Lambda(Vec<Expr>, Vec<Expr>, Option<Environment>),
    String(String),
}

impl Expr {
    pub fn to_f64(&self) -> Result<f64, String> {
        match *self {
            Expr::Number(n) => Ok(n),
            _ => Err(format!("Cannot convert {} to f64", self))
        }
    }

    pub fn is_empty(&self) -> bool {
        *self == Expr::Empty
    }

    pub fn is_pair(&self) -> bool {
        match *self {
            Expr::Pair(_, _) => true,
            _ => false
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //        return write!(f, "None");
        match self {
            Expr::And(exprs) => write!(f, "(and {})", join_exprs(exprs, " ")),
            Expr::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Expr::Cond(predicate_branches, else_branch) => {
                let predicates = join_exprs(predicate_branches, " ");

                if else_branch.len() == 0 {
                    write!(f, "(cond ({}))", predicates)
                } else {
                    write!(f, "(cond ({}) {})", predicates, join_exprs(else_branch, " "))
                }
            }
            Expr::Define(name, _) => write!(f, "{}", name),
            Expr::Empty => write!(f, "()"),
            Expr::Expression(exprs) => {
                write!(f, "({})", join_exprs(exprs, " "))
            }
            Expr::Function(name, _) => write!(f, "native '{}'", name),
            Expr::Callable(callable) => write!(f, "native '{}'", callable.name),
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::If(predicate, then_branch, else_branch) => {
                let predicate = predicate.as_ref();
                let then_branch = then_branch.as_ref();

                match else_branch {
                    Some(else_branch) => write!(f, "(if ({}) {} {})", predicate, then_branch, else_branch.as_ref()),
                    None => write!(f, "(if ({}) {})", predicate, then_branch)
                }
            }
            Expr::Lambda(params, body, _) => write!(f, "lambda ({}) {}", join_exprs(params, " "), join_exprs(body, " ")),
            Expr::None => write!(f, "No result"),
            Expr::Not(expr) => write!(f, "(not {})", expr),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Or(exprs) => write!(f, "(or {})", join_exprs(exprs, " ")),
            Expr::Pair(first, rest) => {
                if first.is_empty() {
                    write!(f, "(())")
                } else {
                    let mut res = String::new();

                    let mut first = first.as_ref();
                    let mut rest = rest.as_ref();

                    loop {
                        if res.len() > 0 {
                            res += " ";
                        }
                        res += first.to_string().as_str();

                        match rest {
                            Expr::Pair(head, tail) => {
                                first = head;
                                rest = tail;
                            }
                            Expr::Empty => {
                                break;
                            }
                            e => {
                                res += " . ";
                                res += e.to_string().as_str();
                                break;
                            }
                        }
                    }
                    write!(f, "({})", res)
                }
            }
            Expr::Predicate(test, exprs) => write!(f, "({} {})", test, join_exprs(exprs, " ")),
            Expr::Quote(expr) => write!(f, "(quote {})", expr),
            Expr::String(s) => write!(f, "{}", s),
        }
    }
}

fn join_exprs(exprs: &Vec<Expr>, sep: &str) -> String {
    let res = exprs.iter().map(|e| e.to_string());
    let res: Vec<String> = res.collect();
    res.join(sep)
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