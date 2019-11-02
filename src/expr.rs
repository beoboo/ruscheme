use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Empty,
    None,
    And(Vec<Expr>),
    Bool(bool),
    Cond(Vec<Expr>, Vec<Expr>),
    Define(String, Box<Expr>),
    Expression(Box<Expr>, Vec<Expr>),
    Function(String, fn(Vec<Expr>) -> Result<Expr, String>),
    Identifier(String),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    List(Vec<Expr>),
    Not(Box<Expr>),
    Number(f64),
    Or(Vec<Expr>),
    Predicate(Box<Expr>, Vec<Expr>),
    Procedure(String, Vec<Expr>, Vec<Expr>),
    String(String),
}

impl Expr {
    pub(crate) fn to_f64(&self) -> Result<f64, String> {
        match *self {
            Expr::Number(n) => Ok(n),
            _ => Err(format!("Cannot convert {} to f64", self))
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            Expr::Function(name, _) => write!(f, "native \"{}\"", name),
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
            Expr::Procedure(name, _, _) => write!(f, "procedure \"{}\"", name),
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