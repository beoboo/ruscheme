use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Empty,
    Bool(bool),
    Conditional(Vec<Expr>),
    Predicate(Box<Expr>, Vec<Expr>),
    Identifier(String),
    Number(f64),
    Definition(String, Box<Expr>),
    List(Vec<Expr>),
    Expression(String, Vec<Expr>),
    Function(String, fn(Vec<Expr>) -> Result<Expr, String>),
    Procedure(String, Vec<Expr>, Box<Expr>),
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
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Conditional(_) => write!(f, "cond"),
            Expr::Definition(name, _) => write!(f, "{}", name),
            Expr::Empty => write!(f, "()"),
            Expr::Expression(name, exprs) => {
                if exprs.len() == 0 {
                    write!(f, "({})", name)
                } else {
                    let s = exprs.iter().map(|e| e.to_string());
                    let s: Vec<String> = s.collect();
                    let s = s.join(",");

                    write!(f, "({}{})", name, s)
                }
            }
            Expr::List(exprs) => {
                let s = exprs.iter().map(|e| e.to_string());
                let s: Vec<String> = s.collect();
                let s = s.join(",");

                write!(f, "[{}]", s)
            }
            Expr::Function(name, _) => write!(f, "native \"{}\"", name),
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::Predicate(_, _) => write!(f, "predicate"),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Procedure(name, _, _) => write!(f, "procedure \"{}\"", name),
//            e => write!(f, "Undefined expression \"{:?}\"", e),
        }
    }
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