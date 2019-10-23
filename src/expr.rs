use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Empty,
    Bool(bool),
    Symbol(String),
    Number(f64),
    Expression(Vec<Expr>),
    Func(fn(Vec<Expr>) -> Result<Expr, String>),
}

impl Expr {
    pub(crate) fn to_f64(&self) -> Result<f64, String>{
        match *self {
            Expr::Number(n) => Ok(n),
            _ => Err(format!("Cannot convert {} to f64", self))
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Empty => write!(f, "()"),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::Expression(list) => {
                let s = list.into_iter().map(|e| e.to_string());
                let s :Vec<String> = s.collect();
                let s = s.join(",");

                write!(f, "({})", s)
            }
            e => write!(f, "Undefined {:?}", e),
        }
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn test_fn() {
        let one = Expr::Func(|_args: Vec<Expr>| -> Result<Expr, String> {
            Ok(Expr::Number(1.0))
        });

        match one {
            Expr::Func(f) => {
                let res = f(vec![]);
                assert_that!(res.unwrap().to_f64().unwrap(), equal_to(1.0));
            },
            _ => {}
        }
    }
}