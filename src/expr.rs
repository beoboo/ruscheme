use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Symbol(String),
    Number(f64),
    Expression(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::Expression(list) => {
                let s = list.into_iter().map(|e| e.to_string());
                let s :Vec<String> = s.collect();
                let s = s.join(",");

                write!(f, "({})", s)
            }
        }
    }
}