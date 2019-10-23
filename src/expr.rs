#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(i64),
    Expression(String, Vec<Expr>),
}
