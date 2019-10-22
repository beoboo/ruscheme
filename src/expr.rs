#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(i64),
    Expression(String, Vec<Expr>),
}

impl Expr {
    pub fn eval(&self) -> i64 {
        match &*self {
            Expr::Number(n) => {
                *n
            }
            Expr::Expression(operator, operands) => {
                match operator.as_ref() {
                    "+" => {
                        let mut res = 0;
                        for operand in operands {
                            res += operand.eval()
                        }
                        res
                    }
                    _ => panic!("Undefined operator {}", operator)
                }
            }
        }
    }
}
