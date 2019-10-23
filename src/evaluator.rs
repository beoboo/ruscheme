use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub struct Evaluator {
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator{}
    }

    pub fn evaluate(&self, expr: Expr) -> Result<i64, String> {
        match expr {
            Expr::Number(n) => {
                Ok(n)
            }
            Expr::Expression(operator, operands) => {
                match operator.as_ref() {
                    "+" => {
                        let mut res = 0;
                        for operand in operands {
                            res += self.evaluate(operand).unwrap()
                        }
                        Ok(res)
                    }
                    _ => return Err(format!("Undefined operator {}", operator))
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn eval_number() {
        let evaluator = Evaluator::new();
        let res = evaluator.evaluate(Expr::Number(123));

        assert_that!(res, is(Ok(123)));
    }
}
