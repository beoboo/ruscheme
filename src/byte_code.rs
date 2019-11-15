use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ByteCode {
    Constant(f64),
    Return,
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ByteCode::Constant(n) => write!(f, "CONSTANT {}", n),
            ByteCode::Return => write!(f, "RETURN"),
        }
    }
}
