use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ByteCode {
    Add,
    Constant(f64),
}

impl fmt::Display for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ByteCode::Add => write!(f, "ADD"),
            ByteCode::Constant(n) => write!(f, "CONSTANT {}", n),
        }
    }
}

pub(crate) type Instructions = Vec<ByteCode>;