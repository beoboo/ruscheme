use std::{fmt, error};

#[derive(Debug, Clone)]
pub enum Error {
    Lexer(String),
    Parser(String),
    Evaluator(String),
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Error::Lexer(message) => write!(f, "{}", message),
            Error::Parser(message) => write!(f, "{}", message),
            Error::Evaluator(message) => write!(f, "{}", message),
        }
    }
}

impl error::Error for Error {}
