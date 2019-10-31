use std::{fmt, error};

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnterminatedInput,
    Lexer(String),
    Parser(String),
    Evaluator(String),
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Error::UnterminatedInput => write!(f, "EOF"),
            Error::Lexer(message) => write!(f, "{}", message),
            Error::Parser(message) => write!(f, "{}", message),
            Error::Evaluator(message) => write!(f, "{}", message),
        }
    }
}

impl error::Error for Error {}
