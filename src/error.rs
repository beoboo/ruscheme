use std::{fmt, error};

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    Analyzer(String),
    Compiler(String),
    Evaluator(String),
    Lexer(String),
    Parser(String),
    VirtualMachine(String),
    UndefinedStage(String),
    UnterminatedInput,
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Error::Analyzer(message) => write!(f, "{}", message),
            Error::Compiler(message) => write!(f, "{}", message),
            Error::Evaluator(message) => write!(f, "{}", message),
            Error::Lexer(message) => write!(f, "{}", message),
            Error::Parser(message) => write!(f, "{}", message),
            Error::VirtualMachine(message) => write!(f, "{}", message),
            Error::UndefinedStage(stage) => write!(f, "Undefined stage: '{}'", stage),
            Error::UnterminatedInput => write!(f, "EOF"),
        }
    }
}

impl error::Error for Error {}

pub fn report_stage_error<S: Into<String>, T>(err: S, stage: &str) -> Result<T, Error> {
    let error = err.into();
    if &error == "EOF" {
        return Err(Error::UnterminatedInput);
    }

    match stage {
        "analyzer" => Err(Error::Analyzer(error)),
        "compiler" => Err(Error::Compiler(error)),
        "evaluator" => Err(Error::Evaluator(error)),
        "parser" => Err(Error::Parser(error)),
        _ => Err(Error::UndefinedStage(stage.into()))
    }
}

