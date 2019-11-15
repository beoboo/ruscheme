use std::iter::Peekable;
use std::slice::Iter;

use log::debug;

use crate::error::{Error, report_stage_error};
use crate::error::Error::UnterminatedInput;
use crate::expr::*;
use crate::token::*;
use crate::byte_code::ByteCode;

#[derive(Debug)]
pub struct Compiler {}

type IterToken<'a> = Iter<'a, Token>;
type PeekableToken<'a> = Peekable<IterToken<'a>>;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&self, tokens: Vec<Token>) -> Result<Vec<ByteCode>, Error> {
        if tokens.len() == 0 {
            return report_error("No tokens available.");
        }

        let mut it = tokens.iter().peekable();

        let token_type = peek(&mut it);
        if token_type == TokenType::EOF {
            return report_error("Unexpected EOF.");
        }

        self.instructions(&mut it)
    }

    fn instructions(&self, it: &mut PeekableToken) -> Result<Vec<ByteCode>, Error> {
        let mut instructions = Vec::new();

        loop {
            if peek(it) == TokenType::EOF {
                instructions.push(emit(ByteCode::Return, it)?);
                break;
            }

            let instruction = self.primitive(it)?;
            instructions.push(instruction)
        }
        Ok(instructions)
    }

    fn primitive(&self, it: &mut PeekableToken) -> Result<ByteCode, Error> {
        match peek(it) {
            TokenType::Number(n) => emit(ByteCode::Constant(n), it),
            t => report_error(format!("Undefined token type: '{}'", t))
        }
    }
}

fn emit(byte_code: ByteCode, it: &mut PeekableToken) -> Result<ByteCode, Error> {
    advance(it)?;

    Ok(byte_code)
}

fn report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    report_stage_error(err, "compiler")
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::lexer::Lexer;

    use super::*;
    use crate::byte_code::ByteCode;

    #[test]
    fn compile_empty() {
        let compiler = Compiler::new();
        assert_that!(compiler.compile(vec![]).is_err(), is(true));
    }

    #[test]
    fn compile_none() {
        assert_compile("1", vec![
            ByteCode::Constant(1.0),
            ByteCode::Return
        ]);
    }

    fn compile(source: &str) -> Result<Vec<ByteCode>, Error> {
        debug!("Compiling: '{}'", source);
        let lexer = Lexer::new();
        let tokens = lexer.lex(source)?;

        let compiler = Compiler::new();
        compiler.compile(tokens)
    }

    fn assert_compile(source: &str, instructions: Vec<ByteCode>) {
        let actual = compile(source).unwrap();

        assert_that!(actual, equal_to(instructions));
    }

    fn assert_invalid(source: &str, message: &str) {
        let error = compile(source).unwrap_err();

        assert_that!(error.to_string(), equal_to(message));
    }
}