use std::iter::Peekable;
use std::slice::Iter;

use log::debug;

use crate::byte_code::*;
use crate::error::{Error, report_stage_error};
use crate::error::Error::UnterminatedInput;
use crate::token::*;

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
        let mut instructions = vec![];

        let token_type = peek(&mut it);
        if token_type == TokenType::EOF {
            return report_error("Unexpected EOF.");
        }

        self.instructions(&mut instructions, &mut it)?;

        Ok(instructions)
    }

    fn instructions(&self, instructions: &mut Instructions, it: &mut PeekableToken) -> Result<(), Error> {
        loop {
            if peek(it) == TokenType::EOF {
                break;
            }

            let instruction = self.primitive(instructions, it)?;
        }

        Ok(())
    }

    fn primitive(&self, instructions: &mut Instructions, it: &mut PeekableToken) -> Result<(), Error> {
        match advance(it)?.token_type {
            TokenType::Number(n) => self.constant(instructions, ByteCode::Constant(n)),
            TokenType::Paren('(') => self.expression(instructions),
            t => Err(Error::Compiler(format!("Undefined token type: '{}'", t)))
        }
    }

    fn constant(&self, instructions: &mut Instructions, byte_code: ByteCode) -> Result<(), Error> {
        instructions.push(byte_code);

        Ok(())
    }

    fn expression(&self, instructions: &mut Instructions) -> Result<(), Error> {
        Ok(())
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

    use crate::byte_code::ByteCode;
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn compile_empty() {
        let compiler = Compiler::new();
        assert_that!(compiler.compile(vec![]).is_err(), is(true));
    }

    #[test]
    fn compile_numbers() {
        assert_compile("486", vec![
            ByteCode::Constant(486.0),
//            ByteCode::Return
        ]);
    }
//
//    #[test]
//    fn compile_expressions() {
//        assert_compile("(+ 137 349)", vec![
//            ByteCode::Constant(137.0),
//            ByteCode::Constant(349.0),
//            ByteCode::Add,
//        ]);
//    }

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