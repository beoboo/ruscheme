use std::iter::Peekable;
use std::str::Chars;

use log::debug;

use crate::error::{Error, report_stage_error};
use crate::token::*;

#[derive(Debug)]
pub struct Desugarizer {}

impl Desugarizer {
    pub fn new() -> Desugarizer {
        Desugarizer {}
    }

    pub fn desugar(&self, tokens: Vec<Token>) -> Result<Vec<Token>, Error> {
        let mut it = tokens.iter().peekable();
        let mut desugared = vec![];

        loop {
            let token = advance(&mut it)?;

            match token.token_type {
                TokenType::QuotationMark => self.quote(&mut desugared, &mut it, token.line)?,
                TokenType::EOF => {
                    desugared.push(token.clone());
                    break;
                }
                _ => desugared.push(token.clone())
            };
        }

        Ok(desugared)
    }

    fn quote(&self, desugared: &mut Vec<Token>, it: &mut PeekableToken, line: u32) -> Result<(), Error> {
        desugared.push(Token::new(TokenType::Paren('('), line));
        desugared.push(Token::new(TokenType::Quote, line));

        let token = advance(it)?;

        match token.token_type {
            TokenType::QuotationMark => self.quote(desugared, it, token.line)?,
            TokenType::Paren('(') => {
                self.expression(token, desugared, it)?
            }
            _ => desugared.push(token.clone())
        };

        desugared.push(Token::new(TokenType::Paren(')'), line));

        Ok(())
    }

    fn expression(&self, token: Token, desugared: &mut Vec<Token>, it: &mut PeekableToken) -> Result<(), Error> {
        debug!("Expression");

        desugared.push(token.clone());

        loop {
            let token = advance(it)?;

            match token.token_type {
                TokenType::QuotationMark => self.quote(desugared, it, token.line)?,
                TokenType::Paren('(') => {
                    self.expression(token, desugared, it)?
                },
                TokenType::Paren(')') => {
                    desugared.push(token.clone());
                    break;
                }
                TokenType::EOF => break,
                _ => desugared.push(token.clone())
            }
        }

        Ok(())
    }
}

fn _report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    report_stage_error(err, "desugarer")
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn desugar_empty() {
        let source = desugar("").unwrap();

        assert_that!(source.len(), equal_to(1));
    }

    #[test]
    fn desugar_quote() {
//        env_logger::init();
        assert_valid("'a", vec![
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Identifier("a".to_string()),
            TokenType::Paren(')'),
        ]);
        assert_valid("''a", vec![
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Identifier("a".to_string()),
            TokenType::Paren(')'),
            TokenType::Paren(')'),
        ]);
        assert_valid("'(a)", vec![
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Paren('('),
            TokenType::Identifier("a".to_string()),
            TokenType::Paren(')'),
            TokenType::Paren(')'),
        ]);
        assert_valid("'((a))", vec![
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Paren('('),
            TokenType::Paren('('),
            TokenType::Identifier("a".to_string()),
            TokenType::Paren(')'),
            TokenType::Paren(')'),
            TokenType::Paren(')'),
        ]);
        assert_valid("'('a)", vec![
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Paren('('),
            TokenType::Paren('('),
            TokenType::Quote,
            TokenType::Identifier("a".to_string()),
            TokenType::Paren(')'),
            TokenType::Paren(')'),
            TokenType::Paren(')'),
        ]);
    }

    fn assert_valid(source: &str, expected: Vec<TokenType>) {
        debug!("Parsing: {}", source);
        let tokens = desugar(source).unwrap();

        let expected_length = expected.len() -
            if expected[expected.len() - 1] == TokenType::EOF { 1 } else { 0 };

        for t in tokens.iter() {
            debug!("{:?}", t);
        }
        assert_that!(tokens.len() - 1, equal_to(expected_length));

        for (i, token) in tokens.iter().enumerate() {
            if token.token_type == TokenType::EOF {
                break;
            }
            assert_that!(&token.token_type, equal_to(&expected[i]));
        }
    }

    fn desugar(source: &str) -> Result<Vec<Token>, Error> {
        let lexer = Lexer::new();
        let desugarer = Desugarizer::new();
        let tokens = lexer.lex(source)?;

        desugarer.desugar(tokens)
    }
}
