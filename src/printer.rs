use std::iter::Peekable;
use std::str::Chars;

use log::debug;

use crate::error::{Error, report_stage_error};
use crate::token::*;

#[derive(Debug)]
pub struct Printer {}

type PeekableChar<'a> = Peekable<Chars<'a>>;

impl Printer {
    pub fn new() -> Printer {
        Printer {}
    }

    pub fn print(&self, tokens: Vec<Token>) -> Result<String, Error> {
        let mut it = tokens.iter();
        let mut res = String::new();
        let mut after_open_paren = true;

        while let Some(t) = it.next() {
//            println!("{}", t);
            if t.is_eof() {
                break;
            }

            let prefix = match t.token_type {
                TokenType::Paren('(') => {
                    let prefix = if res.is_empty() || after_open_paren {
                        ""
                    } else {
                        " "
                    };
                    after_open_paren = true;
                    prefix
                }
                TokenType::Paren(')') => {
                    after_open_paren = false;
                    ""
                }
                _ => {
                    if after_open_paren {
                        after_open_paren = false;
                        ""
                    } else {
                        " "
                    }
                }
            };

            debug!("{}", t);
            res += format!("{}{}", prefix, t).as_str();
        }
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use crate::desugarizer::Desugarizer;
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn print_tests() {
//        env_logger::init();
        assert_print("", "");
        assert_print("()", "()");
        assert_print("(+ 1 2)", "(+ 1 2)");
        assert_print("'(+ 1 2)", "(quote (+ 1 2))");
        assert_print("'((a b) (c d))", "(quote ((a b) (c d)))");
    }

    fn assert_invalid(source: &str, message: &str) {
        let error = print(source).unwrap_err();

        assert_that!(error.to_string(), equal_to(message));
    }

    fn assert_print(source: &str, expected: &str) {
        let res = print(source).unwrap();
        assert_that!(res, equal_to(expected));
    }

    fn print(source: &str) -> Result<String, Error> {
        let lexer = Lexer::new();
        let desugarizer = Desugarizer::new();
        let printer = Printer::new();

        let tokens = lexer.lex(source)?;
        let tokens = desugarizer.desugar(tokens)?;

        printer.print(tokens)
    }
}
