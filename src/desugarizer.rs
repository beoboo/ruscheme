use std::iter::Peekable;
use std::str::Chars;

use log::debug;

use crate::error::{Error, report_stage_error};
use crate::token::*;

#[derive(Debug)]
pub struct Desugarizer {}

type PeekableChar<'a> = Peekable<Chars<'a>>;

impl Desugarizer {
    pub fn new() -> Desugarizer {
        Desugarizer {}
    }

    pub fn desugar(&self, source: &str) -> Result<String, Error> {
        let mut it = source.chars().peekable();
        let mut res = String::new();
        let mut quoting = false;

        while let Some(ch) = it.next() {
            match ch {
                '"' => {
                    quoting = !quoting;
                    res.push(ch);
                }
                '\'' => {
                    if quoting {
                        res.push(ch)
                    } else {
                        self.quote(&mut res, &mut it);
                    }
                }
                _ => res.push(ch)
            }
        };

        Ok(res)
    }

    fn quote(&self, res: &mut String, it: &mut PeekableChar) -> Result<(), Error> {
        res.push_str("(quote ");

        let mut indent = 0;
        let mut quoting = false;

        while let Some(ch) = it.next() {
            match ch {
                '"' => {
                    quoting = !quoting;
                    res.push(ch);
                }
                '\'' => {
                    if quoting {
                        res.push(ch)
                    } else {
                        self.quote(res, it);
                    }
                }
                '(' => {
                    debug!("(");
                    indent += 1;
                    res.push('(');
                }
                ')' => {
                    debug!(") {}", indent);
                    indent -= 1;
                    res.push(')');

                    if indent == 0 {
                        break;
                    }
                }
                _ => res.push(ch)
            }
        };

        res.push(')');

        Ok(())
    }
}

fn _report_error<S: Into<String>, T>(err: S) -> Result<T, Error> {
    report_stage_error(err, "desugarer")
}

#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn desugar_empty() {
        let source = desugar("").unwrap();

        assert_that!(source, equal_to(""));
    }

    #[test]
    fn desugar_quote() {
        assert_valid("'a", "(quote a)");
        assert_valid("'(expr)", "(quote (expr))");
        assert_valid("'(expr a b c)", "(quote (expr a b c))");
        assert_valid("'(expr) a", "(quote (expr)) a");
        assert_valid("'(expr (a) b c)", "(quote (expr (a) b c))");
        assert_valid("'((expr (a) b) c)", "(quote ((expr (a) b) c))");
    }

    #[test]
    fn desugar_double_quote() {
//        env_logger::init();
        assert_valid("''a", "(quote (quote a))");
        assert_valid("''(a)", "(quote (quote (a)))");
        assert_valid("''((a) b)", "(quote (quote ((a) b)))");
        assert_valid("'('a) b", "(quote ((quote a) b))");
    }

    #[test]
    fn ignore_double_quotes() {
//        env_logger::init();
        assert_valid("\"'a\"", "\"'a\"");
        assert_valid("'\"'a\"", "(quote \"'a\")");
    }

    fn assert_valid(source: &str, expected: &str) {
        debug!("Parsing: {}", source);
        let source = desugar(source).unwrap();

        assert_that!(source, equal_to(expected));
    }

    fn desugar(source: &str) -> Result<String, Error> {
        let desugarer = Desugarizer::new();
        desugarer.desugar(source)
    }
}
