use std::iter::Peekable;
use std::str::Chars;

use crate::error::Error;
use crate::token::*;

#[derive(Debug)]
pub struct Lexer {}

type PeekableChar<'a> = Peekable<Chars<'a>>;

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {}
    }

    pub fn lex(&self, source: &str) -> Result<Vec<Token>, Error> {
        let mut it = source.chars().peekable();
        let mut tokens = Vec::new();
        let mut line = 1;

        while let Some(&c) = it.peek() {
            let res = match c {
                '0'..='9' => number(&mut it),
                '+' | '-' | '*' | '/' | '=' | '<' | '>' => symbol(&mut it),
                '(' | ')' => paren(c, &mut it),
                ' ' | '\t' => {
                    it.next();
                    continue;
                }
                '"' => string(&mut it),
                '\n' => {
                    line += 1;
                    it.next();
                    continue;
                }
                _ => {
                    if is_alphanum(peek(&mut it)) {
                        identifier(&mut it)
                    } else {
                        return Err(Error::Lexer(format!("Invalid token: '{}'.", advance(&mut it))));
                    }
                }
            };

            match res {
                Ok(t) => tokens.push(build_token(t, line)),
                Err(e) => return Err(e)
            }
        }

        tokens.push(build_token(TokenType::EOF, line));
        Ok(tokens.clone())
    }
}

fn build_token(token_type: TokenType, line: u32) -> Token {
    Token::new(token_type, line)
}

fn identifier(it: &mut PeekableChar) -> Result<TokenType, Error> {
    let mut id = String::new();

    while is_alphanum(peek(it)) {
        id.push(advance(it));
    }

    let t = match id.as_ref() {
        "and" => TokenType::And,
        "cond" => TokenType::Cond,
        "define" => TokenType::Define,
        "else" => TokenType::Else,
        "false" => TokenType::Bool(false),
        "if" => TokenType::If,
        "not" => TokenType::Not,
        "or" => TokenType::Or,
        "true" => TokenType::Bool(true),
        _ => TokenType::Identifier(id)
    };

    Ok(t)
}

fn number(it: &mut PeekableChar) -> Result<TokenType, Error> {
    let mut number = String::new();

    while is_digit(peek(it)) {
        number.push(advance(it));
    }

    if it.peek() == Some(&'.') {
        number.push(advance(it)); // Consume '.'

        while is_digit(peek(it)) {
            number.push(advance(it));
        }
    }

    let number = number.parse::<f64>().unwrap();

    Ok(TokenType::Number(number))
}

fn paren(c: char, it: &mut PeekableChar) -> Result<TokenType, Error> {
    it.next();
    Ok(TokenType::Paren(c))
}

fn string(it: &mut PeekableChar) -> Result<TokenType, Error> {
    let mut string = String::new();

    // Consume '"'.
    it.next();

    while peek(it) != '"' && !is_at_end(it) {
        string.push(advance(it));
    }

    if is_at_end(it) {
        return Err(Error::Lexer(format!("Unterminated string.")));
    }

    // Consume '"'.
    it.next();

    Ok(TokenType::String(string))
}

fn is_at_end(it: &mut Peekable<Chars>) -> bool {
    peek(it) == '\0'
}

fn symbol(it: &mut PeekableChar) -> Result<TokenType, Error> {
    let c = advance(it);
    let mut op = c.to_string();

    match c {
        '>' | '<' => {
            if peek(it) == '=' {
                op.push(advance(it));
            }
        }
        _ => {}
    }

    Ok(TokenType::Identifier(op))
}

fn is_digit(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false
    }
}

fn is_alphanum(ch: char) -> bool {
    match ch {
        '0'..='9' | 'a'..='z' | 'A'..='Z' | '?' | '_' | '-' | '$' => true,
        _ => false
    }
}

fn peek(it: &mut PeekableChar) -> char {
    match it.peek() {
        Some(t) => *t,
        None => '\0'
    }
}

fn advance(it: &mut PeekableChar) -> char {
    match it.next() {
        Some(t) => t,
        None => '\0'
    }
}


#[cfg(test)]
mod tests {
    use hamcrest2::prelude::*;

    use super::*;

    #[test]
    fn lex_empty() {
        let lexer = Lexer::new();
        let tokens = lexer.lex("").unwrap();

        assert_that!(&tokens, len(1));
        assert_token(&tokens[0], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_invalid() {
        assert_invalid(",", "Invalid token: ','.");
        assert_invalid("\"", "Unterminated string.");
    }

    #[test]
    fn lex_booleans() {
        let tokens = lex("true false").unwrap();

        assert_that!(&tokens, len(3));
        assert_token(&tokens[0], &TokenType::Bool(true), 1);
        assert_token(&tokens[1], &TokenType::Bool(false), 1);
        assert_token(&tokens[2], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_keywords() {
        assert_tokens("and cond define else if not or", vec![
            TokenType::And,
            TokenType::Cond,
            TokenType::Define,
            TokenType::Else,
            TokenType::If,
            TokenType::Not,
            TokenType::Or,
        ]);
    }

    #[test]
    fn lex_numbers() {
        assert_tokens("123 4.56\n", vec![
            TokenType::Number(123.0),
            TokenType::Number(4.56),
        ]);
    }

    #[test]
    fn lex_strings() {
        assert_tokens("\"\" \"this is a string\"", vec![
            TokenType::String("".to_string()),
            TokenType::String("this is a string".to_string()),
        ]);
    }

    #[test]
    fn lex_identifiers() {
        assert_tokens("+ - * / plus_one", vec![
            TokenType::Identifier("+".to_string()),
            TokenType::Identifier("-".to_string()),
            TokenType::Identifier("*".to_string()),
            TokenType::Identifier("/".to_string()),
            TokenType::Identifier("plus_one".to_string()),
        ]);
    }

    #[test]
    fn lex_paren() {
        assert_tokens("()", vec![
            TokenType::Paren('('),
            TokenType::Paren(')'),
        ]);
    }

    fn assert_token(token: &Token, token_type: &TokenType, line: u32) {
        assert_that!(&token.token_type, equal_to(token_type));
        assert_that!(token.line, equal_to(line));
    }

    fn assert_invalid(source: &str, message: &str) {
        let error = lex(source).unwrap_err();

        assert_that!(error.to_string(), equal_to(message));
    }

    fn assert_tokens(source: &str, expected_tokens: Vec<TokenType>) {
        let tokens = lex(source).unwrap();

        assert_that!(tokens.len() - 1, equal_to(expected_tokens.len()));

        for (i, token) in tokens.iter().enumerate() {
            if token.token_type == TokenType::EOF {
                break;
            }
            assert_that!(&token.token_type, equal_to(&expected_tokens[i]));
        }
    }

    fn lex(source: &str) -> Result<Vec<Token>, Error> {
        let lexer = Lexer::new();
        lexer.lex(source)
    }
}