use std::iter::Peekable;

use crate::token::*;

#[derive(Debug)]
pub struct Lexer {
    //    tokens: Vec<Token>,
    line: u32,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
//            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn lex(&mut self, source: &str) -> Result<Vec<Token>, String> {
        let mut it = source.chars().peekable();
        let mut tokens = Vec::new();

        while let Some(&c) = it.peek() {
            let token_type = match c {
                '0'..='9' => {
                    number(&mut it)
                }
                '+' | '-' | '*' | '/' | '=' | '<' | '>' => {
                    symbol(&mut it)
                }
                '(' | ')' => {
                    it.next();
                    TokenType::Paren(c)
                }
                ' ' | '\t' => {
                    it.next();
                    continue;
                }
                '\n' => {
                    self.line += 1;
                    it.next();
                    continue;
                }
                _ => {
                    identifier(&mut it)
                }
            };

            tokens.push(self.build_token(token_type));
        }

        tokens.push(self.build_token(TokenType::EOF));
        Ok(tokens.clone())
    }

    fn build_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.line)
    }
}

fn identifier<T: Iterator<Item=char>>(it: &mut Peekable<T>) -> TokenType {
    let mut id = String::new();

    while is_alphanum(peek(it)) {
        id.push(advance(it));
    }

    match id.as_ref() {
        "true" => TokenType::Bool(true),
        "false" => TokenType::Bool(false),
        "define" => TokenType::Define,
        _ => TokenType::Identifier(id)
    }
}

fn number<T: Iterator<Item=char>>(it: &mut Peekable<T>) -> TokenType {
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

    TokenType::Number(number)
}

fn symbol<T: Iterator<Item=char>>(it: &mut Peekable<T>) -> TokenType {
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

    TokenType::Identifier(op)
}

fn is_digit(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false
    }
}

fn is_alphanum(ch: char) -> bool {
    match ch {
        '0'..='9' | 'a'..='z' | 'A'..='Z' | '?' | '_' | '$' => true,
        _ => false
    }
}

fn peek<T: Iterator<Item=char>>(it: &mut Peekable<T>) -> char {
    match it.peek() {
        Some(t) => *t,
        None => '\0'
    }
}

fn advance<T: Iterator<Item=char>>(it: &mut Peekable<T>) -> char {
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
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("").unwrap();

        assert_that!(&tokens, len(1));
        assert_token(&tokens[0], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_booleans() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("true false").unwrap();

        assert_that!(&tokens, len(3));
        assert_token(&tokens[0], &TokenType::Bool(true), 1);
        assert_token(&tokens[1], &TokenType::Bool(false), 1);
        assert_token(&tokens[2], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_keywords() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("define").unwrap();

        assert_that!(&tokens, len(2));
        assert_token(&tokens[0], &TokenType::Define, 1);
        assert_token(&tokens[1], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_numbers() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("123 4.56\n").unwrap();

        assert_that!(&tokens, len(3));
        assert_token(&tokens[0], &TokenType::Number(123.0), 1);
        assert_token(&tokens[1], &TokenType::Number(4.56), 1);
        assert_token(&tokens[2], &TokenType::EOF, 2);
    }

    #[test]
    fn lex_symbols() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("+ - * /").unwrap();

        assert_that!(&tokens, len(5));
        assert_token(&tokens[0], &TokenType::Identifier("+".to_string()), 1);
        assert_token(&tokens[1], &TokenType::Identifier("-".to_string()), 1);
        assert_token(&tokens[2], &TokenType::Identifier("*".to_string()), 1);
        assert_token(&tokens[3], &TokenType::Identifier("/".to_string()), 1);
        assert_token(&tokens[4], &TokenType::EOF, 1);
    }

    #[test]
    fn lex_paren() {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex("()").unwrap();

        assert_that!(&tokens, len(3));
        assert_token(&tokens[0], &TokenType::Paren('('), 1);
        assert_token(&tokens[1], &TokenType::Paren(')'), 1);
        assert_token(&tokens[2], &TokenType::EOF, 1);
    }

    fn assert_token(token: &Token, token_type: &TokenType, line: u32) {
        assert_that!(&token.token_type, equal_to(token_type));
        assert_that!(token.line, equal_to(line));
    }
}