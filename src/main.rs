mod lexer;

use lexer::*;

fn main() {
    let mut lexer = Lexer::new();
    let tokens = lexer.lex("123\n456\n".to_string()).unwrap();

    println!("Hello, {} tokens!", &lexer.tokens.len());
    for token in &tokens {
        println!("Token: {:?}", token);
    }
}

#[cfg(test)] #[macro_use] extern crate hamcrest2;