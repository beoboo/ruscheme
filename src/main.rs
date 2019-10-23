#[cfg(test)]
#[macro_use]
extern crate hamcrest2;

use std::{env, fs, io, io::Write};

use crate::evaluator::*;
use crate::lexer::*;
use crate::parser::*;

mod lexer;
mod parser;
mod token;
mod expr;
mod evaluator;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1])
    }
}

fn run(source: &str) {
    let mut lexer: Lexer = Lexer::new();
    let parser: Parser = Parser::new();
    let evaluator: Evaluator = Evaluator::new();

    let res = lexer.lex(source);
    let tokens;

    match res {
        Ok(t) => tokens = t,
        Err(e) => {
            println!("Lexing error: {}", e);
            return;
        }
    };

    println!("{} token{}:", &tokens.len(), if tokens.len() != 1 { "s" } else { "" });
    for token in &tokens {
        println!("- {:?}", token);
    }

    println!();
    let res = parser.parse(tokens);
    let expressions;

    match res {
        Ok(exprs) => expressions = exprs,
        Err(e) => {
            println!("Parsing error: {}", e);
            return;
        }
    };

    println!("{} expression{}:", &expressions.len(), if expressions.len() != 1 { "s" } else { "" });
    for expr in expressions {
        println!("- {:?}", expr);
        println!();
        println!("Result: {}", evaluator.evaluate(expr).unwrap());
    }
}

fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();

        if io::stdin().read_line(&mut line).is_ok() {
            run(&line);
        };
    }
}

fn run_file(filename: &String) {
    let contents: String = fs::read_to_string(filename)
        .expect(format!("Cannot read {}", filename).as_str());

    run(&contents);
}
