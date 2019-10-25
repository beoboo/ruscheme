#[cfg(test)]
#[macro_use]
extern crate hamcrest2;

use std::{env, fs, io};
use std::io::prelude::*;

use colored::*;

use crate::environment::Environment;
use crate::evaluator::*;
use crate::lexer::*;
use crate::parser::*;

mod lexer;
mod parser;
mod token;
mod expr;
mod evaluator;
mod environment;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1])
    }
}

fn repl() {
    let mut globals = Environment::global();
    let mut source = String::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();

        if io::stdin().read_line(&mut line).is_ok() {
            source += line.as_str();
            match run(&source, &mut globals) {
                Err(e) => {
//                    eprintln!("{}", format!("{}", e).red())
                }
                _ => source = String::new(),
            };
        }
    }
}

fn run_file(filename: &String) {
    let mut globals = Environment::global();

    let source: String = fs::read_to_string(filename)
        .expect(format!("Cannot read {}", filename).as_str());

    match run(&source, &mut globals) {
        Err(e) => eprintln!("{}", format!("{}", e).red()),
        _ => (),
    };
}

fn run(source: &str, globals: &mut Environment) -> Result<(), String> {
    let lexer: Lexer = Lexer::new();

    let res = lexer.lex(source);
    let tokens;

    match res {
        Ok(t) => tokens = t,
        Err(e) => {
            return Err(format!("Lexing error: {}", e));
        }
    };

//    println!("{} token{}:", &tokens.len(), if tokens.len() != 1 { "s" } else { "" });
//    for token in &tokens {
//        println!("- {:?}", token);
//    }
//    println!();

    let parser: Parser = Parser::new();
    let res = parser.parse(tokens);
    let expressions;

    match res {
        Ok(exprs) => expressions = exprs,
        Err(e) => {
            return Err(format!("Parsing error: {}", e));
        }
    };
//
//    println!("{} expression{}:", &expressions.len(), if expressions.len() != 1 { "s" } else { "" });
//    for expr in &expressions {
//        println!("- {:?}", expr);
//    }
//    println!();

    let evaluator: Evaluator = Evaluator::new();

    for expr in expressions {
        match evaluator.evaluate(&expr, globals) {
            Ok(res) => println!("{}", res.to_string().green()),
            Err(e) => return Err(format!("Evaluating error: {}", e))
        }
    }

    Ok(())
}
