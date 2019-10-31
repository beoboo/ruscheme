#[cfg(test)]
#[macro_use]
extern crate hamcrest2;

use std::{env, fs, io};
use std::io::Write;

use colored::*;

use crate::environment::Environment;
use crate::error::Error;
use crate::evaluator::*;
use crate::lexer::*;
use crate::parser::*;

mod environment;
mod error;
mod evaluator;
mod expr;
mod lexer;
mod parser;
mod token;

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

    let mut prompt = "> ";
    loop {
        print!("{}", prompt);
        io::stdout().flush().unwrap();

        let mut line = String::new();

        if io::stdin().read_line(&mut line).is_ok() {
            source += line.as_str();

            match run(&source, &mut globals) {
                Err(e) => {
                    match e {
                        Error::UnterminatedInput => {
                            println!();
                            prompt = "| ";
                        }
                        _ => {
                            eprintln!("{}", format!("{}", e).red());
                            source = String::new();
                            prompt = "> ";
                        },
                    }
                }
                _ => {
                    prompt = "> ";
                }
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

fn run(source: &str, globals: &mut Environment) -> Result<(), Error> {
    let lexer: Lexer = Lexer::new();

    let res = lexer.lex(source);
    let tokens;

    match res {
        Ok(t) => tokens = t,
        Err(e) => return Err(e)
    };

//    println!("{} token{}:", &tokens.len(), if tokens.len() != 1 { "s" } else { "" });
//    for token in &tokens {
//        println!("- {:?}", token);
//    }
//    println!();

    // Handle EOF.
    if tokens.len() == 1 {
        return Ok(());
    }

    let parser: Parser = Parser::new();
    let res = parser.parse(tokens);

    let expr = match res {
        Ok(expr) => expr,
        Err(e) => return Err(e)
    };
//
//    println!("{} expression{}:", &expressions.len(), if expressions.len() != 1 { "s" } else { "" });
//    for expr in &expressions {
//        println!("- {:?}", expr);
//    }
//    println!();

    let evaluator: Evaluator = Evaluator::new();

    match evaluator.evaluate(&expr, globals) {
        Ok(res) => println!("{}", res.to_string().green()),
        Err(e) => return Err(e)
    }

    Ok(())
}
