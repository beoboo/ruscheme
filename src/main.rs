#[cfg(test)]
#[macro_use]
extern crate hamcrest2;

use std::{env, fs, io};
use std::io::{stdin, stdout, Write};
use std::io::prelude::*;

use colored::*;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

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
//    let stdout = stdout();
//    let mut stdout = stdout.lock().into_raw_mode().unwrap();
//    let stdin = stdin();
//    let stdin = stdin.lock();
//    let mut bytes = stdin.bytes();
//    loop {
//        let b = bytes.next().unwrap().unwrap();
//
//        match b {
//            // Quit
//            b'q' => return,
//            // Write it to stdout.
//            a => write!(stdout, "{:?}", String::from_utf8(vec![a]).unwrap()).unwrap(),
//        };
//
//        stdout.flush().unwrap();
//    }
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
                    eprintln!("{}", format!("{}", e).red());

                    match e {
                        Error::Parser(_) => {}
                        _ => source = String::new(),
                    }
                }
                _ => {}
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
