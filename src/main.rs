extern crate clap;
#[cfg(test)]
#[macro_use]
extern crate hamcrest2;

use std::{fs, io};
use std::io::Write;

use clap::{App, Arg};
use colored::*;

use crate::compiler::*;
use crate::desugarizer::*;
use crate::environment::*;
use crate::error::*;
use crate::evaluator::*;
use crate::lexer::*;
use crate::parser::*;
//use crate::printer::*;
use crate::virtual_machine::*;

mod byte_code;
mod compiler;
mod desugarizer;
mod environment;
mod error;
mod evaluator;
mod expr;
mod lexer;
mod parser;
mod printer;
mod token;
mod virtual_machine;


fn main() {
    env_logger::init();

    let matches = App::new("ruscheme")
        .arg(Arg::with_name("filename")
            .short("f")
            .long("filename")
            .takes_value(true)
            .help("File to run")
        )
        .arg(
            Arg::with_name("use-compiler")
                .long("use-compiler")
                .help("Use compiler instead of interpreter")
        ).get_matches();

    let use_compiler = matches.is_present("use-compiler");

    if let Some(filename) = matches.value_of("filename") {
        run_file(filename, use_compiler)
    } else {
        repl(use_compiler)
    }
}

fn repl(use_compiler: bool) {
    let mut globals = Environment::global();
    let mut source = String::new();

    let mut prompt = "> ";
    loop {
        print!("{}", prompt);
        io::stdout().flush().unwrap();

        let mut line = String::new();

        if io::stdin().read_line(&mut line).is_ok() {
            source += line.as_str();

            match run(&source, &mut globals, use_compiler, true) {
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
                        }
                    }
                }
                _ => {
                    source = String::new();
                    prompt = "> ";
                }
            };
        }
    }
}

fn run_file(filename: &str, use_compiler: bool) {
    let mut globals = Environment::global();

    let source: String = fs::read_to_string(filename)
        .expect(format!("Cannot read {}", filename).as_str());

    match run(&source, &mut globals, use_compiler, false) {
        Err(e) => eprintln!("{}", format!("{}", e).red()),
        _ => (),
    };
}

fn run(source: &str, globals: &mut Environment, use_compiler: bool, print_output: bool) -> Result<(), Error> {
    if use_compiler {
        run_compiler(&source, print_output)
    } else {
        run_interpreter(&source, globals, print_output)
    }
}

fn run_interpreter(source: &str, globals: &mut Environment, print_output: bool) -> Result<(), Error> {
    let lexer = Lexer::new();
    let tokens = lexer.lex(source)?;

//    println!("{} token{}:", &tokens.len(), if tokens.len() != 1 { "s" } else { "" });
//    for token in &tokens {
//        println!("- {:?}", token);
//    }
//    println!();
    let desugarizer = Desugarizer::new();
    let tokens = desugarizer.desugar(tokens)?;

    // Handle EOF.
    if tokens.len() == 1 {
        return Ok(());
    }

    let parser = Parser::new();
    let exprs = parser.parse(tokens)?;

//
//    println!("{} expression{}:", &expressions.len(), if expressions.len() != 1 { "s" } else { "" });
//    for expr in &expressions {
//        println!("- {:?}", expr);
//    }
//    println!();

    let evaluator: Evaluator = Evaluator::new();

    for expr in exprs {
        match evaluator.evaluate(&expr, globals) {
            Ok(res) => {
                if print_output {
                    println!("{}", res.to_string().green());
                }
            }
            Err(e) => return Err(e)
        }
    }

    Ok(())
}

fn run_compiler(source: &str, print_output: bool) -> Result<(), Error> {
    let lexer: Lexer = Lexer::new();
    let tokens = lexer.lex(source)?;

//    println!("{} token{}:", &tokens.len(), if tokens.len() != 1 { "s" } else { "" });
//    for token in &tokens {
//        println!("- {:?}", token);
//    }
//    println!();

    // Handle EOF.
    if tokens.len() == 1 {
        return Ok(());
    }

    let compiler: Compiler = Compiler::new();
    let instructions = compiler.compile(tokens)?;

//
//    println!("{} expression{}:", &expressions.len(), if expressions.len() != 1 { "s" } else { "" });
//    for expr in &expressions {
//        println!("- {:?}", expr);
//    }
//    println!();

    let vm: VirtualMachine = VirtualMachine::new();

    match vm.execute(instructions) {
        Ok(res) => {
            if print_output {
                for byte_code in res {
                    println!("{}", format!("{}", byte_code).green());
                }
            }
        },
        Err(e) => return Err(e),
    }

    Ok(())
}
