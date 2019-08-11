use std::io::{self, Write};

use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::Value;

pub fn start() {
    println!("Welcome to the lite repl!");

    let mut interpreter = Interpreter::new();
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let ast = match parser.parse_program() {
            Ok(ast) => ast,
            Err(err) => {
                println!("Parse error: {}", err);
                continue;
            }
        };

        match interpreter.evaluate_stmt(&ast) {
            Ok(value) => match value {
                Value::Void => (),
                _ => println!("{}", value),
            },
            Err(err) => println!("Evaluation error: {}", err),
        }
    }
}
