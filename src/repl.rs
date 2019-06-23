use std::io::{self, Write};
use std::panic::{self, AssertUnwindSafe};

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

        let _ = panic::catch_unwind(AssertUnwindSafe(|| {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let value = interpreter.evaluate_stmt(&parser.parse_program());

            match value {
                Value::Void => (),
                _ => println!("{}", value),
            }
        }));
    }
}
