use std::io::{self, Write};
use std::panic::{self, AssertUnwindSafe};

use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub fn start() {
    println!("Welcome to the lite repl!");

    let mut env = Environment::new();
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let _ = panic::catch_unwind(AssertUnwindSafe(|| {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let interpreter = Interpreter;

            let value = interpreter.evaluate_stmt(parser.parse_program(), &mut env);

            println!("{}", value);
        }));
    }
}
