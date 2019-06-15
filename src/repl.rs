use std::io::{self, Write};

use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub fn start() {
    println!("Welcome to the lite repl!");
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let interpreter = Interpreter;

        let value = interpreter.evaluate_program(parser.parse_program());

        println!("{}", value);
    }
}
