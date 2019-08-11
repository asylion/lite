use std::env;
use std::fs;

mod ast;
mod builtins;
mod environment;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod value;

mod repl;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() <= 1 {
        repl::start();
    } else {
        let file_name = &args[1];
        let input = fs::read_to_string(file_name).expect("Failed to read file");

        let mut interpreter = Interpreter::new();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        interpreter
            .evaluate_stmt(&parser.parse_program().unwrap())
            .unwrap();
    }
}
