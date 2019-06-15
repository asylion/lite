mod ast;
mod value;
mod token;
mod lexer;
mod parser;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;

fn main() {
    let lexer = Lexer::new("5 + 123 * 456");
    let mut parser = Parser::new(lexer);
    let interpreter = Interpreter;
    let result = interpreter.evaluate_program(parser.parse_program());
    println!("{:?}", result);
}