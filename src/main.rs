mod ast;
mod value;
mod token;
mod lexer;
mod parser;
mod interpreter;

mod repl;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;

fn main() {
    repl::start();
}