mod ast;
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
    repl::start();
}
