#![feature(box_patterns)]

use std::env::args;

use interpreter::Interpreter;
use parser::Parser;
use tokenizer::Tokenizer;

mod source;
mod tokenizer;
mod parser;
mod interpreter;

fn main() {
    let input = args().nth(1).expect("no code passed");
    let tokens = Tokenizer::tokenize(&input).expect("tokenization failed");
    let node = Parser::parse(&tokens[..]).expect("parsing failed");
    Interpreter::new().evaluate(&node).expect("evaluation failed");
}
