#![feature(box_patterns)]
#![feature(let_else)]

use std::{env::args, fs::read_to_string};

use interpreter::Interpreter;
use parser::Parser;
use tokenizer::Tokenizer;

mod source;
mod tokenizer;
mod parser;
mod interpreter;

fn main() {
    let mut input = args().nth(1).expect("no code passed");
    if input == "--file" || input == "-f" {
        input = read_to_string(args().nth(2).expect("no file passed")).unwrap();
    }
    Interpreter::new().parse_and_evaluate(&input).expect("evaluation failed");
}
