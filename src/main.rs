#![feature(box_patterns)]
#![feature(never_type)]
#![feature(assert_matches)]

use std::{env::args, fs::read_to_string};

use interpreter::Interpreter;
use source::SourceFile;

mod source;
mod tokenizer;
mod parser;
mod interpreter;

fn main() {
    let arg = args().nth(1).expect("no code passed");
    let input_contents;
    let input_name;
    if arg == "--file" || arg == "-f" {
        input_name = args().nth(2).expect("no file passed");
        input_contents = read_to_string(input_name.clone()).unwrap();
    } else {
        input_name = "command-line".to_string();
        input_contents = arg.clone();
    }
    let src = SourceFile::new(&input_name, &input_contents).rc();
    if let Err(e) = Interpreter::new().parse_and_evaluate(src) {
        println!("Fatal error:\n  {}\n", e.kind);
        if let Some(details) = e.details {
            if let Some(location) = details.location {
                println!("At: {}:{}", location.source_file.name, location.line_number());
                println!("|   {}\n", location.line_contents().trim_end());
            }

            println!("Backtrace (most recent first):");
            for frame in details.backtrace.iter().rev() {
                println!("  - {}", frame.context);
            }
        }
    }
}
