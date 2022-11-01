#![feature(box_patterns)]
#![feature(never_type)]
#![feature(assert_matches)]
#![feature(result_flattening)]
#![feature(let_chains)]

use std::{env::args, fs::read_to_string};

use clap::Parser;
use interpreter::Interpreter;
use source::SourceFile;

mod source;
mod tokenizer;
mod parser;
mod interpreter;
mod doc_gen;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
   /// Path to a script to run
   #[arg(short = 'f', long, conflicts_with = "code")]
   file: Option<String>,

   /// Raw code to run
   #[arg(short = 'e', long, conflicts_with = "file")]
   code: Option<String>,

   /// Rather than running any code, print Markdown stdlib documentation
    #[arg(long, conflicts_with = "code", conflicts_with = "file")]
    doc_gen: bool,
}

fn main() {
    let args = Args::parse();

    let input_contents;
    let input_name;
    if let Some(file) = args.file {
        input_name = file.clone();
        input_contents = read_to_string(input_name.clone()).unwrap();
    } else if let Some(code) = args.code {
        input_name = "command-line".to_string();
        input_contents = code.clone();
    } else if args.doc_gen {
        let interpreter = Interpreter::new().unwrap();
        let output = doc_gen::generate_documentation(&interpreter);
        println!("{}", output);
        return;
    } else {
        unreachable!("invalid command")
    };

    let src = SourceFile::new(&input_name, &input_contents).rc();
    if let Err(e) = Interpreter::new().map(|mut i| i.parse_and_evaluate(src)).flatten() {
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
