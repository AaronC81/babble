#![feature(box_patterns)]
#![feature(never_type)]
#![feature(assert_matches)]
#![feature(result_flattening)]
#![feature(let_chains)]
#![feature(type_name_of_val)]
#![feature(trait_upcasting)]
#![feature(test)]

use std::{fs::read_to_string, io::{stdin, stdout, Write}};

use clap::Parser;
use interpreter::{Interpreter, InterpreterError, InterpreterErrorKind};
use parser::ParserError;
use source::{SourceFile, Location};
use tokenizer::{Tokenizer, TokenizerError};

use crate::interpreter::{instruction::compile, Value};

mod source;
mod tokenizer;
mod parser;
mod interpreter;
mod doc_gen;

#[cfg(test)]
mod bench;

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

    /// Rather than running any code, print compiled assembly for input
    #[arg(long)]
    show_asm: bool,
}

fn main() {
    let args = Args::parse();

    let input_contents;
    let input_name;
    let src;
    if let Some(file) = args.file {
        input_name = file;
        input_contents = read_to_string(input_name.clone()).unwrap();
        src = SourceFile::new(std::fs::canonicalize(input_name).unwrap().to_str().unwrap(), &input_contents).rc();
    } else if let Some(code) = args.code {
        input_name = "command-line".to_string();
        input_contents = code;
        src = SourceFile::new(&input_name, &input_contents).rc();
    } else if args.doc_gen {
        let interpreter = Interpreter::new(None).unwrap();
        let output = doc_gen::generate_html_documentation(&interpreter);
        println!("{output}");
        return;
    } else {
        repl();
    };

    if args.show_asm {
        let tokens = Tokenizer::tokenize(src.clone()).expect("tokenization failed");
        let node = crate::parser::Parser::parse_and_analyse(src, &tokens[..]).expect("parsing failed");
        let compiled = compile(node).expect("compilation failed");
        println!("{compiled}");
        return;
    }

    if let Err(e) = Interpreter::new(Some(src)).and_then(|mut i| i.parse_and_evaluate_root()) {
        print_interpreter_error(e);
    }
}

fn repl() -> ! {
    // Construct an interpreter instance
    let mut interpreter = Interpreter::new(None).unwrap();    

    let mut command_number = 1;
    loop {
        // Read a command
        print!("> ");
        stdout().flush().unwrap();
        let mut command = "".to_string();
        stdin().read_line(&mut command).unwrap();
        if command.ends_with('\n') {
            command.pop();
            if command.ends_with('\r') {
                command.pop();
            }
        }

        // Run it
        let source = SourceFile::new(&format!("repl-{command_number}"), &command);
        match interpreter.parse_and_evaluate(source.rc()) {
            Ok(result) => {
                interpreter.send_message(
                    Value::new_type(interpreter.resolve_stdlib_type("Console")).rc(),
                    "println:",
                    &[result],
                ).unwrap();
            },
            Err(e) => print_interpreter_error(e),
        }
        println!();

        command_number += 1;
    }
}

fn print_interpreter_error(e: InterpreterError) {
    if let InterpreterError { kind: InterpreterErrorKind::ParserError(e), .. } = e {
        print_parser_error(e);
        return;
    }
    if let InterpreterError { kind: InterpreterErrorKind::TokenizerError(e), .. } = e {
        print_tokenizer_error(e);
        return;
    }

    println!("Program error:\n  {}\n", e.kind);
    if let Some(details) = e.details {
        if let Some(location) = details.location {
            print_location(&location);
        }

        if let Some(backtrace) = details.backtrace {
            println!("Backtrace (most recent first):");
            for frame in backtrace.iter().rev() {
                println!("  - {}", frame.context);
            }
        }
    }
}

fn print_parser_error(e: ParserError) {
    println!("Parse error:\n  {}\n", e);
    print_location(e.location());
}

fn print_tokenizer_error(e: TokenizerError) {
    println!("Parse error:\n  {}\n", e); // Not *really* a parse error, but close enough for a user!
    print_location(e.location());
}

fn print_location(location: &Location) {
    let (line_number, position) = location.line_number_and_position();
    println!("At: {}:{}", location.source_file.name, line_number);
    println!("|   {}", location.line_contents().trim_end());
    println!("|   {}^\n", " ".repeat(position - 1));
}
