use crate::interpreter::Interpreter;
use clap::Parser as ClapParser;
use std::{
    fs,
    io::{stdin, stdout, Write},
};

mod expr;
mod interpreter;
mod parser;
mod stmt;
mod tokenizer;
mod value;

#[derive(ClapParser)]
#[command(name = "lox-rs")]
#[command(version = "1.0")]
#[command(about = "Run a .lox file", long_about = None)]
struct Args {
    #[arg()]
    filepath: Option<String>,

    #[arg(short, long, default_value_t = 4)]
    tabsize: u8,
}

#[macro_export]
macro_rules! bad_print {
    ($($arg:tt)*) => {
        {
            eprintln!("{}{}{}{}{}", inline_colorization::style_bold, inline_colorization::color_red, format!($($arg)*), inline_colorization::color_reset, inline_colorization::style_reset)
        }
    };
}

fn run_file(interpreter: &mut Interpreter, filepath: &str, tabsize: usize) {
    let file_contents = match fs::read_to_string(filepath) {
        Ok(file_contents) => file_contents.trim().to_string(),
        Err(err) => {
            bad_print!("Failed to read file: {}", err);
            return;
        }
    };
    interpreter.run(&file_contents, tabsize);
}

fn run_prompt(interpreter: &mut Interpreter, tabsize: usize) {
    loop {
        print!("> ");
        if let Err(err) = stdout().flush() {
            bad_print!("Failed to flush stdout: {}", err)
        }
        let mut line = String::new();
        if let Err(err) = stdin().read_line(&mut line) {
            bad_print!("Failed to read line from stdin: {}", err)
        }
        let line = line.trim();
        if line.is_empty() {
            return;
        }
        interpreter.run(&line, tabsize);
    }
}

fn main() {
    let args = Args::parse();
    let mut interpreter = Interpreter::new();

    if let Some(filepath) = args.filepath {
        run_file(&mut interpreter, &filepath, args.tabsize.into())
    } else {
        run_prompt(&mut &mut interpreter, args.tabsize.into())
    }
}
