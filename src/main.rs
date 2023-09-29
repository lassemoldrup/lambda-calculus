use parser::Parser;
use std::env::args;
use std::fs;

mod lexer;
mod parser;

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Parse error: {0}")]
    Parse(#[from] parser::ParseError),
}

fn main() -> parser::Result<()> {
    let arg = args().nth(1).expect("file name should be provided");
    // let arg = "test.lc";
    let program = fs::read_to_string(arg).expect("failed to read file");
    let parsed = Parser::new(&program).parse()?;
    println!("Parsed input: {}\n", parsed);
    // println!("Output: {}", parsed.eval_normal_order());

    Ok(())
}
