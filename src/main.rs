use std::env::args;
use std::fs::File;
use std::io::Read;
use parser::parse;
use lexer::tokenize;

mod lexer;
mod parser;
mod interpreter;

fn main() -> parser::Result<()> {
    let mut file = String::new();
    File::open(args().nth(1).unwrap()).unwrap().read_to_string(&mut file).unwrap();

    let parsed = parse(&tokenize(&file))?;
    println!("Parsed input: {}\n", parsed);
    println!("Output: {}", parsed.eval_normal_order());

    Ok(())
}
