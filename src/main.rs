use std::env::args;
use std::fs::File;
use std::io::Read;
use crate::interpreter::Strategy;

mod lexer;
mod parser;
mod interpreter;

fn main() -> Result<(), parser::ParseError> {
    let mut file = String::new();
    File::open(args().nth(1).unwrap()).unwrap().read_to_string(&mut file).unwrap();

    println!("{}", parser::parse(&lexer::tokenize(&file))?
        .eval(Strategy::NormalOrder));

    Ok(())
}
