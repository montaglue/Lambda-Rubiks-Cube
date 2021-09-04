#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);

mod error;
mod interior_representation;
mod interpreter;
mod parse;
mod typechecker;
mod utils;

fn main() {
    println!("Hello, world!");
}
