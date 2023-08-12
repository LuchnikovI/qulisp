mod ast;
mod qulisp_code_snippets;
mod environment;

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);


fn main() {
    println!("Hello, world!");
}
