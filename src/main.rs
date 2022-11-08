pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;
#[warn(dead_code)]

fn main() {
    println!("Hello, This is the Monkey programming language!\n");
    println!("Feel free to type in commands");
    repl::start_repl().unwrap()
}
