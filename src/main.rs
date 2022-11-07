pub mod ast;
pub mod lexer;
pub mod repl;
pub mod token;

fn main() {
    println!("Hello, This is the Monkey programming language!\n");
    println!("Feel free to type in commands");
    repl::start_repl().unwrap()
}
