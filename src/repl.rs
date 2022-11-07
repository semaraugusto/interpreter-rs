use crate::lexer;
use crate::token::Token;
use rustyline::Editor;

pub fn start_repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::<()>::new()?;
    let readline = rl.readline(">> ");
    match readline {
        Ok(line) => {
            println!("Line: {:?}", line);
            let mut lexer = lexer::Lexer::new(line);
            loop {
                let token = lexer.next_token();
                if token == Token::Eof {
                    break;
                }
                println!("Token: {:?}", token);
            }
        }
        Err(_) => println!("No input"),
    }
    Ok(())
}
