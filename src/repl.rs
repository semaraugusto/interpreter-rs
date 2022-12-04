use crate::lexer;
use crate::parser;
use rustyline::Editor;

const MONKEY_FACE: &str = "            __,__
   .--.  .-\"     \"-.  .--.
  / .. \\/  .-. .-.  \\/ .. \\
 | |  '|  /   Y   \\  |'  | |
 | \\   \\  \\ 0 | 0 /  /   / |
  \\ '- ,\\.-\"\"\"\"\"\"\"-./, -' /
   ''-' /_   ^ ^   _\\ '-''
       |  \\._   _./  |
       \\   \\ '~' /   /
        '._ '-=-' _.'
           '-----'
";

pub fn start_repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::<()>::new()?;
    println!("{}", MONKEY_FACE);
    while let Ok(line) = rl.readline(">> ") {
        println!("Line: {:?}", line);
        let lexer = lexer::Lexer::new(line);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            println!("{}", MONKEY_FACE);
            println!("Woops! We ran into some monkey business here!");
            println!(" parser errors:");
            for err in parser.errors.iter() {
                println!(" {}", err);
            }
            return Ok(());
        }
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("stmt {}: {}", i, stmt.to_string());
        }
    }
    Ok(())
}
