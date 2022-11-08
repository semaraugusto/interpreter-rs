use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Option<Token>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            cur_token,
            peek_token: Some(peek_token),
        }
    }
    fn next_token(&mut self) {
        match &self.peek_token {
            Some(peek) => {
                self.cur_token = peek.clone();
                self.peek_token = Some(self.lexer.next_token());
            }
            None => assert!(self.cur_token.token_type == TokenType::Eof),
        }
    }
    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.cur_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();
            if let Some(stmt) = statement {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }
    fn parse_statement(&mut self) -> Option<Box<impl Statement>> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<LetStatement>> {
        let let_token = self.cur_token.clone();
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }
        let ident = Identifier::new(self.cur_token.clone());

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Box::new(LetStatement {
            token: let_token,
            name: ident,
        }))
    }
    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            return true;
        }
        false
    }
    fn peek_token_is(&self, token_type: TokenType) -> bool {
        match &self.peek_token {
            Some(token) => token.clone().is_type(token_type),
            None => false,
        }
    }
    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.clone().is_type(token_type)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_let_statement() {
        println!("test_let_statement");
        let input = "let x = 5;
let y = 10;
let foobar = 838383;
";
        println!("input: {}", input);
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        println!("before");
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        let expected = ["X", "Y", "FOOBAR"];
        println!("before");

        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            let let_stmt: _ = stmt;
            println!("let_stmt: {:?}", let_stmt.token_literal());
            println!("let_stmt: {:?}", let_stmt.statement_node());
            let let_stmt = stmt.statement_node();
            assert_eq!(let_stmt[0], "LET");
            assert_eq!(let_stmt[1], expected[i]);
            // assert_eq!(let_stmt.name.value, expected[i]);
        }
    }
}
