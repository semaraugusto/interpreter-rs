use crate::ast::*;
use crate::errors::*;
use crate::lexer::*;
use crate::token::*;
use std::error::Error;

// enum ParserStatus {
//     Success,
//     Failure,
// }

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            cur_token,
            peek_token: Some(peek_token),
            errors: vec![],
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
    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.token_type {
            TokenType::Let => {
                let stmt = match self.parse_let_statement() {
                    Some(stmt) => stmt,
                    None => return None,
                };
                Some(Box::new(stmt))
            }
            TokenType::Return => {
                let stmt = match self.parse_return_statement() {
                    Some(stmt) => stmt,
                    None => return None,
                };

                Some(Box::new(stmt))
            }
            _ => None,
        }
    }
    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let return_token = self.cur_token.clone();

        self.next_token();
        if self.cur_token_is(TokenType::Int) {
            let expr = self.parse_integer_literal();
            return Some(ReturnStatement {
                token: return_token,
                expr: Some(expr),
            });
        }
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }
        Some(ReturnStatement {
            token: return_token,
            expr: None,
        })
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let let_token = self.cur_token.clone();
        if let Err(err) = self.expect_peek(TokenType::Ident) {
            self.errors.push(err.to_string());
            return None;
        }
        let ident = Identifier::new(self.cur_token.clone());

        if let Err(err) = self.expect_peek(TokenType::Assign) {
            self.errors.push(err.to_string());
            return None;
        }

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(LetStatement {
            token: let_token,
            name: ident,
            value: None,
        })
    }
    fn parse_expression(&mut self) -> Box<dyn Expression> {
        if self.cur_token_is(TokenType::Int) {
            if self.peek_token_is(TokenType::Plus) {
                return self.parse_operator_expression();
            } else if self.peek_token_is(TokenType::Semicolon) {
                return self.parse_integer_literal();
            } else {
                unimplemented!("Return errors")
            }
        } else if self.cur_token_is(TokenType::LParen) {
            unimplemented!("Parse group expression")
        } else {
            unimplemented!("Return errors")
        }
    }
    fn parse_operator_expression(&mut self) -> Box<dyn Expression> {
        unimplemented!()
    }
    fn parse_integer_literal(&mut self) -> Box<dyn Expression> {
        Box::new(IntegerLiteral::new(self.cur_token.clone()))
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Result<bool, Box<dyn Error>> {
        if self.peek_token_is(token_type) {
            self.next_token();
            return Ok(true);
        }
        let actual = self.peek_token.clone().expect("Could not peek next token");
        let err = WrongTokenError::new(actual, token_type);
        Err(Box::new(err))
        // false
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
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        println!("before");
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        let expected = [
            "LET X = NOT_IMPLEMENTED",
            "LET Y = NOT_IMPLEMENTED",
            "LET FOOBAR = NOT_IMPLEMENTED",
        ];
        println!("before");

        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            println!("let_stmt: {:?}", stmt.token_literal());
            assert_eq!(stmt.token_literal(), "LET");
            assert_eq!(stmt.statement_node(), expected[i]);
        }
        // println!("let_stmt: {:?}", let_stmt.statement_node());
        // let let_stmt = stmt.statement_node();
        // assert_eq!(stmt[1], expected[i]);
        // assert_eq!(let_stmt.name.value, expected[i]);
    }

    #[test]
    fn test_let_statement_error() {
        let input = "let x 5;
let = 10;
let 838383;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_program();
        // println!("input: {:?}", program.statements);
        let mut errors_iter = parser.errors.iter();
        assert_eq!(
            *errors_iter.next().unwrap(),
            "Expected next token to be Assign, got 'Token(Int, 5)' instead".to_string()
        );
        assert_eq!(
            *errors_iter.next().unwrap(),
            "Expected next token to be Ident, got 'Token(Assign, =)' instead".to_string()
        );
        assert_eq!(
            *errors_iter.next().unwrap(),
            "Expected next token to be Ident, got 'Token(Int, 838383)' instead".to_string()
        );
    }

    #[test]
    fn test_return_statement() {
        println!("test_return_statement");
        let input = "return 5;
return 10;
return 993322;
";
        println!("input: {}", input);
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        if program.statements.len() != 3 {
            panic!("Invalid number of statements");
        }
        let expected = ["RETURN 5", "RETURN 10", "RETURN 993322"];

        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            println!("token: {:?}", stmt.token_literal());
            println!("stmt: {:?}", stmt.statement_node());
            assert_eq!(stmt.statement_node(), expected[i]);
        }
        // println!("let_stmt: {:?}", let_stmt.statement_node());
        // let let_stmt = stmt.statement_node();
        // assert_eq!(stmt[1], expected[i]);
        // assert_eq!(stmt.name.value, expected[i]);
    }
}
