use crate::ast::*;
use crate::errors::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;
use std::error::Error;

// enum ParserStatus {
//     Success,
//     Failure,
// }

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
type InfixParseFn = fn(&mut Parser, Box<dyn Expression>) -> Option<Box<dyn Expression>>;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Option<Token>,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}
fn match_precedence(token_type: TokenType) -> Precedence {
    match token_type {
        TokenType::Eq => Precedence::Equals,
        TokenType::NotEq => Precedence::Equals,
        TokenType::LT => Precedence::LessGreater,
        TokenType::GT => Precedence::LessGreater,
        TokenType::Plus => Precedence::Sum,
        TokenType::Minus => Precedence::Sum,
        TokenType::Slash => Precedence::Product,
        TokenType::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

fn parse_infix_expression(
    parser: &mut Parser,
    left: Box<dyn Expression>,
) -> Option<Box<dyn Expression>> {
    println!("parse_INFIX_expression");
    let token = parser.cur_token.clone();
    println!("token: {:?}", token);
    let precedence = parser.cur_precedence();
    println!("precedence: {:?}", precedence);
    parser.next_token();
    let expr = parser.parse_expression(precedence);
    if let Some(expr) = &expr {
        println!("expr: {:?}", expr.to_string());
    }
    Some(Box::new(InfixExpression::new(token, Some(left), expr)))
}

impl Parser {
    fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        prefix_parse_fns.insert(TokenType::Ident, Parser::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Parser::parse_integer_literal);
        prefix_parse_fns.insert(TokenType::Bang, Parser::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::Minus, Parser::parse_prefix_expression);

        let mut infix_parse_fns: HashMap<TokenType, InfixParseFn> = HashMap::new();
        infix_parse_fns.insert(TokenType::Plus, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Minus, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Slash, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Asterisk, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Eq, parse_infix_expression);
        infix_parse_fns.insert(TokenType::NotEq, parse_infix_expression);
        infix_parse_fns.insert(TokenType::GT, parse_infix_expression);
        infix_parse_fns.insert(TokenType::LT, parse_infix_expression);

        Self {
            lexer,
            cur_token,
            peek_token: Some(peek_token),
            errors: vec![],
            prefix_parse_fns,
            infix_parse_fns,
        }
        // parser.register_prefix(TokenType::Ident, parser.parse_identifier);
    }
    fn register_prefix(&mut self, token_type: TokenType, prefix_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, prefix_fn);
    }
    fn register_infix(&mut self, token_type: TokenType, infix_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, infix_fn);
    }
    fn next_token(&mut self) {
        println!("NEXT_TOKEN");
        println!("next_token");
        println!("cur_token: {:?}", self.cur_token);
        println!("peek_token: {:?}", self.peek_token);
        match &self.peek_token {
            Some(peek) => {
                self.cur_token = peek.clone();
                self.peek_token = Some(self.lexer.next_token());
                println!("NEW cur_token: {:?}", self.cur_token);
                println!("NEW peek_token: {:?}", self.peek_token);
            }
            None => assert!(self.cur_token.token_type == TokenType::Eof),
        }
    }
    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        println!("parse_program");
        while self.cur_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();
            if let Some(stmt) = statement {
                println!("parsed_statement {}", stmt.to_string());
                program.statements.push(stmt);
            } else {
                println!("Could not parse statement");
            }

            self.next_token();
        }
        program
    }
    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        println!("parse_statement cur_token: {:?}", self.cur_token);
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
            TokenType::Int | TokenType::Ident | TokenType::Bang | TokenType::Minus => {
                let stmt = match self.parse_expression_statement() {
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
        let expr = self.parse_expression(Precedence::Lowest);

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }
        Some(ReturnStatement {
            token: return_token,
            expr,
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
    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        println!("parse_expression_statement");
        let token = self.cur_token.clone();
        println!("token {}", token.to_string());
        let expr = self.parse_expression(Precedence::Lowest);
        if let Some(e) = expr.as_ref() {
            println!("expr {}", e.to_string());
        }
        self.next_token();
        return Some(ExpressionStatement { token, expr });
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix: Option<&PrefixParseFn> = self.prefix_parse_fns.get(&self.cur_token.token_type);
        let token = self.cur_token.clone();
        println!("parse_expression {:?}", token);
        println!("CUR_TOKEN INIT {:?}", self.cur_token);
        match prefix {
            Some(prefix) => {
                let mut left = prefix(self);
                if let Some(l) = left.as_ref() {
                    println!("prefix run left {:?}", l.to_string());
                    // println!("left {}", l.to_string());
                } else {
                    println!("prefix run left = NONE");
                }
                println!("CUR_TOKEN PREFIX {:?}", self.cur_token);

                // println!(
                //     "tokentype: {token_type} left {:?}",
                //     left.as_ref().unwrap().to_string()
                // );
                let peek_precedence = self.peek_precedence().unwrap();
                let comparison = precedence < self.peek_precedence().unwrap();
                println!(
                    "precedence {:?} peek_precedence {:?} comparison: {}",
                    precedence, peek_precedence, comparison
                );
                while !(self.peek_token_is(TokenType::Semicolon)
                    | self.peek_token_is(TokenType::Eof))
                    && comparison
                {
                    // if left.is_none() {
                    //     return None;
                    // }
                    // let peek_token = self.peek_token.clone().unwrap();
                    // println!("peek_token1111 : {peek_token:?}");
                    // let infix = p.infix_parse_fns[p.peek_token.Type]
                    let infixes = self.infix_parse_fns.clone();
                    // println!("infixes : {:?}", infixes.keys());

                    println!("peek_token1111: {:?}", self.peek_token.clone());
                    let infix = infixes
                        .get(&self.peek_token.clone().unwrap().token_type)
                        .clone();
                    println!("infix {:?}", infix.is_none());
                    println!("LEFT: {:?}", left.as_ref().unwrap().to_string());
                    println!("CUR_TOKEN LOOP {:?}", self.cur_token);
                    if infix.is_none() {
                        return left;
                    }
                    println!("CUR_TOKEN LOOP {:?}", self.cur_token);
                    self.next_token();
                    println!("CUR_TOKEN AFTER NEXT {:?}", self.cur_token);
                    let infix = infix.unwrap();
                    left = infix(self, left.unwrap());
                    println!("AFTER: {:?}", left.as_ref().unwrap().to_string());
                }
                Some(left.unwrap())
            }
            None => {
                let msg = format!(
                    "Could not find prefix_parse_fn for {:?}. Available keys are {:?}",
                    &self.cur_token.token_type,
                    self.prefix_parse_fns.keys()
                );
                self.errors.push(msg.clone());
                panic!("msg: {}", msg.as_str());
            }
        }
    }
    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        print!("parse_identifier");
        let token = self.cur_token.clone();
        print!("token: {:?}", token);
        // if let Err(err) = self.expect_peek(TokenType::Semicolon) {
        //     self.errors.push(err.to_string());
        //     return None;
        // }
        // self.next_token();
        Some(Box::new(Identifier::new(token)))
    }
    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(IntegerLiteral::new(self.cur_token.clone())))
    }
    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        println!("parse_prefix_expression cur_token {:?}", self.cur_token);
        let token = self.cur_token.clone();
        self.next_token();
        let expr = self.parse_expression(Precedence::Prefix);
        println!("new cur_token {:?}", self.cur_token);
        Some(Box::new(PrefixExpression::new(token, expr)))
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
    fn peek_precedence(&mut self) -> Option<Precedence> {
        self.peek_token
            .clone()
            .map(|token| match_precedence(token.token_type))
        // match &self.peek_token {
        //     Some(peek_token) => Some(match_precedence(peek_token.token_type)),
        //     None => None,
        // }
    }
    fn cur_precedence(&mut self) -> Precedence {
        match_precedence(self.cur_token.token_type)
    }
    fn peek_token_is(&mut self, token_type: TokenType) -> bool {
        match &self.peek_token {
            Some(token) => token.clone().is_type(token_type),
            None => false,
        }
    }
    fn cur_token_is(&mut self, token_type: TokenType) -> bool {
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
        assert_eq!(parser.errors.len(), 0);
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
        assert_eq!(parser.errors.len(), 3);
        assert_eq!(
            *errors_iter.next().unwrap(),
            "Expected next token to be Assign, got '5' instead".to_string()
        );
        assert_eq!(
            *errors_iter.next().unwrap(),
            "Expected next token to be Ident, got '=' instead".to_string()
        );
        assert_eq!(
            *errors_iter.next().unwrap(),
            "Expected next token to be Ident, got '838383' instead".to_string()
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
        assert_eq!(parser.errors.len(), 0);
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
    }
    #[test]
    fn test_identifier_expression() {
        println!("test_identity_expression");
        let input = "foobar;";
        println!("input: {}", input);
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        println!("errors: {:?}", parser.errors);
        assert_eq!(parser.errors.len(), 0);
        if program.statements.len() != 1 {
            panic!("Invalid number of statements");
        }
        let expected = ["FOOBAR"];

        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            println!("token: {:?}", stmt.token_literal());
            println!("stmt: {:?}", stmt.statement_node());
            assert_eq!(stmt.statement_node(), expected[i]);
        }
    }
    #[test]
    fn test_integer_literal_expression() {
        println!("test_integer_literal_expression");
        let input = "5;";
        println!("input: {}", input);
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        println!("errors: {:?}", parser.errors);
        assert_eq!(parser.errors.len(), 0);
        if program.statements.len() != 1 {
            panic!("Invalid number of statements");
        }
        let expected = ["5"];

        println!("START TEST");
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            println!("token: {:?}", stmt.token_literal());
            println!("stmt: {:?}", stmt.statement_node());
            assert_eq!(stmt.statement_node(), expected[i]);
        }
    }
    #[test]
    fn test_prefix_expressions() {
        println!("test_prefix_expressions");
        let inputs = ["!5;", "-15;", "-a;"];
        println!("input: {:?}", inputs);
        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            // println!("input: {:?}", program.statements);
            println!("errors: {:?}", parser.errors);
            assert_eq!(parser.errors.len(), 0);
            if program.statements.len() != 1 {
                panic!("Invalid number of statements");
            }
            let expected = ["(! 5)", "(- 15)", "(- A)"];

            println!("START TEST");
            for (j, stmt) in program.statements.iter().enumerate() {
                println!("j: {}", j);
                println!("token: {:?}", stmt.token_literal());
                println!("stmt: {:?}", stmt.statement_node());
                assert_eq!(stmt.statement_node(), expected[i]);
            }
        }
    }
    #[test]
    fn test_infix_expressions() {
        println!("test_prefix_expressions");
        println!("test_infix_expressions");
        let inputs = [
            "1 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 > 5;", "5 < 5;", "5 == 5;", "5 != 5;",
        ];

        let expected = [
            "(1 + 5)", "(5 - 5)", "(5 * 5)", "(5 / 5)", "(5 > 5)", "(5 < 5)", "(5 == 5)",
            "(5 != 5)",
        ];
        println!("input: {:?}", inputs);
        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            // println!("input: {:?}", program.statements);
            println!("errors: {:?}", parser.errors);
            assert_eq!(parser.errors.len(), 0);
            // if program.statements.len() != 1 {
            //     panic!("Invalid number of statements");
            // }
            println!("START TEST");
            for (j, stmt) in program.statements.iter().enumerate() {
                println!("j: {}", j);
                println!("token: {:?}", stmt.token_literal());
                println!("stmt: {:?}", stmt.statement_node());
                assert_eq!(stmt.statement_node(), expected[i]);
            }
        }
    }
    #[test]
    // #[ignore]
    fn test_operator_precedence_partins() {
        println!("test_operator_precedence_partins");
        let inputs = [
            "-3 * 5",
            "-a * b;",
            "!-a;",
            "a + b + c;",
            "a + b - c;",
            "a * b * c;",
            "a * b / c;",
            "a + b / c;",
            "a + b * c + d / e - f;",
            "3 + 4; -5 * 5;",
            "5 > 4 == 3 < 4;",
            "5 < 4 != 3 > 4;",
            "3 + 4 * 5 == 3 * 1 + 4 * 5;",
        ];

        let expected = [
            "((- 3) * 5)",
            "((-A) * B)",
            "(!(-a))",
            "((a + b) + c)",
            "((a + b) - c)",
            "((a * b) * c)",
            "((a * b) / c)",
            "(a + (b / c))",
            "(((a + (b * c)) + (d / e)) - f)",
            "(3 + 4)((-5) * 5)",
            "((5 > 4) == (3 < 4))",
            "((5 < 4) != (3 > 4))",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ];
        println!("input: {:?}", inputs);
        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            // println!("input: {:?}", program.statements);
            println!("errors: {:?}", parser.errors);
            assert_eq!(parser.errors.len(), 0);
            // if program.statements.len() != 1 {
            //     panic!("Invalid number of statements");
            // }
            println!("START TEST");
            for (j, stmt) in program.statements.iter().enumerate() {
                println!("j: {}", j);
                println!("token: {:?}", stmt.token_literal());
                println!("stmt: {:?}", stmt.statement_node());
                assert_eq!(stmt.statement_node(), expected[i]);
            }
            break;
        }
    }
}
