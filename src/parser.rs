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

// impl Precedence {
//     pub fn decrement(&mut self) -> Precedence {
//         match self {
//             Precedence::Lowest => Precedence::Lowest,
//             Precedence::Equals => Precedence::Lowest,
//             Precedence::LessGreater => Precedence::Equals,
//             Precedence::Sum => Precedence::LessGreater,
//             Precedence::Product => Precedence::Sum,
//             Precedence::Prefix => Precedence::Product,
//             Precedence::Call => Precedence::Prefix,
//         }
//     }
//     pub fn increment(&mut self) -> Precedence {
//         match self {
//             Precedence::Lowest => Precedence::Equals,
//             Precedence::Equals => Precedence::LessGreater,
//             Precedence::LessGreater => Precedence::Sum,
//             Precedence::Sum => Precedence::Product,
//             Precedence::Product => Precedence::Prefix,
//             Precedence::Prefix => Precedence::Call,
//             Precedence::Call => Precedence::Prefix,
//         }
//     }
// }

type PrefixParseFn = fn(&mut Parser) -> Option<Box<Expression>>;
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Option<Box<Expression>>;

pub struct Parser {
    pub lexer: Lexer,
    pub cur_token: Token,
    pub peek_token: Option<Token>,
    pub errors: Vec<String>,
    pub prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    pub infix_parse_fns: HashMap<TokenType, InfixParseFn>,
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
        TokenType::LParen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

fn parse_infix_expression(parser: &mut Parser, left: Box<Expression>) -> Option<Box<Expression>> {
    // println!("parse_INFIX_expression");
    let token = parser.cur_token.clone();
    let operator = token.literal.clone();
    let precedence = parser.cur_precedence();

    parser.next_token();
    let expr = parser.parse_expression(precedence);
    // if let Some(expr) = &expr {
    //     println!("expr: {:?}", expr.to_string());
    // }
    Some(Box::new(Expression::InfixExpression {
        token,
        left: Some(left),
        operator,
        right: expr,
    }))
}
fn parse_call_expression(
    parser: &mut Parser,
    function: Box<Expression>,
) -> Option<Box<Expression>> {
    // println!("parse_call_expression");
    let token = parser.cur_token.clone();
    let arguments = parser.parse_call_arguments();
    Some(Box::new(Expression::CallExpression {
        token,
        function,
        arguments,
    }))
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        prefix_parse_fns.insert(TokenType::Ident, Parser::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Parser::parse_integer_literal);
        prefix_parse_fns.insert(TokenType::Bang, Parser::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::Minus, Parser::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::True, Parser::parse_boolean);
        prefix_parse_fns.insert(TokenType::False, Parser::parse_boolean);
        prefix_parse_fns.insert(TokenType::LParen, Parser::parse_grouped_expression);
        prefix_parse_fns.insert(TokenType::If, Parser::parse_if_expression);
        prefix_parse_fns.insert(TokenType::Function, Parser::parse_function_literal);

        let mut infix_parse_fns: HashMap<TokenType, InfixParseFn> = HashMap::new();
        infix_parse_fns.insert(TokenType::Plus, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Minus, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Slash, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Asterisk, parse_infix_expression);
        infix_parse_fns.insert(TokenType::Eq, parse_infix_expression);
        infix_parse_fns.insert(TokenType::NotEq, parse_infix_expression);
        infix_parse_fns.insert(TokenType::GT, parse_infix_expression);
        infix_parse_fns.insert(TokenType::LT, parse_infix_expression);
        infix_parse_fns.insert(TokenType::LParen, parse_call_expression);

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
        // println!("calling next_token");
        // println!("CURR_TOKEN {:?}", self.cur_token);
        match &self.peek_token {
            Some(peek) => {
                self.cur_token = peek.clone();
                self.peek_token = Some(self.lexer.next_token());
            }
            None => assert!(self.cur_token.token_type == TokenType::Eof),
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };
        // println!("parse_program");
        while self.cur_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();
            if let Some(stmt) = statement {
                // TODO: remove this
                if stmt.to_string().as_str() != "" {
                    program.statements.push(stmt);
                }
                // println!("parsed_statement {}", stmt.to_string());
            } else {
                // println!("Could not parse statement");
            }

            self.next_token();
        }
        program
    }
    fn parse_statement(&mut self) -> Option<Statement> {
        // println!("parse_statement cur_token: {:?}", self.cur_token);
        match self.cur_token.token_type {
            TokenType::Let => {
                let stmt = match self.parse_let_statement() {
                    Some(stmt) => stmt,
                    None => return None,
                };
                Some(stmt)
            }
            TokenType::Return => {
                let stmt = match self.parse_return_statement() {
                    Some(stmt) => stmt,
                    None => return None,
                };

                Some(stmt)
            }
            TokenType::Semicolon => {
                self.next_token();
                self.parse_statement()
            }
            _ => {
                let stmt = match self.parse_expression_statement() {
                    Some(stmt) => stmt,
                    None => return None,
                };

                Some(stmt)
            } // _ => None,
        }
    }
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let return_token = self.cur_token.clone();

        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }
        Some(Statement::ReturnStatement {
            token: return_token,
            expr,
        })
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();
        // println!("parse_let_statement let_token: {:?}", let_token);
        if self.expect_peek(TokenType::Ident).is_err() {
            // println!("ERROR IN LET STATEMENT: {e}");
            let peek_token = self.peek_token.clone().unwrap();
            // println!("ERROR IN LET STATEMENT: {e}");
            let err = WrongTokenError::new(peek_token, TokenType::Ident);
            self.errors.push(err.to_string());
            while !(self.cur_token_is(TokenType::Semicolon) || self.cur_token_is(TokenType::Eof)) {
                self.next_token();
            }
            return None;
        }
        let value = self.cur_token.literal.clone();
        let ident = Expression::Identifier {
            token: self.cur_token.clone(),
            value,
        };

        if self.expect_peek(TokenType::Assign).is_err() {
            // println!("ERROR IN LET STATEMENT: {e}");
            let peek_token = self.peek_token.clone().unwrap();
            let err = WrongTokenError::new(peek_token, TokenType::Assign);
            self.errors.push(err.to_string());
            while !(self.cur_token_is(TokenType::Semicolon) || self.cur_token_is(TokenType::Eof)) {
                self.next_token();
            }
            return None;
        }
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);

        while !(self.cur_token_is(TokenType::Semicolon) || self.cur_token_is(TokenType::Eof)) {
            self.next_token();
        }

        Some(Statement::LetStatement {
            token: let_token,
            name: ident,
            value: expr,
        })
    }
    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expr = self.parse_expression(Precedence::Lowest);
        Some(Statement::ExpressionStatement { token, expr })
    }
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<Expression>> {
        let prefix: Option<&PrefixParseFn> = self.prefix_parse_fns.get(&self.cur_token.token_type);
        match prefix {
            Some(prefix) => {
                let mut left = prefix(self);
                let mut peek_precedence: Precedence = self.peek_precedence().unwrap();
                let mut comparison = precedence < peek_precedence;
                // if let Some(l) = left.as_ref() {
                //     // if peek_precedence == Precedence::Sum {
                //     //     peek_precedence = peek_precedence.decrement();
                //     // }
                //     println!(
                //         "precedence {:?} peek_precedence {:?} comparison: {} prefix: {}",
                //         precedence,
                //         peek_precedence,
                //         comparison,
                //         l.to_string()
                //     );
                //     // println!("left {}", l.to_string());
                // } else {
                //     println!(
                //         "precedence {:?} peek_precedence {:?} comparison: {} prefix: None",
                //         precedence, peek_precedence, comparison,
                //     );
                // }

                while !(self.peek_token_is(TokenType::Eof)) && comparison {
                    let infixes = self.infix_parse_fns.clone();
                    // println!("infixes : {:?}", infixes.keys());

                    // println!("peek_token1111: {:?}", self.peek_token.clone());
                    let infix = infixes.get(&self.peek_token.clone().unwrap().token_type);
                    // println!("infix {:?}", infix.is_none());
                    // println!("LEFT: {:?}", left.as_ref().unwrap().to_string());
                    if infix.is_none() {
                        return left;
                    }
                    self.next_token();
                    let infix = infix.unwrap();
                    left = infix(self, left.unwrap());
                    // println!("AFTER: {:?}", left.as_ref().unwrap().to_string());
                    peek_precedence = self.peek_precedence().unwrap();
                    comparison = precedence < peek_precedence;
                }
                // println!("RETURNING LEFT: {:?}", left.as_ref().unwrap().to_string());
                Some(left.unwrap())
            }
            None => {
                // let msg = format!(
                //     "Could not find prefix_parse_fn for {:?}. Available keys are {:?}",
                //     &self.cur_token.token_type,
                //     self.prefix_parse_fns.keys()
                // );
                // self.errors.push(msg.clone());
                None
                // panic!("msg: {}", msg.as_str());
            }
        }
    }
    fn parse_call_arguments(&mut self) -> Vec<Box<Expression>> {
        // println!("parse_call_arguments");
        let mut args = Vec::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return args;
        }
        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest).unwrap());
        }
        if let Err(err) = self.expect_peek(TokenType::RParen) {
            self.errors.push(err.to_string());
            return Vec::new();
        }
        // self.next_token();
        // Some(Box::new(Identifier::new(token)))
        args
    }
    fn parse_identifier(&mut self) -> Option<Box<Expression>> {
        // println!("parse_identifier");
        let token = self.cur_token.clone();
        let ident = Expression::Identifier {
            token: token.clone(),
            value: token.literal,
        };
        Some(Box::new(ident))
    }
    fn parse_integer_literal(&mut self) -> Option<Box<Expression>> {
        let token = self.cur_token.clone();
        // let value
        let value = token.literal.parse::<i64>();
        if value.is_err() {
            let msg = format!("Could not parse {} as integer", token.literal);
            self.errors.push(msg);
            return None;
        }

        let int_literal = Expression::IntegerLiteral {
            token,
            value: value.unwrap(),
        };
        Some(Box::new(int_literal))
    }
    fn parse_boolean(&mut self) -> Option<Box<Expression>> {
        let token = self.cur_token.clone();
        // let value
        let value = token.literal.to_lowercase().parse::<bool>();
        if let Err(_) = value {
            let msg = format!("Could not parse {} as boolean", token.literal);
            self.errors.push(msg);
            return None;
        }

        let int_literal = Expression::Boolean {
            token: token,
            value: value.unwrap(),
        };
        Some(Box::new(int_literal))
    }
    fn parse_prefix_expression(&mut self) -> Option<Box<Expression>> {
        // println!("parse_prefix_expression cur_token {:?}", self.cur_token);
        let token = self.cur_token.clone();
        let operator = token.literal.clone();
        self.next_token();
        let expr = self.parse_expression(Precedence::Prefix);
        // println!("new cur_token {:?}", self.cur_token);
        let prefix_expr = Expression::PrefixExpression {
            token,
            operator,
            right: expr,
        };
        Some(Box::new(prefix_expr))
    }
    fn parse_grouped_expression(&mut self) -> Option<Box<Expression>> {
        // println!("parse_prefix_expression cur_token {:?}", self.cur_token);
        let token = self.cur_token.clone();
        let operator = token.literal.clone();
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);
        if self.expect_peek(TokenType::RParen).is_err() {
            let msg = format!("Could not parse {} as grouped expression", token.literal);
            self.errors.push(msg);
            return None;
        }
        // println!("new cur_token {:?}", self.cur_token);
        let prefix_expr = Expression::PrefixExpression {
            token,
            operator,
            right: expr,
        };
        Some(Box::new(prefix_expr))
    }
    fn parse_if_expression(&mut self) -> Option<Box<Expression>> {
        // println!("parse_if_expression cur_token {:?}", self.cur_token);
        let token = self.cur_token.clone();
        if self.expect_peek(TokenType::LParen).is_err() {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        if self.expect_peek(TokenType::RParen).is_err()
            | self.expect_peek(TokenType::LBrace).is_err()
        {
            return None;
        }
        let consequence = self.parse_block_statement();
        // self.next_token();
        // println!("-------------------");
        // println!("PARSED IF TOKEN {:?}", token);
        // println!("PARSED CONDITION {:?}", condition.to_string());
        // println!("PARSED CONSEQUENCE {:?}", consequence.to_string());
        // println!("CURR TOKEN {:?}", self.cur_token);

        if self.expect_peek(TokenType::Else).is_err() {
            return Some(Box::new(Expression::IfExpression {
                token,
                condition,
                consequence,
                alternative: None,
            }));
        }
        // println!("CURR TOKEN {:?}", self.cur_token);
        // println!("next {:?}", self.peek_token);
        if self.expect_peek(TokenType::LBrace).is_err() {
            return None;
        }

        let alternative = self.parse_block_statement();
        Some(Box::new(Expression::IfExpression {
            token,
            condition,
            consequence,
            alternative: Some(alternative),
        }))
    }
    fn parse_function_literal(&mut self) -> Option<Box<Expression>> {
        // println!("parse_function_literal cur_token {:?}", self.cur_token);
        let token = self.cur_token.clone();
        if self.expect_peek(TokenType::LParen).is_err() {
            return None;
        }
        self.next_token();
        println!("CUR_TOKEN {:?}", self.cur_token);
        let parameters = self.parse_function_parameters();
        println!("PARAMETERS {:?}", self.cur_token);
        if self.expect_peek(TokenType::LBrace).is_err() {
            return None;
        }
        let body = self.parse_block_statement();
        // println!("-------------------");
        // println!("PARSED function TOKEN {:?}", token);
        // // println!("PARSED CONDITION {:?}", parameters.to_string());
        // println!("PARSED BODY {:?}", body.to_string());
        Some(Box::new(Expression::FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }
    fn parse_function_parameters(&mut self) -> Vec<Box<Expression>> {
        // println!("parse_function_parameters cur_token {:?}", self.cur_token);
        let mut identifiers = Vec::new();
        if self.peek_token_is(TokenType::RParen) {
            if self.cur_token_is(TokenType::LParen) {
                return identifiers;
            }
            let ident = Expression::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            identifiers.push(Box::new(ident));
            self.next_token();
            return identifiers;
        }

        println!(
            "BEFORE ERRROR parse_function_parameters cur_token {:?}",
            self.cur_token
        );
        let ident = Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        // println!("parse_function_parameters IDENT {:?}", ident.to_string());
        identifiers.push(Box::new(ident));
        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            // println!(
            //     "1. parse_function_parameters cur_token {:?}",
            //     self.cur_token
            // );
            self.next_token();
            // println!(
            //     "2. parse_function_parameters cur_token {:?}",
            //     self.cur_token
            // );
            let ident = Expression::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            identifiers.push(Box::new(ident));
        }
        if self.expect_peek(TokenType::RParen).is_err() {
            return Vec::new();
        }
        identifiers
    }
    fn parse_block_statement(&mut self) -> BlockStatement {
        // println!("parse_program");

        let mut statements = Vec::new();
        let token = self.cur_token.clone();
        // println!("PARSE_BLOCK cur_token {:?}", self.cur_token);
        self.next_token();
        // println!("PARSE_BLOCK cur_token {:?}", self.cur_token);
        while self.cur_token.token_type != TokenType::Semicolon
            && self.cur_token.token_type != TokenType::RBrace
            && self.cur_token.token_type != TokenType::Eof
        {
            let statement = self.parse_statement();
            if let Some(stmt) = statement {
                // println!("BLOCK STATEMENT PARSED: {}", stmt.to_string());
                statements.push(stmt);
            } else {
                // println!("Could not parse statement");
            }

            self.next_token();
        }
        if self.cur_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        BlockStatement::new(token, statements)
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

    // fn test_identifier(parser: &mut Parser, expected: &str) {
    //     let program = parser.parse_program();
    //     assert_eq!(1, program.statements.len());
    //     assert_eq!(0, parser.errors.len());
    //     let stmt = program.statements[0].to_string();
    //     assert_eq!(stmt, expected);
    // }

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
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        assert_eq!(parser.errors.len(), 0);
        let expected = [
            ("let", "x", "5"),
            ("let", "y", "10"),
            ("let", "foobar", "838383"),
        ];

        for (i, stmt) in program.statements.iter().enumerate() {
            match stmt {
                Statement::LetStatement { token, name, value } => {
                    let value = value.as_ref().unwrap();
                    println!("token: {}", token);
                    println!("name: {}", name.to_string());
                    println!("value: {}", value.to_string());
                    println!("expected: {:?}", expected[i]);
                    println!("----------------");
                    assert_eq!(token.to_string(), expected[i].0);
                    assert_eq!(name.to_string(), expected[i].1);
                    assert_eq!(value.to_string(), expected[i].2);
                }
                _ => panic!("Expected LetStatement"),
            }
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
        for err in parser.errors.iter() {
            println!("err: {}", err);
        }
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
    fn test_if_statement() {
        println!("test_if_statement");
        let inputs = ["if (x < y) { x }", "if (x < y) { x } else { y }"];
        let expected = [
            ("if", "(x < y)", "x", "", ""),
            ("if", "(x < y)", "x", "else", "y"),
        ];
        // let expected = ["IF (X < Y) X ELSE Y"];
        for (i, input) in inputs.iter().enumerate() {
            println!("input: {:?}", input.to_string());
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            // println!("input: {:?}", program.statements);
            assert_eq!(parser.errors.len(), 0);

            for (_, stmt) in program.statements.iter().enumerate() {
                match stmt {
                    Statement::ExpressionStatement { token: _, expr } => {
                        // let value = value.as_ref().unwrap();
                        // assert_eq!(expr.as_ref().unwrap().to_string(), expected[i]);
                        let expr = (*expr.as_ref().unwrap()).clone();
                        match *expr {
                            Expression::IfExpression {
                                token,
                                condition,
                                consequence,
                                alternative,
                            } => {
                                println!("token: {}", token);
                                println!("condition: {}", condition.to_string());
                                println!("consequence: {}", consequence.to_string());
                                match alternative {
                                    Some(alternative) => {
                                        println!("alternative: {}", alternative.to_string());
                                        assert_eq!(alternative.to_string(), expected[i].4);
                                    }
                                    None => println!("alternative: None"),
                                }
                                println!("----------------");
                                assert_eq!(token.to_string(), expected[i].0);
                                assert_eq!(condition.to_string(), expected[i].1);
                                assert_eq!(consequence.to_string(), expected[i].2);
                            }
                            _ => panic!("Expected IfExpression"),
                        }
                        // assert_eq!(token.to_string(), expected[i].0);
                        // assert_eq!(name.to_string(), expected[i].1);
                    }
                    _ => panic!("Expected LetStatement"),
                }
            }
            // assert!(false);
        }
    }
    //
    #[test]
    fn test_function_statement() {
        println!("test_function_statement");
        // let inputs = ["if (x < y) { x }"];
        let inputs = [
            //"fn(x, y) { x + y; }",
            "fn(x) { x + 2; };",
        ];
        // let expected = ["IF (X < Y) X"]; // "IF (X < Y) X ELSE Y"];
        let expected = [
            // ("fn", ("x", "y"), "(x + y)"),
            ("fn", ("x", ""), "(x + 2)"),
        ];
        for (i, input) in inputs.iter().enumerate() {
            println!("input: {:?}", input.to_string());
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            // println!("input: {:?}", program.statements);
            assert_eq!(parser.errors.len(), 0);

            for (i, stmt) in program.statements.iter().enumerate() {
                match stmt {
                    Statement::ExpressionStatement { token: _, expr } => {
                        println!("expr: {:?}", expr);
                        let expr = (*expr.as_ref().unwrap()).clone();
                        match *expr {
                            Expression::FunctionLiteral {
                                token,
                                parameters,
                                body,
                            } => {
                                println!("token: {}", token);
                                println!("params: {:?}", parameters);
                                println!("body: {}", body.to_string());
                                println!("----------------");
                                assert_eq!(token.to_string(), expected[i].0);
                                assert_eq!(parameters[0].to_string(), expected[i].1 .0);
                                assert_eq!(body.to_string(), expected[i].2);
                            }
                            _ => panic!("Expected IfExpression"),
                        }
                    }
                    _ => panic!("Expected LetStatement"),
                }
            }
        }
    }
    #[test]
    fn test_call_statement() {
        println!("test_function_statement");
        // let inputs = ["if (x < y) { x }"];
        let inputs = ["add(1, 2 * 3, 4 + 5)"];
        // let expected = ["IF (X < Y) X"]; // "IF (X < Y) X ELSE Y"];
        let expected = [("add", ("1", "(2 * 3)", "(4 + 5)"))];
        let expected_str = ["add (1, (2 * 3), (4 + 5))"];
        for (i, input) in inputs.iter().enumerate() {
            println!("input: {:?}", input.to_string());
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            // println!("input: {:?}", program.statements);
            assert_eq!(parser.errors.len(), 0);

            for (i, stmt) in program.statements.iter().enumerate() {
                println!("i: {}", i);
                assert_eq!(stmt.to_string(), expected_str[i]);
                match stmt {
                    Statement::ExpressionStatement { token, expr } => {
                        println!("expr: {:?}", expr);
                        let expr = (*expr.as_ref().unwrap()).clone();
                        assert_eq!(token.to_string(), expected[i].0);
                        match *expr {
                            Expression::CallExpression {
                                token: _,
                                function,
                                arguments,
                            } => {
                                println!("token: {}", token);
                                println!("function: {}", function.to_string());
                                println!("params: {:?}", arguments);
                                println!("----------------");
                                assert_eq!(function.to_string(), expected[i].0);
                                assert_eq!(arguments[0].to_string(), expected[i].1 .0);
                                assert_eq!(arguments[1].to_string(), expected[i].1 .1);
                                assert_eq!(arguments[2].to_string(), expected[i].1 .2);
                            }
                            _ => panic!("Expected IfExpression"),
                        }
                    }
                    _ => panic!("Expected ExpressionStatement"),
                }
            }
        }
    }
    #[test]
    fn test_return_statement() {
        println!("test_return_statement");
        let input = "return 5;
    return 10;
    return 993322;
    ";
        println!("input: {}", input);
        let expected = [("return", "5"), ("return", "10"), ("return", "993322")];
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        // println!("input: {:?}", program.statements);
        assert_eq!(parser.errors.len(), 0);
        if program.statements.len() != 3 {
            panic!("Invalid number of statements");
        }

        for (i, stmt) in program.statements.iter().enumerate() {
            match stmt {
                Statement::ReturnStatement { token, expr } => {
                    let expr = expr.clone().unwrap();
                    println!("token: {}", token);
                    println!("name: {}", expr.to_string());
                    println!("expected: {:?}", expected[i]);
                    println!("----------------");
                    assert_eq!(token.to_string(), expected[i].0);
                    assert_eq!(expr.to_string(), expected[i].1);
                }
                _ => panic!("Expected ReturnStatement"),
            }
        }
    }
    #[test]
    fn test_identifier_expression() {
        println!("test_identity_expression");
        let input = "foobar;";
        let expected = ["foobar"];
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

        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            assert_eq!(stmt.to_string(), expected[i]);
            match stmt {
                Statement::ExpressionStatement { token, expr } => {
                    println!("expr: {:?}", expr);
                    let expr = (*expr.as_ref().unwrap()).clone();
                    assert_eq!(token.to_string(), expected[i]);
                    match *expr {
                        Expression::Identifier { token, value } => {
                            assert_eq!(token.to_string(), expected[i]);
                            assert_eq!(value, expected[i]);
                        }
                        _ => panic!("Expected IfExpression"),
                    }
                }
                _ => panic!("Expected ExpressionStatement"),
            }
        }
    }
    #[test]
    fn test_integer_literal_expression() {
        println!("test_integer_literal_expression");
        let input = "5;";
        let expected = [5];
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

        println!("START TEST");
        for (i, stmt) in program.statements.iter().enumerate() {
            println!("i: {}", i);
            assert_eq!(stmt.to_string(), expected[i].to_string());
            match stmt {
                Statement::ExpressionStatement { token, expr } => {
                    println!("expr: {:?}", expr);
                    let expr = (*expr.as_ref().unwrap()).clone();
                    assert_eq!(token.to_string(), expected[i].to_string());
                    match *expr {
                        Expression::IntegerLiteral { token, value } => {
                            assert_eq!(token.to_string(), expected[i].to_string());
                            assert_eq!(value, expected[i]);
                        }
                        _ => panic!("Expected IfExpression"),
                    }
                }
                _ => panic!("Expected ExpressionStatement"),
            }
        }
    }
    #[test]
    fn test_boolean_expression() {
        println!("test_integer_literal_expression");
        let inputs = ["true;", "false;"];
        let expected = [true, false];
        for (i, input) in inputs.iter().enumerate() {
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

            println!("START TEST");
            for (j, stmt) in program.statements.iter().enumerate() {
                println!("i: {}", i);
                assert_eq!(stmt.to_string(), expected[i].to_string());
                match stmt {
                    Statement::ExpressionStatement { token, expr } => {
                        println!("expr: {:?}", expr);
                        let expr = (*expr.as_ref().unwrap()).clone();
                        assert_eq!(token.to_string(), expected[i].to_string());
                        match *expr {
                            Expression::Boolean { token, value } => {
                                assert_eq!(token.to_string(), expected[i].to_string());
                                assert_eq!(value, expected[i]);
                            }
                            _ => panic!("Expected IfExpression"),
                        }
                    }
                    _ => panic!("Expected ExpressionStatement"),
                }
            }
        }
    }
    #[test]
    fn test_prefix_expressions() {
        println!("test_prefix_expressions");
        let inputs = ["!5;", "-15;", "-a;", "!true;", "!false;"];
        let expected = [
            ("!", "5"),
            ("-", "15"),
            ("-", "a"),
            ("!", "true"),
            ("!", "false"),
        ];
        let expected_str = ["(! 5)", "(- 15)", "(- a)", "(! true)", "(! false)"];
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

            println!("START TEST");
            for (j, stmt) in program.statements.iter().enumerate() {
                assert_eq!(stmt.to_string(), expected_str[i]);
                match stmt {
                    Statement::ExpressionStatement { token, expr } => {
                        println!("expr: {:?}", expr);
                        let expr = (*expr.as_ref().unwrap()).clone();
                        match *expr {
                            Expression::PrefixExpression {
                                token,
                                operator,
                                right,
                            } => {
                                assert_eq!(token.to_string(), expected[i].0);
                                assert_eq!(right.unwrap().to_string(), expected[i].1);
                            }
                            _ => panic!("Expected IfExpression"),
                        }
                    }
                    _ => panic!("Expected ExpressionStatement"),
                }
            }
        }
    }
    #[test]
    fn test_infix_expressions() {
        println!("test_infix_expressions");
        let inputs = [
            "1 + 5;",
            "5 - 5;",
            "5 * 5;",
            "5 / 5;",
            "5 > 5;",
            "5 < 5;",
            "5 == 5;",
            "5 != 5;",
            "true == true;",
            "true != false;",
            "false == false;",
        ];

        let expected = [
            "(1 + 5)",
            "(5 - 5)",
            "(5 * 5)",
            "(5 / 5)",
            "(5 > 5)",
            "(5 < 5)",
            "(5 == 5)",
            "(5 != 5)",
            "(true == true)",
            "(true != false)",
            "(false == false)",
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
                println!("stmt: {:?}", stmt.to_string());
                assert_eq!(stmt.to_string(), expected[i]);
            }
        }
    }
    #[test]
    fn test_operator_precedence_partins() {
        println!("test_operator_precedence_partins");
        let inputs = [
            "1 + 2 + 3",
            "-3 * 5",
            "-a * b",
            "!-a",
            "a + b + c",
            "a + b - c",
            "a * b * c",
            "a * b / c",
            "a + b / c",
            "1 + 2 * 3 + 4 / 5",
            "a + b * c + d / e",
            "a + b * c + d / e - f",
            // "3 + 4; -5 * 5",
            "5 > 4 == 3 < 4",
            "5 < 4 != 3 > 4",
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "true",
            "false",
            "3 > 5 == false",
            "3 < 5 == true",
            "!(true == true)",
            "1 + (2 + 3) + 4",
            "(5 + 5) * 2",
            "2 / (5 + 5)",
            "-(5 + 5)",
        ];

        let expected = [
            "((1 + 2) + 3)",
            "((- 3) * 5)",
            "((- a) * b)",
            "(! (- a))",
            "((a + b) + c)",
            "((a + b) - c)",
            "((a * b) * c)",
            "((a * b) / c)",
            "(a + (b / c))",
            "((1 + (2 * 3)) + (4 / 5))",
            "((a + (b * c)) + (d / e))",
            "(((a + (b * c)) + (d / e)) - f)",
            // "(3 + 4)((-5) * 5)",
            "((5 > 4) == (3 < 4))",
            "((5 < 4) != (3 > 4))",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            "true",
            "false",
            "((3 > 5) == false)",
            "((3 < 5) == true)",
            "(! (true == true))",
            "((1 + (2 + 3)) + 4)",
            "((5 + 5) * 2)",
            "(2 / (5 + 5))",
            "(- (5 + 5))",
        ];
        println!("input: {:?}", inputs);
        for (i, input) in inputs.iter().enumerate() {
            println!("input: {:?}", input.to_string());
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            println!("errors: {:?}", parser.errors);
            assert_eq!(parser.errors.len(), 0);
            for (j, stmt) in program.statements.iter().enumerate() {
                println!("j: {}", j);
                println!("stmt: {:?}", stmt.to_string());
                // println!("stmt: {:?}", program.statements[1].statement_node());
                assert_eq!(stmt.to_string(), expected[i]);
            }
        }
    }
}
