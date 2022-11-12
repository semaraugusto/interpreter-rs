use crate::token::Token;
use crate::token::TokenType;
use std::fmt;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) -> String;
}

pub trait Expression: Node {
    fn expression_node(&self) -> String;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            return self.statements[0].token_literal();
        }
        "NOT_IMPLEMENTED".to_string()
    }
    fn to_string(&self) -> String {
        if !self.statements.is_empty() {
            return self.statements[0].token_literal();
        }
        "NOT_IMPLEMENTED".to_string()
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let value = match &self.value {
            None => "NOT_IMPLEMENTED".to_string(),
            Some(value) => value.to_string(),
        };
        format!(
            "{} {} = {}",
            self.token.literal,
            self.name.token_literal(),
            value
        )
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> String {
        self.to_string()
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub expr: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
    fn to_string(&self) -> String {
        let value = match &self.expr {
            Some(value) => value.to_string(),
            None => "NOT_IMPLEMENTED".to_string(),
        };
        format!("{} {}", self.token.literal, value)
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> String {
        self.to_string()
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl Identifier {
    pub fn new(token: Token) -> Self {
        let value = token.literal.clone();
        if token.token_type != TokenType::Ident {
            panic!("Not an identifier");
        }
        Self { token, value }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        self.token.to_string()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) -> String {
        self.to_string()
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expr: Option<Box<dyn Expression>>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        let value = match &self.expr {
            None => "NOT_IMPLEMENTED".to_string(),
            Some(value) => value.expression_node(),
        };
        format!("{}", value)
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> String {
        self.to_string()
    }
}

pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl IntegerLiteral {
    pub fn new(token: Token) -> Self {
        if token.token_type != TokenType::Int {
            panic!("Not an integer");
        }
        let value: i64 = token
            .literal
            .parse()
            .expect("Not an integer (Should not happen)");
        Self { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) -> String {
        self.to_string()
    }
}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl PrefixExpression {
    pub fn new(token: Token, right: Option<Box<dyn Expression>>) -> Self {
        match token.token_type {
            TokenType::Bang | TokenType::Minus => (),
            _ => panic!("Not a prefix operator"),
        }
        let operator = token.literal.clone();
        Self {
            token,
            operator,
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        // format!("{}", self.value)
        if let Some(right) = &self.right {
            return format!("({} {})", self.operator, right.to_string());
        }
        unreachable!()
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) -> String {
        // if let Some(right) = &self.right {
        //     return format!("({} {})", self.operator, right.to_string());
        // }
        // format!("({} NOT_IMPLEMENTED)", self.operator,)
        self.to_string()
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Option<Box<dyn Expression>>,
        right: Option<Box<dyn Expression>>,
    ) -> Self {
        match token.token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Slash
            | TokenType::Asterisk
            | TokenType::Eq
            | TokenType::NotEq
            | TokenType::LT
            | TokenType::GT => (),
            _ => panic!("Not a infix operator"),
        }
        let operator = token.literal.clone();
        Self {
            token,
            left,
            operator,
            right,
        }
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn to_string(&self) -> String {
        // format!("{}", self.value)
        if let Some(right) = &self.right {
            if let Some(left) = &self.left {
                return format!(
                    "({} {} {})",
                    left.to_string(),
                    self.operator,
                    right.to_string()
                );
            }
        }
        unreachable!()
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) -> String {
        self.to_string()
    }
}
