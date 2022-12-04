use crate::token::Token;
use crate::token::TokenType;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Statement(Statement),
    Program(Program),
    Expression(Expression),
    BlockStatement(BlockStatement),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Statement(stmt) => write!(f, "{}", stmt.to_string()),
            Node::Program(prog) => write!(f, "{}", prog.to_string()),
            Node::Expression(expr) => write!(f, "{}", expr.to_string()),
            Node::BlockStatement(stmt) => write!(f, "{}", stmt.to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement {
        token: Token,
        name: Expression,
        value: Option<Box<Expression>>,
    },
    ReturnStatement {
        token: Token,
        expr: Option<Box<Expression>>,
    },
    ExpressionStatement {
        token: Token,
        expr: Option<Box<Expression>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}
impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        Self { token, statements }
    }
    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    pub fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>()
            .join(" ")
    }
    pub fn statement_node(&self) -> String {
        self.to_string()
    }
}
impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier {
        token: Token,
        value: String,
    },
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    PrefixExpression {
        token: Token,
        operator: String,
        right: Option<Box<Expression>>,
    },
    InfixExpression {
        token: Token,
        left: Option<Box<Expression>>,
        operator: String,
        right: Option<Box<Expression>>,
    },
    IfExpression {
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        token: Token,
        parameters: Vec<Box<Expression>>, // Identifiers
        body: BlockStatement,
    },
    CallExpression {
        token: Token,
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
}

impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::Identifier { token, value: _ } => token.to_string(),
            Expression::IntegerLiteral { token: _, value } => format!("{}", value),
            Expression::Boolean { token: _, value } => format!("{}", value),
            Expression::PrefixExpression {
                token,
                operator,
                right,
            } => {
                if let Some(right) = &right {
                    if token.token_type != TokenType::LParen {
                        return format!("({} {})", operator, right.to_string());
                    }
                    return right.to_string();
                }
                unreachable!()
            }
            Expression::InfixExpression {
                token: _,
                left,
                operator,
                right,
            } => {
                // format!("{}", self.value)
                if let Some(right) = &right {
                    if let Some(left) = &left {
                        return format!(
                            "({} {} {})",
                            left.to_string(),
                            operator,
                            right.to_string()
                        );
                    }
                }
                unreachable!()
            }

            Expression::IfExpression {
                token: _,
                condition,
                consequence,
                alternative,
            } => {
                // format!("{}", self.value)
                match &alternative {
                    Some(alt) => format!(
                        "IF {} {} ELSE {}",
                        condition.to_string(),
                        consequence.to_string(),
                        alt.to_string()
                    ),
                    None => format!("IF {} {}", condition.to_string(), consequence.to_string()),
                }
            }
            Expression::FunctionLiteral {
                token,
                parameters,
                body,
            } => {
                // format!("{}", self.value)
                let mut params = Vec::new();
                for p in parameters {
                    params.push(p.to_string());
                }
                format!("{} ({}) -> {}", token, params.join(", "), body.to_string())
            }
            Expression::CallExpression {
                token: _,
                function,
                arguments,
            } => {
                // format!("{}", self.value)
                let mut args = Vec::new();
                for arg in arguments {
                    args.push(arg.to_string());
                }
                format!("{} ({})", function.to_string(), args.join(", "),)
            }
        }
    }
}

// impl Node {
//     fn token_literal(&self) -> String {
//         todo!()
//     }
//     fn to_string(&self) -> String {
//         todo!()
//     }
// }

impl Statement {
    pub fn to_string(&self) -> String {
        match self {
            Statement::LetStatement { token, name, value } => match value {
                Some(value) => format!(
                    "{} {} = {}",
                    token.literal,
                    name.to_string(),
                    value.to_string()
                ),
                None => "ERROR IN LET_STATEMENT_PARSING".to_string(),
            },
            Statement::ReturnStatement { token, expr } => {
                let value = match &expr {
                    Some(value) => value.to_string(),
                    None => "RETURN_STATEMENT_ERROR".to_string(),
                };
                format!("{} {}", token.literal, value)
            }
            Statement::ExpressionStatement { token, expr } => match &expr {
                Some(value) => value.to_string(),
                None => token.to_string(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn to_string(&self) -> String {
        let mut out = String::new();
        for stmt in self.statements.iter() {
            out += stmt.to_string().as_str();
        }
        out
    }
}
///////////////////////
///////////////////////
///////////////////////

// use crate::token::Token;
// use crate::token::TokenType;
//
// pub trait Node {
//     fn token_literal(&self) -> String;
//     fn to_string(&self) -> String;
// }
//
// pub trait Statement: Node {
//     fn statement_node(&self) -> String;
// }
//
// pub trait Expression: Node {
//     fn expression_node(&self) -> String;
// }
//
// pub struct Program {
//     pub statements: Vec<Box<dyn Statement>>,
// }
//
// impl Node for Program {
//     fn token_literal(&self) -> String {
//         if !self.statements.is_empty() {
//             return self.statements[0].token_literal();
//         }
//         "PROGRAM_STRING_NOT_IMPLEMENTED".to_string()
//     }
//     fn to_string(&self) -> String {
//         if !self.statements.is_empty() {
//             return self.statements[0].token_literal();
//         }
//         "PROGRAM_STRING".to_string()
//     }
// }
//
// pub struct LetStatement {
//     pub token: Token,
//     pub name: Identifier,
//     pub value: Option<Box<dyn Expression>>,
// }
//
// impl Node for LetStatement {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         let value = match &self.value {
//             None => "LET_STATEMENT_ERROR".to_string(),
//             Some(value) => value.to_string(),
//         };
//         format!(
//             "{} {} = {}",
//             self.token.literal,
//             self.name.token_literal(),
//             value
//         )
//     }
// }
//
// impl Statement for LetStatement {
//     fn statement_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct ReturnStatement {
//     pub token: Token,
//     pub expr: Option<Box<dyn Expression>>,
// }
//
// impl Node for ReturnStatement {
//     fn token_literal(&self) -> String {
//         self.token.to_string()
//     }
//     fn to_string(&self) -> String {
//         let value = match &self.expr {
//             Some(value) => value.to_string(),
//             None => "RETURN_STATEMENT_ERROR".to_string(),
//         };
//         format!("{} {}", self.token.literal, value)
//     }
// }
//
// impl Statement for ReturnStatement {
//     fn statement_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct Identifier {
//     pub token: Token,
//     pub value: String,
// }
// impl Identifier {
//     pub fn new(token: Token) -> Self {
//         let value = token.literal.clone();
//         // println!("IDENTIFIER: {}", value);
//         if token.token_type != TokenType::Ident {
//             panic!("Not an identifier");
//         }
//         Self { token, value }
//     }
// }
//
// impl Node for Identifier {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         self.token.to_string()
//     }
// }
//
// impl Expression for Identifier {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct ExpressionStatement {
//     pub token: Token,
//     pub expr: Option<Box<dyn Expression>>,
// }
//
// impl Node for ExpressionStatement {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         match &self.expr {
//             None => self.token.to_string(),
//             Some(value) => value.to_string(),
//         }
//         // format!("{}", value)
//     }
// }
//
// impl Statement for ExpressionStatement {
//     fn statement_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct IntegerLiteral {
//     pub token: Token,
//     pub value: i64,
// }
// impl IntegerLiteral {
//     pub fn new(token: Token) -> Self {
//         if token.token_type != TokenType::Int {
//             panic!("Not an integer");
//         }
//         let value: i64 = token
//             .literal
//             .parse()
//             .expect("Not an integer (Should not happen)");
//         Self { token, value }
//     }
// }
//
// impl Node for IntegerLiteral {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         format!("{}", self.value)
//     }
// }
//
// impl Expression for IntegerLiteral {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct Boolean {
//     pub token: Token,
//     pub value: bool,
// }
// impl Boolean {
//     pub fn new(token: Token) -> Self {
//         match token.token_type {
//             TokenType::True => Self { token, value: true },
//             TokenType::False => Self {
//                 token,
//                 value: false,
//             },
//             _ => panic!("Not a boolean"),
//         }
//     }
// }
//
// impl Node for Boolean {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         format!("{}", self.value).to_uppercase()
//     }
// }
//
// impl Expression for Boolean {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct PrefixExpression {
//     pub token: Token,
//     pub operator: String,
//     pub right: Option<Box<dyn Expression>>,
// }
//
// impl PrefixExpression {
//     pub fn new(token: Token, right: Option<Box<dyn Expression>>) -> Self {
//         match token.token_type {
//             TokenType::LParen | TokenType::Bang | TokenType::Minus => (),
//             _ => panic!("Not a prefix operator"),
//         }
//         let operator = token.literal.clone();
//         Self {
//             token,
//             operator,
//             right,
//         }
//     }
// }
//
// impl Node for PrefixExpression {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         // format!("{}", self.value)
//         if let Some(right) = &self.right {
//             if self.token.token_type != TokenType::LParen {
//                 return format!("({} {})", self.operator, right.to_string());
//             }
//             return right.to_string();
//         }
//         unreachable!()
//     }
// }
//
// impl Expression for PrefixExpression {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct InfixExpression {
//     pub token: Token,
//     pub left: Option<Box<dyn Expression>>,
//     pub operator: String,
//     pub right: Option<Box<dyn Expression>>,
// }
//
// impl InfixExpression {
//     pub fn new(
//         token: Token,
//         left: Option<Box<dyn Expression>>,
//         right: Option<Box<dyn Expression>>,
//     ) -> Self {
//         match token.token_type {
//             TokenType::Plus
//             | TokenType::Minus
//             | TokenType::Slash
//             | TokenType::Asterisk
//             | TokenType::Eq
//             | TokenType::NotEq
//             | TokenType::LT
//             | TokenType::GT => (),
//             _ => panic!("Not a infix operator"),
//         }
//         let operator = token.literal.clone();
//         Self {
//             token,
//             left,
//             operator,
//             right,
//         }
//     }
// }
//
// impl Node for InfixExpression {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         // format!("{}", self.value)
//         if let Some(right) = &self.right {
//             if let Some(left) = &self.left {
//                 // println!(
//                 //     "FORMATTING: ({} {} {})",
//                 //     left.to_string(),
//                 //     self.operator,
//                 //     right.to_string()
//                 // );
//                 // println!("LEFT: {}", left.to_string());
//                 // println!("RIGHT: {}", right.to_string());
//                 return format!(
//                     "({} {} {})",
//                     left.to_string(),
//                     self.operator,
//                     right.to_string()
//                 );
//             }
//         }
//         unreachable!()
//     }
// }
//
// impl Expression for InfixExpression {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct IfExpression {
//     pub token: Token,
//     pub condition: Box<dyn Expression>,
//     pub consequence: BlockStatement,
//     pub alternative: Option<BlockStatement>,
// }
//
// impl IfExpression {
//     pub fn new(
//         token: Token,
//         condition: Box<dyn Expression>,
//         consequence: BlockStatement,
//         alternative: Option<BlockStatement>,
//     ) -> Self {
//         match token.token_type {
//             TokenType::If => (),
//             _ => panic!("Not a if operator"),
//         }
//         let operator = token.literal.clone();
//         Self {
//             token,
//             condition,
//             consequence,
//             alternative,
//         }
//     }
// }
//
// impl Node for IfExpression {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         // format!("{}", self.value)
//         match &self.alternative {
//             Some(alt) => format!(
//                 "IF {} {} ELSE {}",
//                 self.condition.to_string(),
//                 self.consequence.to_string(),
//                 alt.to_string()
//             ),
//             None => format!(
//                 "IF {} {}",
//                 self.condition.to_string(),
//                 self.consequence.to_string()
//             ),
//         }
//     }
// }
//
// impl Expression for IfExpression {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct BlockStatement {
//     pub token: Token,
//     pub statements: Vec<Box<dyn Statement>>,
// }
// impl BlockStatement {
//     pub fn new(token: Token, statements: Vec<Box<dyn Statement>>) -> Self {
//         Self { token, statements }
//     }
// }
//
// impl Node for BlockStatement {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         self.statements
//             .iter()
//             .map(|stmt| stmt.to_string())
//             .collect::<Vec<String>>()
//             .join(" ")
//     }
// }
//
// impl Statement for BlockStatement {
//     fn statement_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct FunctionLiteral {
//     pub token: Token,
//     pub parameters: Vec<Identifier>,
//     pub body: BlockStatement,
// }
//
// impl FunctionLiteral {
//     pub fn new(token: Token, parameters: Vec<Identifier>, body: BlockStatement) -> Self {
//         match token.token_type {
//             TokenType::Function => (),
//             _ => panic!("Not a function operator"),
//         }
//         Self {
//             token,
//             parameters,
//             body,
//         }
//     }
// }
//
// impl Node for FunctionLiteral {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         // format!("{}", self.value)
//         let mut params = Vec::new();
//         for p in &self.parameters {
//             params.push(p.to_string());
//         }
//         format!(
//             "{} ({}) -> {}",
//             self.token_literal(),
//             params.join(", "),
//             self.body.to_string()
//         )
//     }
// }
//
// impl Expression for FunctionLiteral {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
// pub struct CallExpression {
//     pub token: Token,
//     pub function: Box<dyn Expression>,
//     pub arguments: Vec<Box<dyn Expression>>,
// }
//
// impl CallExpression {
//     pub fn new(
//         token: Token,
//         function: Box<dyn Expression>,
//         arguments: Vec<Box<dyn Expression>>,
//     ) -> Self {
//         // match token.token_type {
//         //     TokenType::Function => (),
//         //     _ => panic!("Not a function operator"),
//         // }
//         Self {
//             token,
//             function,
//             arguments,
//         }
//     }
// }
//
// impl Node for CallExpression {
//     fn token_literal(&self) -> String {
//         self.token.literal.clone()
//     }
//     fn to_string(&self) -> String {
//         // format!("{}", self.value)
//         let mut args = Vec::new();
//         for arg in &self.arguments {
//             args.push(arg.to_string());
//         }
//         format!("{} ({})", self.function.to_string(), args.join(", "),)
//     }
// }
//
// impl Expression for CallExpression {
//     fn expression_node(&self) -> String {
//         self.to_string()
//     }
// }
//
