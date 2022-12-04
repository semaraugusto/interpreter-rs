use crate::object::*;
use crate::token::*;
use std::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct WrongTokenError {
    pub actual: Token,
    pub expected: TokenType,
}
impl WrongTokenError {
    pub fn new(actual: Token, expected: TokenType) -> Self {
        Self { actual, expected }
    }
}

impl fmt::Display for WrongTokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expected = self.expected;
        let actual = self.actual.clone();
        write!(
            f,
            "Expected next token to be {expected:?}, got '{actual}' instead"
        )
    }
}

impl Error for WrongTokenError {}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub msg: String,
}
impl ParseError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.msg)
    }
}

impl Error for ParseError {}

#[derive(Debug, Clone)]
pub struct TypeMismatch {
    pub actual: Object,
    pub operator: String,
    pub expected: Option<String>,
}
impl TypeMismatch {
    pub fn new(actual: Object, operator: String, expected: Option<String>) -> Self {
        Self {
            actual,
            operator,
            expected,
        }
    }
}

impl fmt::Display for TypeMismatch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.expected {
            Some(expected) => write!(
                f,
                "type mismatch: {} {} {}",
                expected,
                self.operator,
                self.actual.obj_type()
            ),
            None => write!(
                f,
                "type mismatch: {} {}",
                self.operator,
                self.actual.obj_type()
            ),
        }
    }
}

impl Error for TypeMismatch {}

#[derive(Debug, Clone)]
pub struct UnknownOperator {
    pub obj_type1: String,
    pub operator: String,
    pub obj_type2: Option<String>,
}
impl UnknownOperator {
    pub fn new(obj_type1: String, operator: String, obj_type2: Option<String>) -> Self {
        Self {
            obj_type1,
            operator,
            obj_type2,
        }
    }
}

impl fmt::Display for UnknownOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.obj_type2 {
            Some(obj_type2) => write!(
                f,
                "unknown operator: {} {} {}",
                obj_type2, self.operator, self.obj_type1
            ),
            None => write!(f, "unknown operator: {} {}", self.operator, self.obj_type1),
        }
    }
}

impl Error for UnknownOperator {}

#[derive(Debug, Clone)]
pub struct IdentifierNotFound {
    pub ident: String,
}
impl IdentifierNotFound {
    pub fn new(ident: String) -> Self {
        Self { ident }
    }
}

impl fmt::Display for IdentifierNotFound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "identifier not found: {}", self.ident)
    }
}

impl Error for IdentifierNotFound {}
