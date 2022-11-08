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