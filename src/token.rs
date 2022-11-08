#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    Eq,
    NotEq,

    // Keywords
    Ident,
    Int,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }
    pub fn is_number(literal: &str) -> bool {
        let numb: i64 = literal.parse().unwrap_or(-1);
        if numb == -1 {
            return false;
        }
        true
    }
    pub fn is_word(literal: &str) -> bool {
        for c in literal.chars() {
            if !c.is_alphabetic() {
                return false;
            }
        }
        true
    }
    pub fn is_type(self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }
}
