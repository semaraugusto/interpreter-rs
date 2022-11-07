// pub type Token = String;

// #[derive(Debug, PartialEq)]
// pub struct Token {
//     pub token_type: Token,
//     pub literal: String,
// }
//
#[derive(Debug, PartialEq)]
pub enum Token {
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
    Ident(String),
    Int(usize),
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
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
}
