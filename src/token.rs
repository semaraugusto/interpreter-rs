use std::fmt;
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
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
    pub fn from_symbol(literal: String) -> Self {
        match literal.as_str() {
            "=" => Token::new(TokenType::Assign, literal),
            "==" => Token::new(TokenType::Eq, literal),
            "+" => Token::new(TokenType::Plus, literal),
            "-" => Token::new(TokenType::Minus, literal),
            "!" => Token::new(TokenType::Bang, literal),
            "!=" => Token::new(TokenType::NotEq, literal),
            "*" => Token::new(TokenType::Asterisk, literal),
            "/" => Token::new(TokenType::Slash, literal),
            "<" => Token::new(TokenType::LT, literal),
            ">" => Token::new(TokenType::GT, literal),
            "," => Token::new(TokenType::Comma, literal),
            ";" => Token::new(TokenType::Semicolon, literal),
            "(" => Token::new(TokenType::LParen, literal),
            ")" => Token::new(TokenType::RParen, literal),
            "{" => Token::new(TokenType::LBrace, literal),
            "}" => Token::new(TokenType::RBrace, literal),
            _ => panic!("Invalid token"),
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
    // pub fn to_string(&self) -> String {
    //     format!("Token({:?}, {})", self.token_type, self.literal)
    // }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}
