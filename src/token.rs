// pub type TokenType = String;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,
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
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn new(literal: String) -> Self {
        let token_type = match literal.as_str() {
            "=" => TokenType::Assign,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "!" => TokenType::Bang,
            "*" => TokenType::Asterisk,
            "/" => TokenType::Slash,
            "!=" => TokenType::NotEq,
            "==" => TokenType::Eq,
            "<" => TokenType::LT,
            ">" => TokenType::GT,
            // Delimiters
            "," => TokenType::Comma,
            ";" => TokenType::Semicolon,
            "(" => TokenType::LParen,
            ")" => TokenType::RParen,
            "{" => TokenType::LBrace,
            "}" => TokenType::RBrace,
            // Identifiers + literals
            // Keywords
            "FN" => TokenType::Function,
            "LET" => TokenType::Let,
            "TRUE" => TokenType::True,
            "FALSE" => TokenType::False,
            "IF" => TokenType::If,
            "ELSE" => TokenType::Else,
            "RETURN" => TokenType::Return,
            "\x00" => TokenType::Eof,
            token => {
                if Self::is_number(token) {
                    TokenType::Int
                } else if Self::is_word(token) {
                    TokenType::Ident
                } else {
                    println!("TOKEN: {}", token);
                    TokenType::Illegal
                }
                // if token.is_alphabetic() {
                //     Token::new(literal)
            }
        };
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
}
