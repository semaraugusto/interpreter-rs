use crate::token::Token;
use crate::token::TokenType;

pub struct Lexer {
    pub input: Vec<char>,
    pub position: usize,
    pub read_position: usize,
    pub ch: char,
}

impl Lexer {
    // add code here
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: char::default(),
        };
        lexer.read_char();
        lexer
    }
    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            char::default()
        } else {
            self.input[self.read_position]
        }
    }
    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_alphabetic() {
            self.read_char();
        }
        let out: String = self.input[position..self.position].iter().collect();
        out.to_uppercase()
    }
    fn read_number(&mut self) -> String {
        let position = self.position;
        println!("read_number {}", self.ch);
        while self.ch.is_digit(10) {
            self.read_char();
        }
        let out: String = self.input[position..self.position].iter().collect();
        out
    }

    fn read_char(&mut self) {
        // println!("CH: {}", self.ch);
        if self.read_position >= self.input.len() {
            self.ch = char::default();
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn reverse_read(&mut self) {
        // println!("CH: {}", self.ch);
        self.position -= 1;
        self.read_position -= 1;
        self.ch = self.input[self.read_position];
    }
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        // println!("next_token called");
        // let _zero: char = char::default();
        self.skip_whitespace();
        println!("CH: {}", self.ch);
        let token: Token = match self.ch {
            // Operators
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(String::from("=="))
                } else {
                    Token::new(String::from(self.ch))
                }
            }
            '+' => Token::new(String::from(self.ch)),
            '-' => Token::new(String::from(self.ch)),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(String::from("!="))
                } else {
                    Token::new(String::from(self.ch))
                }
            }
            '*' => Token::new(String::from(self.ch)),
            '/' => Token::new(String::from(self.ch)),
            '<' => Token::new(String::from(self.ch)),
            '>' => Token::new(String::from(self.ch)),
            ',' => Token::new(String::from(self.ch)),
            '(' => Token::new(String::from(self.ch)),
            ')' => Token::new(String::from(self.ch)),
            '{' => Token::new(String::from(self.ch)),
            '}' => Token::new(String::from(self.ch)),
            ';' => Token::new(String::from(self.ch)),
            '\x00' => Token::new(String::from(self.ch)),
            // Delimiters
            // Keywords
            token => {
                if token.is_alphabetic() {
                    let literal = self.read_identifier();
                    println!("LITERAL: {}", literal);
                    self.reverse_read();
                    Token::new(literal)
                } else if token.is_digit(10) {
                    let literal: String = self.read_number();
                    println!("LITERAL: {}", literal);
                    self.reverse_read();
                    Token::new(literal)
                } else {
                    let literal = String::from("illegal");
                    Token::new(literal)
                }
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;
    #[test]
    fn test_symbols() {
        let input = String::from("=+(){},;");
        let expected = [
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::RBrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            // println!("{:?}", tok);
            assert_eq!(tok.token_type, test.0);
        }
    }

    #[test]
    fn test_source() {
        let input = String::from(
            r"let five = 5;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
        ",
        );
        let expected = [
            (TokenType::Let, "LET"),
            (TokenType::Ident, "FIVE"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "LET"),
            (TokenType::Ident, "TEN"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "LET"),
            (TokenType::Ident, "ADD"),
            (TokenType::Assign, "="),
            (TokenType::Function, "FN"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "X"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "Y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "X"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "Y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "LET"),
            (TokenType::Ident, "RESULT"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "ADD"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "FIVE"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "TEN"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok.token_type, test.0);
            assert_eq!(tok.token_type, test.0);
        }
    }

    #[test]
    fn test_source2() {
        let input = String::from(
            "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;
",
        );
        let expected = [
            (TokenType::Let, "LET"),
            (TokenType::Ident, "FIVE"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "LET"),
            (TokenType::Ident, "TEN"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "LET"),
            (TokenType::Ident, "ADD"),
            (TokenType::Assign, "="),
            (TokenType::Function, "FN"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "X"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "Y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "X"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "Y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "LET"),
            (TokenType::Ident, "RESULT"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "ADD"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "FIVE"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "TEN"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, "\0"),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok.token_type, test.0);
            assert_eq!(tok.literal, test.1);
        }
    }
    #[test]
    fn test_source3() {
        let input = String::from(
            "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;",
        );
        let expected = [
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::GT, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::LT, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "RETURN"),
            (TokenType::True, "TRUE"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "ELSE"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "RETURN"),
            (TokenType::False, "FALSE"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok.token_type, test.0);
        }
    }
}
