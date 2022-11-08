use crate::token::Token;
use crate::token::TokenType;

pub struct Lexer {
    pub input: Vec<char>,
    pub position: usize,
    pub read_position: usize,
    pub ch: Option<char>,
}

impl Lexer {
    // add code here
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: None,
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
        while let Some(literal) = self.ch {
            if !literal.is_alphabetic() {
                break;
            }
            self.read_char();
        }
        let out: String = self.input[position..self.position].iter().collect();
        out.to_uppercase()
    }
    fn read_number(&mut self) -> usize {
        let position = self.position;
        // println!("read_number {}", self.ch);
        while let Some(chr) = self.ch {
            if !chr.is_digit(10) {
                break;
            }
            self.read_char();
        }
        let literal: String = self.input[position..self.position].iter().collect();
        literal
            .parse::<usize>()
            .expect("could not parse number (should never happen)")
    }

    fn read_char(&mut self) {
        // println!("CH: {}", self.ch);
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input[self.read_position]);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn reverse_read(&mut self) {
        // println!("CH: {}", self.ch);
        self.position -= 1;
        self.read_position -= 1;
        self.ch = Some(self.input[self.read_position]);
    }
    fn skip_whitespace(&mut self) {
        while let Some(chr) = self.ch {
            if !chr.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        // println!("next_token called");
        // let _zero: char = char::default();
        self.skip_whitespace();
        println!("CH: {:?}", self.ch);
        let token: Token = match self.ch {
            None => Token::new(TokenType::Eof, "".to_string()),
            Some(value) => match value {
                // Operators
                '=' => {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::new(TokenType::Eq, "==".to_string())
                    } else {
                        Token::new(TokenType::Assign, value.to_string())
                    }
                }
                '+' => Token::new(TokenType::Plus, value.to_string()),
                '-' => Token::new(TokenType::Minus, value.to_string()),
                '!' => {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::new(TokenType::NotEq, "!=".to_string())
                    } else {
                        Token::new(TokenType::Bang, value.to_string())
                    }
                }
                '*' => Token::new(TokenType::Asterisk, value.to_string()),
                '/' => Token::new(TokenType::Slash, value.to_string()),
                '<' => Token::new(TokenType::LT, value.to_string()),
                '>' => Token::new(TokenType::GT, value.to_string()),
                ',' => Token::new(TokenType::Comma, value.to_string()),
                ';' => Token::new(TokenType::Semicolon, value.to_string()),
                '(' => Token::new(TokenType::LParen, value.to_string()),
                ')' => Token::new(TokenType::RParen, value.to_string()),
                '{' => Token::new(TokenType::LBrace, value.to_string()),
                '}' => Token::new(TokenType::RBrace, value.to_string()),
                // Delimiters
                // Keywords
                token => {
                    if token.is_alphabetic() {
                        let literal = self.read_identifier();
                        println!("LITERAL: {}", literal);
                        self.reverse_read();
                        match literal.as_str() {
                            "FN" => Token::new(TokenType::Function, literal),
                            "LET" => Token::new(TokenType::Let, literal),
                            "TRUE" => Token::new(TokenType::True, literal),
                            "FALSE" => Token::new(TokenType::False, literal),
                            "IF" => Token::new(TokenType::If, literal),
                            "ELSE" => Token::new(TokenType::Else, literal),
                            "RETURN" => Token::new(TokenType::Return, literal),
                            _ => Token::new(TokenType::Ident, literal),
                        }
                    } else if token.is_digit(10) {
                        let literal: usize = self.read_number();
                        println!("LITERAL: {}", literal);
                        self.reverse_read();
                        Token::new(TokenType::Int, literal.to_string())
                    } else {
                        Token::new(TokenType::Illegal, token.to_string())
                    }
                }
            },
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
            assert_eq!(tok.literal, test.1);
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
            assert_eq!(tok.literal, test.1);
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
            (TokenType::Eof, ""),
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
            (TokenType::If, "IF"),
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
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok.token_type, test.0);
            assert_eq!(tok.literal, test.1);
        }
    }
}
