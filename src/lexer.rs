use crate::token::Token;

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
            None => Token::Eof,
            Some(value) => match value {
                // Operators
                '=' => {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::Eq
                    } else {
                        Token::Assign
                    }
                }
                '+' => Token::Plus,
                '-' => Token::Minus,
                '!' => {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::NotEq
                    } else {
                        Token::Bang
                    }
                }
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '<' => Token::LT,
                '>' => Token::GT,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '\x00' => Token::Eof,
                // Delimiters
                // Keywords
                token => {
                    if token.is_alphabetic() {
                        let literal = self.read_identifier();
                        println!("LITERAL: {}", literal);
                        self.reverse_read();
                        match literal.as_str() {
                            "FN" => Token::Function,
                            "LET" => Token::Let,
                            "TRUE" => Token::True,
                            "FALSE" => Token::False,
                            "IF" => Token::If,
                            "ELSE" => Token::Else,
                            "RETURN" => Token::Return,
                            _ => Token::Ident(literal),
                        }
                    } else if token.is_digit(10) {
                        let literal: usize = self.read_number();
                        println!("LITERAL: {}", literal);
                        self.reverse_read();
                        Token::Int(literal)
                    } else {
                        Token::Illegal
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
    use crate::token::Token;
    #[test]
    fn test_symbols() {
        let input = String::from("=+(){},;");
        let expected = [
            (Token::Assign, "="),
            (Token::Plus, "+"),
            (Token::LParen, "("),
            (Token::RParen, ")"),
            (Token::LBrace, "{"),
            (Token::RBrace, "}"),
            (Token::Comma, ","),
            (Token::Semicolon, ";"),
            (Token::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            // println!("{:?}", tok);
            assert_eq!(tok, test.0);
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
            (Token::Let, "LET"),
            (Token::Ident("FIVE".to_string()), "FIVE"),
            (Token::Assign, "="),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("TEN".to_string()), "TEN"),
            (Token::Assign, "="),
            (Token::Int(10), "10"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("ADD".to_string()), "ADD"),
            (Token::Assign, "="),
            (Token::Function, "FN"),
            (Token::LParen, "("),
            (Token::Ident("X".to_string()), "X"),
            (Token::Comma, ","),
            (Token::Ident("Y".to_string()), "Y"),
            (Token::RParen, ")"),
            (Token::LBrace, "{"),
            (Token::Ident("X".to_string()), "X"),
            (Token::Plus, "+"),
            (Token::Ident("Y".to_string()), "Y"),
            (Token::Semicolon, ";"),
            (Token::RBrace, "}"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("RESULT".to_string()), "RESULT"),
            (Token::Assign, "="),
            (Token::Ident("ADD".to_string()), "ADD"),
            (Token::LParen, "("),
            (Token::Ident("FIVE".to_string()), "FIVE"),
            (Token::Comma, ","),
            (Token::Ident("TEN".to_string()), "TEN"),
            (Token::RParen, ")"),
            (Token::Semicolon, ";"),
            (Token::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok, test.0);
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
            (Token::Let, "LET"),
            (Token::Ident("FIVE".to_string()), "FIVE"),
            (Token::Assign, "="),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("TEN".to_string()), "TEN"),
            (Token::Assign, "="),
            (Token::Int(10), "10"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("ADD".to_string()), "ADD"),
            (Token::Assign, "="),
            (Token::Function, "FN"),
            (Token::LParen, "("),
            (Token::Ident("X".to_string()), "X"),
            (Token::Comma, ","),
            (Token::Ident("Y".to_string()), "Y"),
            (Token::RParen, ")"),
            (Token::LBrace, "{"),
            (Token::Ident("X".to_string()), "X"),
            (Token::Plus, "+"),
            (Token::Ident("Y".to_string()), "Y"),
            (Token::Semicolon, ";"),
            (Token::RBrace, "}"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("RESULT".to_string()), "RESULT"),
            (Token::Assign, "="),
            (Token::Ident("ADD".to_string()), "ADD"),
            (Token::LParen, "("),
            (Token::Ident("FIVE".to_string()), "FIVE"),
            (Token::Comma, ","),
            (Token::Ident("TEN".to_string()), "TEN"),
            (Token::RParen, ")"),
            (Token::Semicolon, ";"),
            (Token::Bang, "!"),
            (Token::Minus, "-"),
            (Token::Slash, "/"),
            (Token::Asterisk, "*"),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::Int(5), "5"),
            (Token::LT, "<"),
            (Token::Int(10), "10"),
            (Token::GT, ">"),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::Eof, "\0"),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok, test.0);
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
            (Token::Let, "let"),
            (Token::Ident("FIVE".to_string()), "five"),
            (Token::Assign, "="),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::Let, "let"),
            (Token::Ident("TEN".to_string()), "ten"),
            (Token::Assign, "="),
            (Token::Int(10), "10"),
            (Token::Semicolon, ";"),
            (Token::Let, "let"),
            (Token::Ident("ADD".to_string()), "add"),
            (Token::Assign, "="),
            (Token::Function, "fn"),
            (Token::LParen, "("),
            (Token::Ident("X".to_string()), "x"),
            (Token::Comma, ","),
            (Token::Ident("Y".to_string()), "y"),
            (Token::RParen, ")"),
            (Token::LBrace, "{"),
            (Token::Ident("X".to_string()), "x"),
            (Token::Plus, "+"),
            (Token::Ident("Y".to_string()), "y"),
            (Token::Semicolon, ";"),
            (Token::RBrace, "}"),
            (Token::Semicolon, ";"),
            (Token::Let, "LET"),
            (Token::Ident("RESULT".to_string()), "result"),
            (Token::Assign, "="),
            (Token::Ident("ADD".to_string()), "add"),
            (Token::LParen, "("),
            (Token::Ident("FIVE".to_string()), "five"),
            (Token::Comma, ","),
            (Token::Ident("TEN".to_string()), "ten"),
            (Token::RParen, ")"),
            (Token::Semicolon, ";"),
            (Token::Bang, "!"),
            (Token::Minus, "-"),
            (Token::Slash, "/"),
            (Token::Asterisk, "*"),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::Int(5), "5"),
            (Token::LT, "<"),
            (Token::Int(10), "10"),
            (Token::GT, ">"),
            (Token::Int(5), "5"),
            (Token::Semicolon, ";"),
            (Token::If, "if"),
            (Token::LParen, "("),
            (Token::Int(5), "5"),
            (Token::LT, "<"),
            (Token::Int(10), "10"),
            (Token::RParen, ")"),
            (Token::LBrace, "{"),
            (Token::Return, "RETURN"),
            (Token::True, "TRUE"),
            (Token::Semicolon, ";"),
            (Token::RBrace, "}"),
            (Token::Else, "ELSE"),
            (Token::LBrace, "{"),
            (Token::Return, "RETURN"),
            (Token::False, "FALSE"),
            (Token::Semicolon, ";"),
            (Token::RBrace, "}"),
            (Token::Int(10), "10"),
            (Token::Eq, "=="),
            (Token::Int(10), "10"),
            (Token::Semicolon, ";"),
            (Token::Int(10), "10"),
            (Token::NotEq, "!="),
            (Token::Int(9), "9"),
            (Token::Semicolon, ";"),
            (Token::Eof, ""),
        ];
        let mut lexer = Lexer::new(input);
        for test in expected.iter() {
            let tok = lexer.next_token();
            println!("actual {:?} expected: {:?}", tok, test);
            assert_eq!(tok, test.0);
        }
    }
}
