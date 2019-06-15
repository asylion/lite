use std::str;

use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
}

impl Lexer {
    pub fn new<T: Into<Vec<u8>>>(input: T) -> Lexer {
        Lexer {
            input: input.into(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.current_char() {
            Some(ch) => match ch {
                '+' => Token::from_char(TokenKind::Plus, ch),
                '-' => Token::from_char(TokenKind::Minus, ch),
                '*' => Token::from_char(TokenKind::Multiply, ch),
                '/' => Token::from_char(TokenKind::Divide, ch),
                '(' => Token::from_char(TokenKind::LParen, ch),
                ')' => Token::from_char(TokenKind::RParen, ch),
                _ => {
                    if ch.is_ascii_digit() {
                        return self.number();
                    }
                    panic!("Unrecognized token: {:?}", ch)
                }
            },
            None => Token::from_char(TokenKind::Eof, ' '),
        };

        self.advance();
        tok
    }

    fn number(&mut self) -> Token {
        let start = self.position;
        let mut end = start;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                end += 1;
                self.advance();
            } else {
                break;
            }
        }
        let number = str::from_utf8(&self.input[start..end]).unwrap();
        Token::from_string(TokenKind::Number, number)
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(ch) = self.current_char() {
                if ch.is_ascii_whitespace() {
                    self.advance();
                    continue;
                }
            }
            break;
        }
    }

    fn current_char(&self) -> Option<char> {
        if self.position >= self.input.len() {
            None
        } else {
            Some(self.input[self.position] as char)
        }
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::TokenKind;

    #[test]
    fn test_next_token() {
        let input = "
+ - * /
123 + 4
()
";
        let mut lexer = Lexer::new(input);

        let expected = vec![
            (TokenKind::Plus, "+"),
            (TokenKind::Minus, "-"),
            (TokenKind::Multiply, "*"),
            (TokenKind::Divide, "/"),
            (TokenKind::Number, "123"),
            (TokenKind::Plus, "+"),
            (TokenKind::Number, "4"),
            (TokenKind::LParen, "("),
            (TokenKind::RParen, ")"),
        ];

        for (i, (kind, value)) in expected.into_iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(
                kind, token.kind,
                "[{}] - Expected token kind: {:?} but got: {:?}",
                i, kind, token.kind
            );
            assert_eq!(
                value, token.value,
                "[{}] - Expected token value: {:?} but got: {:?}",
                i, value, token.value
            );
        }
    }
}
