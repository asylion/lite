use std::str;

use crate::token::{self, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
}

impl Lexer {
    pub fn new<T: Into<Vec<u8>>>(input: T) -> Self {
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
                '{' => Token::from_char(TokenKind::LBrace, ch),
                '}' => Token::from_char(TokenKind::RBrace, ch),
                ',' => Token::from_char(TokenKind::Comma, ch),
                '=' => {
                    return self.single_or_double_char_token(
                        '=',
                        '=',
                        TokenKind::Assign,
                        TokenKind::Eq,
                    );
                }
                '&' => return self.double_char_token(ch, TokenKind::And),
                '|' => return self.double_char_token(ch, TokenKind::Or),
                '!' => {
                    return self.single_or_double_char_token(
                        '!',
                        '=',
                        TokenKind::Not,
                        TokenKind::Neq,
                    );
                }
                '>' => {
                    return self.single_or_double_char_token(
                        '>',
                        '=',
                        TokenKind::Gt,
                        TokenKind::Geq,
                    );
                }
                '<' => {
                    return self.single_or_double_char_token(
                        '<',
                        '=',
                        TokenKind::Lt,
                        TokenKind::Leq,
                    );
                }
                '"' => return self.string(),
                _ => {
                    if ch.is_ascii_digit() {
                        return self.number();
                    }
                    if ch.is_ascii_alphabetic() {
                        return self.identifier();
                    }
                    self.unrecognized(ch)
                }
            },
            None => Token::from_char(TokenKind::Eof, ' '),
        };

        self.advance();
        tok
    }

    fn single_or_double_char_token(
        &mut self,
        first_char: char,
        second_char: char,
        single_kind: TokenKind,
        double_kind: TokenKind,
    ) -> Token {
        let mut value = String::new();
        value.push(first_char);
        self.advance();

        if let Some(ch) = self.current_char() {
            if ch == second_char {
                value.push(ch);
                self.advance();
                return Token::from_string(double_kind, value);
            }
        }
        Token::from_string(single_kind, value)
    }

    fn double_char_token(&mut self, ch: char, kind: TokenKind) -> Token {
        let mut value = String::new();
        value.push(ch);
        self.advance();

        if let Some(second_char) = self.current_char() {
            if second_char == ch {
                value.push(ch);
                self.advance();
                return Token::from_string(kind, value);
            }
            self.unrecognized(second_char)
        }
        self.unrecognized(ch)
    }

    fn string(&mut self) -> Token {
        self.advance(); // Skip opening "

        let start = self.position;
        let mut end = start;
        while let Some(ch) = self.current_char() {
            self.advance();

            if ch == '"' {
                let string = str::from_utf8(&self.input[start..end]).unwrap();
                return Token::from_string(TokenKind::Str, string);
            }

            end += 1;
        }

        panic!("Unterminated string");
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

    fn identifier(&mut self) -> Token {
        let start = self.position;
        let mut end = start;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphabetic() {
                end += 1;
                self.advance();
            } else {
                break;
            }
        }
        let number = str::from_utf8(&self.input[start..end]).unwrap();
        Token::from_string(token::identifier_kind(number), number)
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

    fn unrecognized(&self, ch: char) -> ! {
        panic!("Unrecognized token: {:?}", ch)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::TokenKind;

    #[test]
    fn test_next_token() {
        let input = r#"
+ - * /
123 + 4
()
"hello"
var x = 5
&& || ! == != >< >= <=
true false
if else {
}
while
break
def ,
"#;
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
            (TokenKind::Str, "hello"),
            (TokenKind::Var, "var"),
            (TokenKind::Ident, "x"),
            (TokenKind::Assign, "="),
            (TokenKind::Number, "5"),
            (TokenKind::And, "&&"),
            (TokenKind::Or, "||"),
            (TokenKind::Not, "!"),
            (TokenKind::Eq, "=="),
            (TokenKind::Neq, "!="),
            (TokenKind::Gt, ">"),
            (TokenKind::Lt, "<"),
            (TokenKind::Geq, ">="),
            (TokenKind::Leq, "<="),
            (TokenKind::True, "true"),
            (TokenKind::False, "false"),
            (TokenKind::If, "if"),
            (TokenKind::Else, "else"),
            (TokenKind::LBrace, "{"),
            (TokenKind::RBrace, "}"),
            (TokenKind::While, "while"),
            (TokenKind::Break, "break"),
            (TokenKind::Def, "def"),
            (TokenKind::Comma, ","),
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
