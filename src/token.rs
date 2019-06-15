#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number,

    Plus,
    Minus,
    Multiply,
    Divide,

    LParen,
    RParen,

    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

impl Token {
    pub fn from_string<T: Into<String>>(kind: TokenKind, value: T) -> Token {
        Token {
            kind: kind,
            value: value.into(),
        }
    }

    pub fn from_char(kind: TokenKind, value: char) -> Token {
        Token {
            kind: kind,
            value: value.to_string(),
        }
    }
}
