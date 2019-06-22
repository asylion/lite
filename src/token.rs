#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number,
    Str,
    Ident,

    Plus,
    Minus,
    Multiply,
    Divide,

    And,
    Or,
    Not,

    Eq,
    Neq,

    Gt,
    Lt,
    Geq,
    Leq,

    LParen,
    RParen,

    LBrace,
    RBrace,

    Assign,

    Var,
    Val,

    True,
    False,

    If,
    Else,

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

pub fn identifier_kind(identifier: &str) -> TokenKind {
    match identifier {
        "var" => TokenKind::Var,
        "val" => TokenKind::Val,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        _ => TokenKind::Ident,
    }
}
