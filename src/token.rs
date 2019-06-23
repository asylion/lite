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

    True,
    False,

    If,
    Else,

    While,
    Break,

    Def,

    Comma,

    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

impl Token {
    pub fn from_string<T: Into<String>>(kind: TokenKind, value: T) -> Self {
        Token {
            kind: kind,
            value: value.into(),
        }
    }

    pub fn from_char(kind: TokenKind, value: char) -> Self {
        Token {
            kind: kind,
            value: value.to_string(),
        }
    }
}

pub fn identifier_kind(identifier: &str) -> TokenKind {
    match identifier {
        "var" => TokenKind::Var,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "while" => TokenKind::While,
        "break" => TokenKind::Break,
        "def" => TokenKind::Def,
        _ => TokenKind::Ident,
    }
}
