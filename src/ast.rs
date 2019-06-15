#[derive(Debug)]
pub enum Expr {
    LiteralNumber(LiteralNumber),
    LiteralString(LiteralString),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
}

#[derive(Debug)]
pub enum Stmt {
    Program(Vec<Stmt>),
    Block(Vec<Stmt>),
    ExprStmt(ExprStmt),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct LiteralNumber {
    pub value: i64,
}

#[derive(Debug)]
pub struct LiteralString {
    pub value: String,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}
