#[derive(Debug)]
pub enum Expr {
    LiteralNumber(LiteralNumber),
    BinaryExpr(BinaryExpr),
}

#[derive(Debug)]
pub enum Stmt {
    Program(Vec<Stmt>),
    Block(Vec<Stmt>),
    ExprStmt(ExprStmt),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
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
pub struct ExprStmt {
    pub expr: Expr,
}