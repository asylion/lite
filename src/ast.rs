#[derive(Debug)]
pub enum Expr {
    LiteralNumber(LiteralNumber),
    LiteralString(LiteralString),
    Identifier(Identifier),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
}

#[derive(Debug)]
pub enum Stmt {
    Program(Vec<Stmt>),
    Block(Vec<Stmt>),
    ExprStmt(ExprStmt),
    VarDecl(VarDecl),
    Assign(Assign),
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
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub initializer: Option<Expr>,
    pub is_constant: bool,
}

#[derive(Debug)]
pub struct Assign {
    pub name: String,
    pub expr: Expr,
}
