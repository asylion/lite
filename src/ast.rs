use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Expr {
    LiteralBoolean(LiteralBoolean),
    LiteralNumber(LiteralNumber),
    LiteralString(LiteralString),
    Identifier(Identifier),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
    FunctionCall(FunctionCall),
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Program(Vec<Stmt>),
    Block(Vec<Stmt>),
    ExprStmt(ExprStmt),
    VarDecl(VarDecl),
    Assign(Assign),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    BreakStmt,
    ReturnStmt(ReturnStmt),
    FunctionDecl(FunctionDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub left: Rc<Expr>,
    pub op: BinaryOp,
    pub right: Rc<Expr>,
}

#[derive(Clone, Debug)]
pub struct LiteralBoolean {
    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct LiteralNumber {
    pub value: i64,
}

#[derive(Clone, Debug)]
pub struct LiteralString {
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub name: String,
    pub initializer: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Assign {
    pub name: String,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub cons: Rc<Stmt>,
    pub alt: Option<Rc<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Rc<Stmt>,
}

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Rc<Stmt>,
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    pub expr: Expr,
}
