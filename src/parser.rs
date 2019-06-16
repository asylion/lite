use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let current_token = lexer.next_token();
        let next_token = lexer.next_token();

        Parser {
            lexer: lexer,
            current_token: current_token,
            next_token: next_token,
        }
    }

    pub fn parse_program(&mut self) -> Stmt {
        let mut stmts = Vec::new();

        while self.current().kind != TokenKind::Eof {
            stmts.push(self.parse_stmt());
        }

        Stmt::Program(stmts)
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.current().kind {
            TokenKind::Var | TokenKind::Val => self.parse_var_decl(),
            TokenKind::Ident => {
                if self.peek().kind == TokenKind::Assign {
                    self.parse_assign()
                } else {
                    self.parse_expr_stmt()
                }
            }
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_var_decl(&mut self) -> Stmt {
        let is_constant = self.current().kind == TokenKind::Val;
        self.consume();

        let name = self.current().value.clone();
        self.expect(TokenKind::Ident);

        if is_constant && self.current().kind != TokenKind::Assign {
            panic!("Unassigned val {}", name);
        }

        let initializer = match self.current().kind {
            TokenKind::Assign => {
                self.consume();
                Some(self.parse_expr())
            }
            _ => None,
        };

        Stmt::VarDecl(VarDecl {
            name: name,
            initializer: initializer,
            is_constant: is_constant,
        })
    }

    fn parse_assign(&mut self) -> Stmt {
        let name = self.current().value.clone();
        self.expect(TokenKind::Ident);

        self.expect(TokenKind::Assign);

        let expr = self.parse_expr();

        Stmt::Assign(Assign {
            name: name,
            expr: expr,
        })
    }

    fn parse_expr_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr();
        Stmt::ExprStmt(ExprStmt { expr: expr })
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_addition()
    }

    fn parse_addition(&mut self) -> Expr {
        let mut expr = self.parse_multiplication();

        loop {
            let op = match self.current().kind {
                TokenKind::Plus => BinaryOp::Plus,
                TokenKind::Minus => BinaryOp::Minus,
                _ => return expr,
            };
            self.consume();

            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                op: op,
                right: Box::new(self.parse_multiplication()),
            });
        }
    }

    fn parse_multiplication(&mut self) -> Expr {
        let mut expr = self.parse_unary();

        loop {
            let op = match self.current().kind {
                TokenKind::Multiply => BinaryOp::Multiply,
                TokenKind::Divide => BinaryOp::Divide,
                _ => return expr,
            };
            self.consume();

            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                op: op,
                right: Box::new(self.parse_unary()),
            });
        }
    }

    fn parse_unary(&mut self) -> Expr {
        let op = match self.current().kind {
            TokenKind::Minus => UnaryOp::Minus,
            _ => return self.parse_term(),
        };
        self.consume();

        Expr::UnaryExpr(UnaryExpr {
            op: op,
            expr: Box::new(self.parse_unary()),
        })
    }

    fn parse_term(&mut self) -> Expr {
        match self.current().kind {
            TokenKind::Number => self.parse_literal_number(),
            TokenKind::Str => self.parse_literal_string(),
            TokenKind::Ident => self.parse_identifier(),
            TokenKind::LParen => {
                self.consume();
                let expr = self.parse_expr();
                self.expect(TokenKind::RParen);
                expr
            }
            _ => panic!("Unexpected token: {:?}", self.current()),
        }
    }

    fn parse_literal_number(&mut self) -> Expr {
        let value = self.current().value.clone();
        self.expect(TokenKind::Number);
        Expr::LiteralNumber(LiteralNumber {
            value: value.parse::<i64>().unwrap(),
        })
    }

    fn parse_literal_string(&mut self) -> Expr {
        let value = self.current().value.clone();
        self.expect(TokenKind::Str);
        Expr::LiteralString(LiteralString { value: value })
    }

    fn parse_identifier(&mut self) -> Expr {
        let name = self.current().value.clone();
        self.expect(TokenKind::Ident);
        Expr::Identifier(Identifier { name: name })
    }

    fn current(&self) -> &Token {
        &self.current_token
    }

    fn peek(&self) -> &Token {
        &self.next_token
    }

    fn consume(&mut self) {
        let current_token = std::mem::replace(&mut self.next_token, self.lexer.next_token());
        self.current_token = current_token;
    }

    fn expect(&mut self, expected: TokenKind) {
        if self.current().kind != expected {
            panic!("Expected {:?} but got {:?}", expected, self.current().kind);
        }
        self.consume();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_literal_number() {
        let input = "1";
        let program = parse_program(input);

        let mut stmts = get_statements(program);

        if stmts.len() != 1 {
            panic!("Expected {} statements, but got {}", 1, stmts.len());
        }

        let expr_stmt = get_expr_stmt(stmts.pop().unwrap());
        test_literal_number(expr_stmt.expr, 1)
    }

    #[test]
    fn test_parse_unary_expr() {
        let tests = vec![("-1", UnaryOp::Minus, 1)];

        for (input, op, val) in tests {
            let program = parse_program(input);

            let mut stmts = get_statements(program);

            if stmts.len() != 1 {
                panic!("Expected {} statements, but got {}", 1, stmts.len());
            }

            let expr_stmt = get_expr_stmt(stmts.pop().unwrap());
            let unary_expr = get_unary_expr(expr_stmt.expr);

            if unary_expr.op != op {
                panic!("Expected operator {:?}, but got {:?}", op, unary_expr.op);
            }

            test_literal_number(*unary_expr.expr, val);
        }
    }

    #[test]
    fn test_parse_binary_expr() {
        let tests = vec![
            ("1 + 2", 1, BinaryOp::Plus, 2),
            ("1 - 2", 1, BinaryOp::Minus, 2),
            ("1 * 2", 1, BinaryOp::Multiply, 2),
            ("1 / 2", 1, BinaryOp::Divide, 2),
        ];

        for (input, left_val, op, right_val) in tests {
            let program = parse_program(input);

            let mut stmts = get_statements(program);

            if stmts.len() != 1 {
                panic!("Expected {} statements, but got {}", 1, stmts.len());
            }

            let expr_stmt = get_expr_stmt(stmts.pop().unwrap());
            let binary_expr = get_binary_expr(expr_stmt.expr);

            assert_eq!(
                binary_expr.op, op,
                "Expected operator {:?}, but got {:?}",
                op, binary_expr.op
            );

            test_literal_number(*binary_expr.left, left_val);
            test_literal_number(*binary_expr.right, right_val);
        }
    }

    #[test]
    fn test_parse_var_decl() {
        let input = "var x = 1";
        let program = parse_program(input);

        let mut stmts = get_statements(program);

        if stmts.len() != 1 {
            panic!("Expected {} statements, but got {}", 1, stmts.len());
        }

        let var_decl = get_var_decl(stmts.pop().unwrap());

        assert_eq!(var_decl.name, "x");
        assert_eq!(var_decl.is_constant, false, "Expected non constant");
        test_literal_number(var_decl.initializer.expect("Missing initializer"), 1);
    }

    #[test]
    fn test_parse_assign() {
        let input = "x = 1";
        let program = parse_program(input);

        let mut stmts = get_statements(program);

        if stmts.len() != 1 {
            panic!("Expected {} statements, but got {}", 1, stmts.len());
        }

        let assign = get_assign(stmts.pop().unwrap());

        assert_eq!(assign.name, "x");
        test_literal_number(assign.expr, 1);
    }

    fn get_statements(program: Stmt) -> Vec<Stmt> {
        match program {
            Stmt::Program(stmts) => stmts,
            _ => panic!("Expected a Program, but got {:?}", program),
        }
    }

    fn get_expr_stmt(stmt: Stmt) -> ExprStmt {
        match stmt {
            Stmt::ExprStmt(stmt) => stmt,
            _ => panic!("Expected an ExprStmt, but got {:?}", stmt),
        }
    }

    fn get_var_decl(stmt: Stmt) -> VarDecl {
        match stmt {
            Stmt::VarDecl(stmt) => stmt,
            _ => panic!("Expected a VarDecl, but got {:?}", stmt),
        }
    }

    fn get_assign(stmt: Stmt) -> Assign {
        match stmt {
            Stmt::Assign(stmt) => stmt,
            _ => panic!("Expected an Assign, but got {:?}", stmt),
        }
    }

    fn get_unary_expr(expr: Expr) -> UnaryExpr {
        match expr {
            Expr::UnaryExpr(expr) => expr,
            _ => panic!("Expected a UnaryExpr, but got {:?}", expr),
        }
    }

    fn get_binary_expr(expr: Expr) -> BinaryExpr {
        match expr {
            Expr::BinaryExpr(expr) => expr,
            _ => panic!("Expected a BinaryExpr, but got {:?}", expr),
        }
    }

    fn test_literal_number(expr: Expr, value: i64) {
        match expr {
            Expr::LiteralNumber(num) => {
                if num.value != value {
                    panic!("Expected {}, but got {}", value, num.value);
                }
            }
            _ => panic!("Expected a LiteralNumber, but got {:?}", expr),
        }
    }

    fn parse_program(input: &str) -> Stmt {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }
}
