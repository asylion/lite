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
        match self.current() {
            _ => self.parse_expr_stmt(),
        }
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
        Expr::LiteralString(LiteralString {
            value: value,
        })
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

            if binary_expr.op != op {
                panic!("Expected operator {:?}, but got {:?}", op, binary_expr.op);
            }

            test_literal_number(*binary_expr.left, left_val);
            test_literal_number(*binary_expr.right, right_val);
        }
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
