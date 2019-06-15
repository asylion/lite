use crate::ast::*;
use crate::value::Value;

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate_program(&self, program: Stmt) -> Value {
        match program {
            Stmt::Program(stmts) => self.evaluate_block(stmts),
            _ => panic!("Expected a Program, but got {:?}", program),
        }
    }

    fn evaluate_block(&self, stmts: Vec<Stmt>) -> Value {
        let mut val = Value::Void;

        for stmt in stmts {
            val = self.evaluate_stmt(stmt);
        }

        val
    }

    fn evaluate_stmt(&self, stmt: Stmt) -> Value {
        match stmt {
            Stmt::ExprStmt(stmt) => self.evaluate_expr(stmt.expr),
            _ => unimplemented!(),
        }
    }

    fn evaluate_expr(&self, expr: Expr) -> Value {
        match expr {
            Expr::LiteralNumber(expr) => Value::Number(expr.value),
            Expr::BinaryExpr(expr) => {
                self.evaluate_binary_expr(*expr.left, expr.op, *expr.right)
            },
        }
    }

    fn evaluate_binary_expr(&self, left: Expr, op: BinaryOp, right: Expr) -> Value {
        let left = self.evaluate_expr(left);
        let right = self.evaluate_expr(right);
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.evaluate_numeric_binary_expr(left, op, right)
            },
            _ => unimplemented!(),
        }
    }

    fn evaluate_numeric_binary_expr(
        &self,
        left: i64,
        op: BinaryOp,
        right: i64,
    ) -> Value {
        let value = match op {
            BinaryOp::Plus => left + right,
            BinaryOp::Minus => left - right,
            BinaryOp::Multiply => left * right,
            BinaryOp::Divide => left / right,
        };
        Value::Number(value)
    }
}