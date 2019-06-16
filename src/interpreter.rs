use crate::ast::*;
use crate::environment::Environment;
use crate::value::Value;

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate_stmt(&self, stmt: Stmt, env: &mut Environment) -> Value {
        match stmt {
            Stmt::Program(stmts) => self.evaluate_block(stmts, env),
            Stmt::Block(stmts) => self.evaluate_block(stmts, env),
            Stmt::ExprStmt(stmt) => self.evaluate_expr(stmt.expr, env),
            Stmt::VarDecl(stmt) => self.evaluate_var_decl(stmt, env),
            Stmt::Assign(stmt) => self.evaluate_assign(stmt, env),
        }
    }

    fn evaluate_block(&self, stmts: Vec<Stmt>, env: &mut Environment) -> Value {
        let mut val = Value::Void;

        for stmt in stmts {
            val = self.evaluate_stmt(stmt, env);
        }

        val
    }

    fn evaluate_var_decl(&self, decl: VarDecl, env: &mut Environment) -> Value {
        let value = match decl.initializer {
            Some(expr) => self.evaluate_expr(expr, env),
            None => Value::Void,
        };

        env.put(decl.name, value.clone());

        value
    }

    fn evaluate_assign(&self, assign: Assign, env: &mut Environment) -> Value {
        let value = self.evaluate_expr(assign.expr, env);

        env.put(assign.name, value.clone());

        value
    }

    fn evaluate_expr(&self, expr: Expr, env: &mut Environment) -> Value {
        match expr {
            Expr::LiteralNumber(expr) => Value::Number(expr.value),
            Expr::LiteralString(expr) => Value::Str(expr.value),
            Expr::Identifier(expr) => match env.get(&expr.name) {
                Some(value) => value.clone(),
                None => panic!("Undeclared identifier {}", expr.name),
            },
            Expr::UnaryExpr(expr) => self.evaluate_unary_expr(expr.op, *expr.expr, env),
            Expr::BinaryExpr(expr) => {
                self.evaluate_binary_expr(*expr.left, expr.op, *expr.right, env)
            }
        }
    }

    fn evaluate_unary_expr(&self, op: UnaryOp, expr: Expr, env: &mut Environment) -> Value {
        let value = self.evaluate_expr(expr, env);
        match value {
            Value::Number(num) => self.evaluate_numeric_unary_expr(op, num),
            _ => panic!("Unary operator not supported for {:?}", value),
        }
    }

    fn evaluate_numeric_unary_expr(&self, op: UnaryOp, value: i64) -> Value {
        match op {
            UnaryOp::Minus => Value::Number(-value),
        }
    }

    fn evaluate_binary_expr(
        &self,
        left: Expr,
        op: BinaryOp,
        right: Expr,
        env: &mut Environment,
    ) -> Value {
        let left = self.evaluate_expr(left, env);
        let right = self.evaluate_expr(right, env);
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.evaluate_numeric_binary_expr(left, op, right)
            }
            (Value::Str(left), Value::Str(right)) => match op {
                BinaryOp::Plus => Value::Str(left + &right),
                _ => panic!("Unsupported operator {:?} for string", op),
            },
            _ => unimplemented!(),
        }
    }

    fn evaluate_numeric_binary_expr(&self, left: i64, op: BinaryOp, right: i64) -> Value {
        let value = match op {
            BinaryOp::Plus => left + right,
            BinaryOp::Minus => left - right,
            BinaryOp::Multiply => left * right,
            BinaryOp::Divide => left / right,
        };
        Value::Number(value)
    }
}
