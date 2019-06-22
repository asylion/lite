use crate::ast::*;
use crate::environment::Environment;
use crate::value::Value;

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate_stmt(&self, stmt: &Stmt, env: &mut Environment) -> Value {
        match stmt {
            Stmt::Program(stmts) => self.evaluate_program(&stmts, env),
            Stmt::Block(stmts) => self.evaluate_block(&stmts, env),
            Stmt::ExprStmt(stmt) => self.evaluate_expr(&stmt.expr, env),
            Stmt::VarDecl(stmt) => self.evaluate_var_decl(&stmt, env),
            Stmt::Assign(stmt) => self.evaluate_assign(&stmt, env),
            Stmt::IfStmt(stmt) => self.evaluate_if_stmt(&stmt, env),
            Stmt::WhileStmt(stmt) => self.evaluate_while_stmt(&stmt, env),
            Stmt::BreakStmt => Value::Break,
        }
    }

    fn evaluate_program(&self, stmts: &Vec<Stmt>, env: &mut Environment) -> Value {
        let mut val = Value::Void;

        for stmt in stmts {
            val = self.evaluate_stmt(stmt, env);
            if let Value::Break = val {
                panic!("Break found outside of a loop");
            }
        }

        val
    }

    fn evaluate_block(&self, stmts: &Vec<Stmt>, env: &mut Environment) -> Value {
        let mut val = Value::Void;

        for stmt in stmts {
            val = self.evaluate_stmt(stmt, env);
        }

        val
    }

    fn evaluate_var_decl(&self, decl: &VarDecl, env: &mut Environment) -> Value {
        let value = match decl.initializer {
            Some(ref expr) => self.evaluate_expr(expr, env),
            None => Value::Void,
        };

        env.put(decl.name.clone(), value.clone());

        value
    }

    fn evaluate_assign(&self, assign: &Assign, env: &mut Environment) -> Value {
        let value = self.evaluate_expr(&assign.expr, env);

        env.put(assign.name.clone(), value.clone());

        value
    }

    fn evaluate_if_stmt(&self, if_stmt: &IfStmt, env: &mut Environment) -> Value {
        let cond = self.evaluate_expr(&if_stmt.cond, env);
        if let Value::Bool(true) = cond {
            return self.evaluate_stmt(&*if_stmt.cons, env);
        } else {
            if let Some(ref alt) = if_stmt.alt {
                return self.evaluate_stmt(&*alt, env);
            }
        }

        Value::Void
    }

    fn evaluate_while_stmt(&self, while_stmt: &WhileStmt, env: &mut Environment) -> Value {
        let mut cond = self.evaluate_expr(&while_stmt.cond, env);

        while let Value::Bool(true) = cond {
            if let Value::Break = self.evaluate_stmt(&*while_stmt.body, env) {
                break;
            }
            cond = self.evaluate_expr(&while_stmt.cond, env);
        }

        Value::Void
    }

    fn evaluate_expr(&self, expr: &Expr, env: &mut Environment) -> Value {
        match expr {
            Expr::LiteralBoolean(expr) => Value::Bool(expr.value),
            Expr::LiteralNumber(expr) => Value::Number(expr.value),
            Expr::LiteralString(expr) => Value::Str(expr.value.to_string()),
            Expr::Identifier(expr) => match env.get(&expr.name) {
                Some(value) => value.clone(),
                None => panic!("Undeclared identifier {}", expr.name),
            },
            Expr::UnaryExpr(expr) => self.evaluate_unary_expr(&expr.op, &*expr.expr, env),
            Expr::BinaryExpr(expr) => {
                self.evaluate_binary_expr(&*expr.left, &expr.op, &*expr.right, env)
            }
        }
    }

    fn evaluate_unary_expr(&self, op: &UnaryOp, expr: &Expr, env: &mut Environment) -> Value {
        let value = self.evaluate_expr(expr, env);
        match value {
            Value::Bool(val) => self.evaluate_boolean_unary_expr(op, val),
            Value::Number(num) => self.evaluate_numeric_unary_expr(op, num),
            _ => panic!("Unary operator not supported for {}", value),
        }
    }

    fn evaluate_boolean_unary_expr(&self, op: &UnaryOp, value: bool) -> Value {
        match op {
            UnaryOp::Not => Value::Bool(!value),
            UnaryOp::Minus => panic!("Operator {:?} not supported for booleans", op),
        }
    }

    fn evaluate_numeric_unary_expr(&self, op: &UnaryOp, value: i64) -> Value {
        match op {
            UnaryOp::Not => panic!("Operator {:?} not supported for numbers", op),
            UnaryOp::Minus => Value::Number(-value),
        }
    }

    fn evaluate_binary_expr(
        &self,
        left: &Expr,
        op: &BinaryOp,
        right: &Expr,
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
                _ => panic!("Unsupported binary operator {:?} for strings {}", op, left),
            },
            (Value::Bool(left), Value::Bool(right)) => {
                self.evaluate_boolean_binary_expr(left, op, right)
            }
            (left, right) => panic!("Type mismatch between {} and {}", left, right),
        }
    }

    fn evaluate_numeric_binary_expr(&self, left: i64, op: &BinaryOp, right: i64) -> Value {
        match op {
            BinaryOp::Plus => Value::Number(left + right),
            BinaryOp::Minus => Value::Number(left - right),
            BinaryOp::Multiply => Value::Number(left * right),
            BinaryOp::Divide => Value::Number(left / right),
            BinaryOp::Eq => Value::Bool(left == right),
            BinaryOp::Neq => Value::Bool(left != right),
            BinaryOp::Gt => Value::Bool(left > right),
            BinaryOp::Lt => Value::Bool(left < right),
            BinaryOp::Geq => Value::Bool(left >= right),
            BinaryOp::Leq => Value::Bool(left <= right),
            _ => panic!("Unsupported binary operator {:?} for numbers", op),
        }
    }

    fn evaluate_boolean_binary_expr(&self, left: bool, op: &BinaryOp, right: bool) -> Value {
        match op {
            BinaryOp::And => Value::Bool(left && right),
            BinaryOp::Or => Value::Bool(left || right),
            BinaryOp::Eq => Value::Bool(left == right),
            BinaryOp::Neq => Value::Bool(left != right),
            _ => panic!("Unsupported binary operator {:?} for booleans", op),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_evaluate_expr_stmt() {
        let input = "\"Hello world\"";
        let expected = "Hello world";

        let value = evaluate_input(input);
        assert!(
            str_value_match(&value, expected),
            "value: {:?} expected: {:?}",
            value,
            expected
        );
    }

    #[test]
    fn test_evaluate_var_decl() {
        let input = r#"
var x = 5
x = x + 1
"#;
        let expected = 6;

        let value = evaluate_input(input);
        assert!(
            num_value_match(&value, expected),
            "evaluated: {:?} expected: {:?}",
            value,
            expected
        );
    }

    #[test]
    fn test_evaluate_boolean_unary_expr() {
        let cases = vec![("!false", true), ("!true", false)];

        for (i, (input, expected)) in cases.into_iter().enumerate() {
            let value = evaluate_input(input);
            assert!(
                bool_value_match(&value, expected),
                "[{}] - evaluated: {:?} expected: {:?}",
                i,
                value,
                expected
            );
        }
    }

    #[test]
    fn test_evaluate_numeric_unary_expr() {
        let cases = vec![("-1", -1)];

        for (i, (input, expected)) in cases.into_iter().enumerate() {
            let value = evaluate_input(input);
            assert!(
                num_value_match(&value, expected),
                "[{}] - evaluated: {:?} expected: {:?}",
                i,
                value,
                expected
            );
        }
    }

    #[test]
    fn test_evaluate_numeric_binary_expr() {
        let numeric_cases = vec![("1 + 1", 2), ("1 - 1", 0), ("2 * 2", 4), ("10 / 2", 5)];

        for (i, (input, expected)) in numeric_cases.into_iter().enumerate() {
            let value = evaluate_input(input);
            assert!(
                num_value_match(&value, expected),
                "[{}] - evaluated: {:?} expected: {:?}",
                i,
                value,
                expected
            );
        }

        let boolean_cases = vec![
            ("1 == 1", true),
            ("2 != 1", true),
            ("2 > 2", false),
            ("10 < 2", false),
            ("10 >= 10", true),
            ("1 <= 2", true),
        ];

        for (i, (input, expected)) in boolean_cases.into_iter().enumerate() {
            let value = evaluate_input(input);
            assert!(
                bool_value_match(&value, expected),
                "[{}] - evaluated: {:?} expected: {:?}",
                i,
                value,
                expected
            );
        }
    }

    #[test]
    fn test_evaluate_boolean_binary_expr() {
        let cases = vec![
            ("true && false", false),
            ("false || true", true),
            ("(3 < 4) == true", true),
            ("false != (5 > 5)", false),
        ];

        for (i, (input, expected)) in cases.into_iter().enumerate() {
            let value = evaluate_input(input);
            assert!(
                bool_value_match(&value, expected),
                "[{}] - evaluated: {:?} expected: {:?}",
                i,
                value,
                expected
            );
        }
    }

    #[test]
    fn test_if_stmt() {
        let input = "
var x = 5
if 2 > 1 {
  x = x + 5
} else {
  x = 0
}
x
";
        let expected = 10;

        let value = evaluate_input(input);
        assert!(
            num_value_match(&value, expected),
            "value: {:?} expected: {:?}",
            value,
            expected
        );
    }

    #[test]
    fn test_while_stmt() {
        let input = "
var x = 5
while x < 1000 {
  x = x + 1
  if x >= 500 {
    break
  }
}
x
";
        let expected = 500;

        let value = evaluate_input(input);
        assert!(
            num_value_match(&value, expected),
            "value: {:?} expected: {:?}",
            value,
            expected
        );
    }

    fn str_value_match(value: &Value, expected: &str) -> bool {
        match value {
            Value::Str(value) => value == expected,
            _ => false,
        }
    }

    fn num_value_match(value: &Value, expected: i64) -> bool {
        match value {
            Value::Number(value) => *value == expected,
            _ => false,
        }
    }

    fn bool_value_match(value: &Value, expected: bool) -> bool {
        match value {
            Value::Bool(value) => *value == expected,
            _ => false,
        }
    }

    fn evaluate_input(input: &str) -> Value {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let interpreter = Interpreter;
        let mut env = Environment::new();
        interpreter.evaluate_stmt(&program, &mut env)
    }
}
