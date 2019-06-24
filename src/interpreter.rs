use std::rc::Rc;

use crate::ast::*;
use crate::environment::Environment;
use crate::value::Value;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
        }
    }

    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> Value {
        match stmt {
            Stmt::Program(stmts) => self.evaluate_program(&stmts),
            Stmt::Block(stmts) => self.evaluate_block(&stmts),
            Stmt::ExprStmt(stmt) => self.evaluate_expr(&stmt.expr),
            Stmt::VarDecl(stmt) => self.evaluate_var_decl(&stmt),
            Stmt::Assign(stmt) => self.evaluate_assign(&stmt),
            Stmt::IfStmt(stmt) => self.evaluate_if_stmt(&stmt),
            Stmt::WhileStmt(stmt) => self.evaluate_while_stmt(&stmt),
            Stmt::BreakStmt => Value::Break,
            Stmt::ReturnStmt(stmt) => self.evaluate_return_stmt(&stmt),
            Stmt::FunctionDecl(decl) => self.evaluate_function_decl(&decl),
        }
    }

    fn evaluate_program(&mut self, stmts: &Vec<Stmt>) -> Value {
        let mut val = Value::Void;

        for stmt in stmts {
            val = self.evaluate_stmt(stmt);
            if let Value::Break = val {
                panic!("Break found outside of a loop");
            }
            if let Value::Return(..) = val {
                panic!("Return found outside of a function");
            }
        }

        val
    }

    fn evaluate_block(&mut self, stmts: &Vec<Stmt>) -> Value {
        let mut val = Value::Void;

        self.enter_scope();
        for stmt in stmts {
            val = self.evaluate_stmt(stmt);
            match val {
                Value::Break | Value::Return(..) => {
                    self.exit_scope();
                    return val;
                }
                _ => (),
            }
        }
        self.exit_scope();

        val
    }

    fn evaluate_var_decl(&mut self, decl: &VarDecl) -> Value {
        let value = match decl.initializer {
            Some(ref expr) => self.evaluate_expr(expr),
            None => Value::Void,
        };

        self.env.declare(decl.name.clone(), value.clone());

        Value::Void
    }

    fn evaluate_assign(&mut self, assign: &Assign) -> Value {
        let value = self.evaluate_expr(&assign.expr);

        if !self.env.update(assign.name.clone(), value.clone()) {
            panic!("Undeclared variable: {}", assign.name);
        }

        Value::Void
    }

    fn evaluate_if_stmt(&mut self, if_stmt: &IfStmt) -> Value {
        let cond = self.evaluate_expr(&if_stmt.cond);
        if let Value::Bool(true) = cond {
            return self.evaluate_stmt(&*if_stmt.cons);
        } else {
            if let Some(ref alt) = if_stmt.alt {
                return self.evaluate_stmt(&*alt);
            }
        }

        Value::Void
    }

    fn evaluate_while_stmt(&mut self, while_stmt: &WhileStmt) -> Value {
        let mut cond = self.evaluate_expr(&while_stmt.cond);

        while let Value::Bool(true) = cond {
            let val = self.evaluate_stmt(&*while_stmt.body);
            match val {
                Value::Break => break,
                Value::Return(..) => return val,
                _ => (),
            }

            cond = self.evaluate_expr(&while_stmt.cond);
        }

        Value::Void
    }

    fn evaluate_return_stmt(&mut self, return_stmt: &ReturnStmt) -> Value {
        let value = self.evaluate_expr(&return_stmt.expr);
        Value::Return(Box::new(value))
    }

    fn evaluate_function_decl(&mut self, decl: &FunctionDecl) -> Value {
        let function = Value::Function(
            decl.name.clone(),
            decl.params.clone(),
            Rc::clone(&decl.body),
        );

        self.env.declare(decl.name.clone(), function);

        Value::Void
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::LiteralBoolean(expr) => Value::Bool(expr.value),
            Expr::LiteralNumber(expr) => Value::Number(expr.value),
            Expr::LiteralString(expr) => Value::Str(expr.value.to_string()),
            Expr::Identifier(expr) => match self.env.get(&expr.name) {
                Some(value) => value.clone(),
                None => panic!("Undeclared identifier {}", expr.name),
            },
            Expr::UnaryExpr(expr) => self.evaluate_unary_expr(&expr.op, &*expr.expr),
            Expr::BinaryExpr(expr) => {
                self.evaluate_binary_expr(&*expr.left, &expr.op, &*expr.right)
            }
            Expr::FunctionCall(expr) => self.evaluate_function_call(&expr),
        }
    }

    fn evaluate_unary_expr(&mut self, op: &UnaryOp, expr: &Expr) -> Value {
        let value = self.evaluate_expr(expr);
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

    fn evaluate_binary_expr(&mut self, left: &Expr, op: &BinaryOp, right: &Expr) -> Value {
        let left = self.evaluate_expr(left);
        let right = self.evaluate_expr(right);
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

    fn evaluate_function_call(&mut self, call: &FunctionCall) -> Value {
        let decl = self.env.get(&call.name);
        let (params, body) = match decl {
            Some(Value::Function(.., params, body)) => (params.clone(), Rc::clone(body)),
            _ => panic!("Undeclared function {}", &call.name),
        };

        if call.args.len() != params.len() {
            panic!(
                "Expected {} args, but got {}",
                params.len(),
                call.args.len()
            )
        }

        self.enter_scope();
        for (i, param) in params.into_iter().enumerate() {
            let arg_value = self.evaluate_expr(&call.args[i]);
            self.env.declare(param, arg_value);
        }
        let result = self.evaluate_stmt(&body);
        self.exit_scope();

        self.unwrap_return(result)
    }

    fn unwrap_return(&self, result: Value) -> Value {
        match result {
            Value::Return(value) => *value,
            _ => result,
        }
    }

    fn enter_scope(&mut self) {
        let new_env = Environment::new();
        let outer_env = std::mem::replace(&mut self.env, new_env);
        self.env.outer_env = Some(Box::new(outer_env));
    }

    fn exit_scope(&mut self) {
        let outer_env = *self.env.outer_env.take().unwrap();
        std::mem::replace(&mut self.env, outer_env);
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
x
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
    x = 0
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

    #[test]
    fn test_scope() {
        let input = "
var x = 5
{
  var x = 123
  x = 100
}
x
";
        let expected = 5;

        let value = evaluate_input(input);
        assert!(
            num_value_match(&value, expected),
            "value: {:?} expected: {:?}",
            value,
            expected
        );
    }

    #[test]
    fn test_function() {
        let input = "
def twice(x) { return 2 * x }
twice(100)
";
        let expected = 200;

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
        let mut interpreter = Interpreter::new();
        interpreter.evaluate_stmt(&program)
    }
}
