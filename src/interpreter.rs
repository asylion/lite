use std::rc::Rc;

use crate::ast::*;
use crate::environment::{global_env, Environment};
use crate::value::Value;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = global_env();
        Interpreter { env: env }
    }

    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::Program(stmts) => self.evaluate_program(&stmts),
            Stmt::Block(stmts) => self.evaluate_block(&stmts),
            Stmt::ExprStmt(stmt) => self.evaluate_expr(&stmt.expr),
            Stmt::VarDecl(stmt) => self.evaluate_var_decl(&stmt),
            Stmt::Assign(stmt) => self.evaluate_assign(&stmt),
            Stmt::IfStmt(stmt) => self.evaluate_if_stmt(&stmt),
            Stmt::WhileStmt(stmt) => self.evaluate_while_stmt(&stmt),
            Stmt::BreakStmt => Ok(Value::Break),
            Stmt::ReturnStmt(stmt) => self.evaluate_return_stmt(&stmt),
            Stmt::FunctionDecl(decl) => self.evaluate_function_decl(&decl),
        }
    }

    fn evaluate_program(&mut self, stmts: &Vec<Stmt>) -> Result<Value, String> {
        let mut val = Value::Void;

        for stmt in stmts {
            val = self.evaluate_stmt(stmt)?;
            if let Value::Break = val {
                return Err("Break found outside of a loop".to_string());
            }
            if let Value::Return(..) = val {
                return Err("Return found outside of a function".to_string());
            }
        }

        Ok(val)
    }

    fn evaluate_block(&mut self, stmts: &Vec<Stmt>) -> Result<Value, String> {
        let mut val = Value::Void;

        self.enter_scope();
        for stmt in stmts {
            val = self.evaluate_stmt(stmt)?;
            match val {
                Value::Break | Value::Return(..) => {
                    self.exit_scope();
                    return Ok(val);
                }
                _ => (),
            }
        }
        self.exit_scope();

        Ok(val)
    }

    fn evaluate_var_decl(&mut self, decl: &VarDecl) -> Result<Value, String> {
        let value = match decl.initializer {
            Some(ref expr) => self.evaluate_expr(expr)?,
            None => Value::Void,
        };

        self.env.declare(decl.name.clone(), value.clone());

        Ok(Value::Void)
    }

    fn evaluate_assign(&mut self, assign: &Assign) -> Result<Value, String> {
        let value = self.evaluate_expr(&assign.expr)?;

        if !self.env.update(assign.name.clone(), value.clone()) {
            return Err(format!("Undeclared variable: {}", assign.name));
        }

        Ok(Value::Void)
    }

    fn evaluate_if_stmt(&mut self, if_stmt: &IfStmt) -> Result<Value, String> {
        let cond = self.evaluate_expr(&if_stmt.cond)?;
        if let Value::Bool(true) = cond {
            return self.evaluate_stmt(&*if_stmt.cons);
        } else {
            if let Some(ref alt) = if_stmt.alt {
                return self.evaluate_stmt(&*alt);
            }
        }

        Ok(Value::Void)
    }

    fn evaluate_while_stmt(&mut self, while_stmt: &WhileStmt) -> Result<Value, String> {
        let mut cond = self.evaluate_expr(&while_stmt.cond)?;

        while let Value::Bool(true) = cond {
            let val = self.evaluate_stmt(&*while_stmt.body)?;
            match val {
                Value::Break => break,
                Value::Return(..) => return Ok(val),
                _ => (),
            }

            cond = self.evaluate_expr(&while_stmt.cond)?;
        }

        Ok(Value::Void)
    }

    fn evaluate_return_stmt(&mut self, return_stmt: &ReturnStmt) -> Result<Value, String> {
        let value = self.evaluate_expr(&return_stmt.expr)?;
        Ok(Value::Return(Box::new(value)))
    }

    fn evaluate_function_decl(&mut self, decl: &FunctionDecl) -> Result<Value, String> {
        let function = Value::Function(
            decl.name.clone(),
            decl.params.clone(),
            Rc::clone(&decl.body),
        );

        self.env.declare(decl.name.clone(), function);

        Ok(Value::Void)
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::LiteralBoolean(expr) => Ok(Value::Bool(expr.value)),
            Expr::LiteralNumber(expr) => Ok(Value::Number(expr.value)),
            Expr::LiteralString(expr) => Ok(Value::Str(expr.value.to_string())),
            Expr::Identifier(expr) => match self.env.get(&expr.name) {
                Some(value) => Ok(value.clone()),
                None => Err(format!("Undeclared identifier {}", expr.name)),
            },
            Expr::UnaryExpr(expr) => self.evaluate_unary_expr(&expr.op, &*expr.expr),
            Expr::BinaryExpr(expr) => {
                self.evaluate_binary_expr(&*expr.left, &expr.op, &*expr.right)
            }
            Expr::FunctionCall(expr) => self.evaluate_function_call(&expr),
        }
    }

    fn evaluate_unary_expr(&mut self, op: &UnaryOp, expr: &Expr) -> Result<Value, String> {
        let value = self.evaluate_expr(expr)?;
        match value {
            Value::Bool(val) => self.evaluate_boolean_unary_expr(op, val),
            Value::Number(num) => self.evaluate_numeric_unary_expr(op, num),
            _ => Err(format!("Unary operator not supported for {}", value)),
        }
    }

    fn evaluate_boolean_unary_expr(&self, op: &UnaryOp, value: bool) -> Result<Value, String> {
        match op {
            UnaryOp::Not => Ok(Value::Bool(!value)),
            UnaryOp::Minus => Err(format!("Operator {:?} not supported for booleans", op)),
        }
    }

    fn evaluate_numeric_unary_expr(&self, op: &UnaryOp, value: i64) -> Result<Value, String> {
        match op {
            UnaryOp::Not => Err(format!("Operator {:?} not supported for numbers", op)),
            UnaryOp::Minus => Ok(Value::Number(-value)),
        }
    }

    fn evaluate_binary_expr(
        &mut self,
        left: &Expr,
        op: &BinaryOp,
        right: &Expr,
    ) -> Result<Value, String> {
        let left = self.evaluate_expr(left)?;
        let right = self.evaluate_expr(right)?;
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                self.evaluate_numeric_binary_expr(left, op, right)
            }
            (Value::Str(left), Value::Str(right)) => match op {
                BinaryOp::Plus => Ok(Value::Str(left + &right)),
                _ => Err(format!(
                    "Unsupported binary operator {:?} for strings {}",
                    op, left
                )),
            },
            (Value::Bool(left), Value::Bool(right)) => {
                self.evaluate_boolean_binary_expr(left, op, right)
            }
            (left, right) => Err(format!("Type mismatch between {} and {}", left, right)),
        }
    }

    fn evaluate_numeric_binary_expr(
        &self,
        left: i64,
        op: &BinaryOp,
        right: i64,
    ) -> Result<Value, String> {
        match op {
            BinaryOp::Plus => Ok(Value::Number(left + right)),
            BinaryOp::Minus => Ok(Value::Number(left - right)),
            BinaryOp::Multiply => Ok(Value::Number(left * right)),
            BinaryOp::Divide => Ok(Value::Number(left / right)),
            BinaryOp::Mod => Ok(Value::Number(left % right)),
            BinaryOp::Eq => Ok(Value::Bool(left == right)),
            BinaryOp::Neq => Ok(Value::Bool(left != right)),
            BinaryOp::Gt => Ok(Value::Bool(left > right)),
            BinaryOp::Lt => Ok(Value::Bool(left < right)),
            BinaryOp::Geq => Ok(Value::Bool(left >= right)),
            BinaryOp::Leq => Ok(Value::Bool(left <= right)),
            _ => Err(format!("Unsupported binary operator {:?} for numbers", op)),
        }
    }

    fn evaluate_boolean_binary_expr(
        &self,
        left: bool,
        op: &BinaryOp,
        right: bool,
    ) -> Result<Value, String> {
        match op {
            BinaryOp::And => Ok(Value::Bool(left && right)),
            BinaryOp::Or => Ok(Value::Bool(left || right)),
            BinaryOp::Eq => Ok(Value::Bool(left == right)),
            BinaryOp::Neq => Ok(Value::Bool(left != right)),
            _ => Err(format!("Unsupported binary operator {:?} for booleans", op)),
        }
    }

    fn evaluate_function_call(&mut self, call: &FunctionCall) -> Result<Value, String> {
        let decl = self.env.get(&call.name);

        let (params, body) = match decl {
            Some(Value::Function(.., params, body)) => (params.clone(), Rc::clone(body)),
            Some(Value::BuiltinFunction0(.., func)) => return Ok(func()),
            Some(Value::BuiltinFunction1(name, func)) => {
                if call.args.len() != 1 {
                    return Err(format!("Function {} expects 1 argument", name));
                }
                return Ok(func(self.evaluate_expr(&call.args[0])?));
            }
            _ => return Err(format!("Undeclared function {}", &call.name)),
        };

        if call.args.len() != params.len() {
            return Err(format!(
                "Expected {} args, but got {}",
                params.len(),
                call.args.len()
            ));
        }

        self.enter_scope();
        for (i, param) in params.into_iter().enumerate() {
            let arg_value = self.evaluate_expr(&call.args[i])?;
            self.env.declare(param, arg_value);
        }
        let result = self.evaluate_stmt(&body)?;
        self.exit_scope();

        Ok(self.unwrap_return(result))
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
        let numeric_cases = vec![
            ("1 + 1", 2),
            ("1 - 1", 0),
            ("2 * 2", 4),
            ("10 / 2", 5),
            ("7 % 3", 1),
        ];

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
        let program = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.evaluate_stmt(&program).unwrap()
    }
}
