use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Number(i64),
    Str(String),
    Bool(bool),
    Break,
    Return(Box<Value>),
    Function(String, Vec<String>, Rc<crate::ast::Stmt>),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(value) => write!(f, "{}", value),
            Value::Str(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Return(value) => write!(f, "{}", value),
            Value::Function(name, ..) => write!(f, "Function: {}", name),
            _ => write!(f, ""),
        }
    }
}
