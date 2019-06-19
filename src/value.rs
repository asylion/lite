use std::fmt;

#[derive(Clone, Debug)]
pub enum Value {
    Number(i64),
    Str(String),
    Bool(bool),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(value) => write!(f, "{}", value),
            Value::Str(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Void => write!(f, ""),
        }
    }
}
