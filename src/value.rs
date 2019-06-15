use std::fmt;

#[derive(Debug)]
pub enum Value {
    Number(i64),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::Void => write!(f, ""),
        }
    }
}