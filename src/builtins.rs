use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::value::Value;

pub fn functions() -> HashMap<String, Value> {
    let mut functions = HashMap::new();

    functions.insert(
        "println".to_string(),
        Value::BuiltinFunction1("println".to_string(), |x| {
            println!("{}", x);
            Value::Void
        }),
    );

    functions.insert(
        "time".to_string(),
        Value::BuiltinFunction0("time".to_string(), || {
            Value::Number(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as i64,
            )
        }),
    );

    functions
}
