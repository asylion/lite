use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::value::Value;

pub fn functions() -> HashMap<String, Value> {
    let mut functions = HashMap::new();

    functions.insert(
        "println".to_string(),
        Value::BuiltinFunction1("println".to_string(), |x| {
            println!("{}", x);
            Ok(Value::Void)
        }),
    );

    functions.insert(
        "time".to_string(),
        Value::BuiltinFunction0("time".to_string(), || {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_err(|err| err.to_string())
                .and_then(|duration| Ok(Value::Number(duration.as_secs() as i64)))
        }),
    );

    functions
}
