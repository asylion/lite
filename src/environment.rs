use std::collections::HashMap;

use crate::value::Value;

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn put(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    pub fn get(&mut self, key: String) -> Option<&Value> {
        self.values.get(&key)
    }
}
