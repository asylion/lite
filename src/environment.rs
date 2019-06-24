use std::collections::HashMap;

use crate::value::Value;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
    pub outer_env: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            outer_env: None,
        }
    }

    pub fn declare(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    pub fn update(&mut self, key: String, value: Value) -> bool {
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            return true;
        }

        match self.outer_env {
            Some(ref mut outer_env) => outer_env.update(key, value),
            None => false,
        }
    }

    pub fn get(&mut self, key: &str) -> Option<&Value> {
        match self.values.get(key) {
            value @ Some(..) => value,
            None => match self.outer_env {
                Some(ref mut outer_env) => outer_env.get(key),
                None => None,
            },
        }
    }
}
