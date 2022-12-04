use crate::object::Object;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(_outer: Option<Environment>) -> Self {
        let outer: Option<Box<Environment>>;
        if let Some(_out) = _outer {
            outer = Some(Box::new(_out));
        } else {
            outer = None;
        }
        Self {
            store: HashMap::new(),
            outer,
        }
    }
    pub fn new_enclosed_env(_outer: Option<Environment>) -> Self {
        return Self::new(_outer);
    }
    pub fn get(&self, name: &str) -> Option<&Object> {
        let obj = self.store.get(name);
        if obj.is_none() && self.outer.is_some() {
            return self.outer.as_ref().unwrap().get(name);
        }
        return obj;
    }
    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }
}
