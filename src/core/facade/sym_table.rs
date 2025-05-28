use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

use crate::core::interpreter::engine::Value;

#[derive(Debug)]
pub struct SymTable {
    pub values: RefCell<HashMap<String, Value>>,
    pub parent: RefCell<Weak<SymTable>>,
    pub children: RefCell<Vec<Rc<SymTable>>>,
}

impl SymTable {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            values: RefCell::new(HashMap::new()),
            parent: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
        })
    }

    pub fn new_child(parent: Rc<Self>) -> Rc<Self> {
        let child = Rc::new(Self {
            values: RefCell::new(HashMap::new()),
            parent: RefCell::new(Rc::downgrade(&parent)),
            children: RefCell::new(vec![]),
        });
        parent.children.borrow_mut().push(child.clone());
        child
    }

    pub fn set(&self, name: String, val: Value) {
        self.values.borrow_mut().insert(name, val);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.values.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent.borrow().upgrade() {
            parent.get(name)
        } else {
            None
        }
    }
}
