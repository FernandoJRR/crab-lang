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

pub struct ScopedSymTable {
    pub current_scope: Rc<SymTable>,
    parent_stack: Vec<Rc<SymTable>>
}

impl ScopedSymTable {
    pub fn new() -> Self {
        Self { current_scope: SymTable::new(), parent_stack: vec![] }
    }

    pub fn push_scope(&mut self) {
        let new_scope = SymTable::new_child(Rc::clone(&self.current_scope));
        self.parent_stack.push(Rc::clone(&self.current_scope));
        self.current_scope = new_scope;
    }

    pub fn pop_scope(&mut self) {
        let parent_weak = self.current_scope.parent.borrow().clone();
        if let Some(parent_rc) = parent_weak.upgrade() {
            self.current_scope = parent_rc;
            self.parent_stack.pop();
        } else {
            panic!("Parent scope was dropped");
        }
    }

    pub fn set(&mut self, name: String, val: Value) {
        self.current_scope.set(name, val);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.current_scope.get(name)
    }
}
