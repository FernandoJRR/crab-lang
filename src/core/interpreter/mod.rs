use std::collections::HashMap;

use crate::core::analyzer::Node;

use super::analyzer::NodeKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Int(u64),
    Float(f64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printable = match *self {
            Self::Null => "null",
            Self::Int(n) => &n.to_string(),
            Self::Float(f) => &f.to_string()
        };
        write!(f, "{}", printable)
    }
}

type ResultValue = Option<Value>;

pub trait Visitor<'src> {
    fn visit_null(&mut self, node: &Node) -> ResultValue;
    fn visit_int(&mut self, node: &Node, value: u64) -> ResultValue;
    fn visit_float(&mut self, node: &Node, value: f64) -> ResultValue;
    fn visit_value(&mut self, node: &Node, name: &'src str) -> ResultValue;
    fn visit_neg(&mut self, node: &Node) -> ResultValue;
    fn visit_mult(&mut self, node: &Node) -> ResultValue;
    fn visit_div(&mut self, node: &Node) -> ResultValue;
    fn visit_add(&mut self, node: &Node) -> ResultValue;
    fn visit_sub(&mut self, node: &Node) -> ResultValue;
    fn visit_decl(&mut self, node: &Node) -> ResultValue;
    fn visit_insts(&mut self, node: &Node) -> ResultValue;
    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue;
}

impl<'src> Node {
    pub fn visit<V: Visitor<'src>>(&'src self, visitor: &mut V) -> ResultValue {
        match &self.kind {
            NodeKind::Null => visitor.visit_null(self),
            NodeKind::Int(n) => visitor.visit_int(self, *n),
            NodeKind::Float(f) => visitor.visit_float(self, *f),
            NodeKind::Value(name) => visitor.visit_value(self, name),
            NodeKind::Neg => visitor.visit_neg(self),
            NodeKind::Mult => visitor.visit_mult(self),
            NodeKind::Div => visitor.visit_div(self),
            NodeKind::Add => visitor.visit_add(self),
            NodeKind::Sub => visitor.visit_sub(self),
            NodeKind::Decl => visitor.visit_decl(self),
            NodeKind::FnCall(fn_name) => visitor.visit_fn_call(self, fn_name),

            NodeKind::Insts => visitor.visit_insts(self),
        }
    }
}

pub struct Interpreter {
    pub sym_table: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            sym_table: HashMap::new(),
        }
    }
}

impl<'src> Visitor<'src> for Interpreter {
    fn visit_null(&mut self, _node: &Node) -> ResultValue {
        Some(Value::Null)
    }

    fn visit_int(&mut self, _node: &Node, value: u64) -> ResultValue {
        Some(Value::Int(value))
    }

    fn visit_float(&mut self, _node: &Node, value: f64) -> ResultValue {
        Some(Value::Float(value))
    }

    fn visit_value(&mut self, _node: &Node, name: &'src str) -> ResultValue {
        match self.sym_table.get(name) {
            Some(value) => Some(value.clone()),
            None => panic!("Variable not initialized"),
        }
    }

    fn visit_neg(&mut self, node: &Node) -> ResultValue {
        let child = &node.children.as_ref().unwrap()[0];
        match child.visit(self) {
            Some(Value::Int(n)) => Some(Value::Int(-(n as i64) as u64)),
            Some(Value::Float(f)) => Some(Value::Float(-f)),
            _ => panic!("Invalid operand to neg"),
        }
    }

    fn visit_mult(&mut self, node: &Node) -> ResultValue {
        let [ref left, ref right] = node.children.as_ref().unwrap()[..] else {
            panic!("Multiplication requires 2 children");
        };
        let left_result = left.visit(self).unwrap();
        let right_result = right.visit(self).unwrap();

        let value = match (left_result, right_result) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
            _ => panic!("Invalid types for *"),
        };
        Some(value)
    }

    fn visit_div(&mut self, node: &Node) -> ResultValue {
        let [ref left, ref right] = node.children.as_ref().unwrap()[..] else {
            panic!("Division requires 2 children");
        };
        let left_result = left.visit(self).unwrap();
        let right_result = right.visit(self).unwrap();

        match right_result {
            Value::Int(n) => {
                if n == 0 {
                    panic!("Division by zero")
                }
            }
            Value::Float(f) => {
                if f == 0.0 {
                    panic!("Division by zero")
                }
            }
            _ => panic!("Invalid type"),
        };

        let value = match (left_result, right_result) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
            _ => panic!("Invalid types for *"),
        };
        Some(value)
    }

    fn visit_add(&mut self, node: &Node) -> ResultValue {
        let [ref left, ref right] = node.children.as_ref().unwrap()[..] else {
            panic!("Multiplication requires 2 children");
        };
        let left_result = left.visit(self).unwrap();
        let right_result = right.visit(self).unwrap();

        let value = match (left_result, right_result) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
            _ => panic!("Invalid types for *"),
        };
        Some(value)
    }

    fn visit_sub(&mut self, node: &Node) -> ResultValue {
        let [ref left, ref right] = node.children.as_ref().unwrap()[..] else {
            panic!("Multiplication requires 2 children");
        };
        let left_result = left.visit(self).unwrap();
        let right_result = right.visit(self).unwrap();

        let value = match (left_result, right_result) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
            _ => panic!("Invalid types for *"),
        };
        Some(value)
    }

    fn visit_decl(&mut self, node: &Node) -> ResultValue {
        let [ref name, ref value] = node.children.as_ref().unwrap()[..] else {
            panic!("Declaration requires 2 children");
        };

        let var_name = match &name.kind {
            NodeKind::Value(n) => n,
            _ => panic!("Invalid name for declaration"),
        };

        let value_result = value.visit(self).unwrap();

        self.sym_table.insert(var_name.to_string(), value_result);

        None
    }

    fn visit_insts(&mut self, node: &Node) -> ResultValue {
        match &node.children {
            Some(children) => {
                for child in children {
                    child.visit(self);
                }
                None
            }
            None => None,
        }
    }

    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue {
        let [ref expr] = node.children.as_ref().unwrap()[..] else {
            panic!("Function call requires 1 children");
        };

        match fn_name {
            "print" => {
                let expr_value = expr.visit(self).unwrap();
                println!("{}", expr_value);
                None
            }
            _ => todo!("Custom functions not implemented"),
        }
    }
}
