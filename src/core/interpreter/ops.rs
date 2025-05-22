use super::engine::{ResultValue, Value};

pub trait BinaryOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue;
}

pub struct AddOp;
pub struct SubOp;
pub struct MultOp;
pub struct DivOp;

impl BinaryOp for AddOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Int(a + b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Float(a + b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Float(a as f64 + b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Float(a + b as f64))),
            _ => panic!("Type mismatch for +"),
        }
    }
}

impl BinaryOp for SubOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Int(a - b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Float(a - b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Float(a as f64 - b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Float(a - b as f64))),
            _ => panic!("Type mismatch for +"),
        }
    }
}

impl BinaryOp for MultOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Int(a * b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Float(a * b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Float(a as f64 * b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Float(a * b as f64))),
            _ => panic!("Type mismatch for *"),
        }
    }
}

impl BinaryOp for DivOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match right {
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

        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Int(a / b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Float(a / b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Float(a as f64 / b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Float(a / b as f64))),
            _ => panic!("Invalid types for *"),
        }
    }
}

pub struct BinaryContext {
    op: Box<dyn BinaryOp>,
}

impl BinaryContext {
    pub fn new(op: Box<dyn BinaryOp>) -> Self {
        Self { op }
    }

    pub fn execute(&self, left: Value, right: Value) -> ResultValue {
        self.op.calculate(left, right)
    }
}
