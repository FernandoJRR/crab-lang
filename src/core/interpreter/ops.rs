use super::engine::{ResultValue, Value};

pub trait UnaryOp {
    fn calculate(&self, value: Value) -> ResultValue;
}

pub trait BinaryOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue;
}

//Arithmetic
pub struct NegOp;
pub struct AddOp;
pub struct SubOp;
pub struct MultOp;
pub struct DivOp;

//Relational
pub struct EqOp;
pub struct NeqOp;
pub struct GthOp;
pub struct LthOp;
pub struct GeqOp;
pub struct LeqOp;

//Boolean
pub struct NotOp;
pub struct OrOp;
pub struct AndOp;


impl UnaryOp for NegOp {
    fn calculate(&self, value: Value) -> ResultValue {
        match value {
            Value::Int(n) => Ok(Some(Value::Int(-(n as i64) as u64))),
            Value::Float(f) => Ok(Some(Value::Float(-f))),
            _ => panic!("Type mismatch for unary -"),
        }
    }
}

impl UnaryOp for NotOp {
    fn calculate(&self, value: Value) -> ResultValue {
        match value {
            Value::Bool(b) => Ok(Some(Value::Bool(!b))),
            _ => panic!("Type mismatch for not"),
        }
    }
}

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

impl BinaryOp for EqOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Bool(a == b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Bool(a == b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Bool(a as f64 == b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Bool(a == b as f64))),
            _ => panic!("Type mismatch for =="),
        }
    }
}

impl BinaryOp for NeqOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Bool(a != b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Bool(a != b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Bool(a as f64 != b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Bool(a != b as f64))),
            _ => panic!("Type mismatch for =="),
        }
    }
}

impl BinaryOp for GthOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Bool(a > b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Bool(a > b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Bool(a as f64 > b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Bool(a > b as f64))),
            _ => panic!("Type mismatch for =="),
        }
    }
}

impl BinaryOp for LthOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Bool(a < b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Bool(a < b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Bool((a as f64) < b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Bool(a < b as f64))),
            _ => panic!("Type mismatch for =="),
        }
    }
}

impl BinaryOp for GeqOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Bool(a >= b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Bool(a >= b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Bool(a as f64 >= b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Bool(a >= b as f64))),
            _ => panic!("Type mismatch for =="),
        }
    }
}

impl BinaryOp for LeqOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(Value::Bool(a <= b))),
            (Value::Float(a), Value::Float(b)) => Ok(Some(Value::Bool(a <= b))),
            (Value::Int(a), Value::Float(b)) => Ok(Some(Value::Bool(a as f64 <= b))),
            (Value::Float(a), Value::Int(b)) => Ok(Some(Value::Bool(a <= b as f64))),
            _ => panic!("Type mismatch for =="),
        }
    }
}

impl BinaryOp for OrOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Some(Value::Bool(b1 || b2))),
            _ => panic!("Invalid types for *"),
        }
    }
}

impl BinaryOp for AndOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Some(Value::Bool(b1 && b2))),
            _ => panic!("Invalid types for *"),
        }
    }
}

pub struct UnaryContext {
    op: Box<dyn UnaryOp>,
}

impl UnaryContext {
    pub fn new(op: Box<dyn UnaryOp>) -> Self {
        Self { op }
    }

    pub fn execute(&self, value: Value) -> ResultValue {
        self.op.calculate(value)
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
