use crate::core::{
    analyzer::Type,
    codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator},
    interpreter::engine::{ResultValue, Value},
};

pub trait UnaryOp {
    fn calculate(&self, value: Value) -> ResultValue;
    fn type_check(&self, value: Value) -> ResultValue;
    fn to_tac(&self, temp: String, value: Value) -> TACInstruction;
    fn value_to_string(&self, value: Value) -> String {
        match value {
            Value::String(s) => s,
            _ => panic!("Expected String value for operand"),
        }
    }
}

pub trait BinaryOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue;
    fn type_check(&self, left: Value, right: Value) -> ResultValue;
    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction;
    fn values_to_string_pair(&self, left: Value, right: Value) -> (String, String) {
        let left = match left {
            Value::String(s) => s,
            _ => panic!("Expected String value for left operand"),
        };

        let right = match right {
            Value::String(s) => s,
            _ => panic!("Expected String value for right operand"),
        };

        (left, right)
    }
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
    fn type_check(&self, value: Value) -> ResultValue {
        match value {
            Value::Int(_) | Value::Float(_) => Ok(Some(value)),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, value: Value) -> TACInstruction {
        TACInstruction::UnaryOp(temp, UnaryOperator::Neg, self.value_to_string(value))
    }
}

impl UnaryOp for NotOp {
    fn calculate(&self, value: Value) -> ResultValue {
        match value {
            Value::Bool(b) => Ok(Some(Value::Bool(!b))),
            _ => panic!("Type mismatch for not"),
        }
    }
    fn type_check(&self, value: Value) -> ResultValue {
        match value {
            Value::Bool(_) => Ok(Some(Value::Bool(false))),
            _ => Err("TypeError: Expected bool".to_string()),
        }
    }
    fn to_tac(&self, temp: String, value: Value) -> TACInstruction {
        TACInstruction::UnaryOp(temp, UnaryOperator::Not, self.value_to_string(value))
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Int))),
            (Value::Type(Type::Float), Value::Type(Type::Float)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Add, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Int))),
            (Value::Type(Type::Float), Value::Type(Type::Float)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Sub, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Int))),
            (Value::Type(Type::Float), Value::Type(Type::Float)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Mult, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Int))),
            (Value::Type(Type::Float), Value::Type(Type::Float)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => {
                Ok(Some(Value::Type(Type::Float)))
            }
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Div, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int))
            | (Value::Type(Type::Float), Value::Type(Type::Float))
            | (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Eq, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int))
            | (Value::Type(Type::Float), Value::Type(Type::Float))
            | (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Neq, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int))
            | (Value::Type(Type::Float), Value::Type(Type::Float))
            | (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Gt, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int))
            | (Value::Type(Type::Float), Value::Type(Type::Float))
            | (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }
    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Lt, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int))
            | (Value::Type(Type::Float), Value::Type(Type::Float))
            | (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }

    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Ge, left, right)
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

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Int), Value::Type(Type::Int))
            | (Value::Type(Type::Float), Value::Type(Type::Float))
            | (Value::Type(Type::Int), Value::Type(Type::Float))
            | (Value::Type(Type::Float), Value::Type(Type::Int)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected int, float".to_string()),
        }
    }
    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Le, left, right)
    }
}

impl BinaryOp for OrOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Some(Value::Bool(b1 || b2))),
            _ => panic!("Invalid types for *"),
        }
    }

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Bool(_), Value::Bool(_)) => Ok(Some(Value::Bool(false))),
            _ => Err("TypeError: Expected bool".to_string()),
        }
    }
    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::Or, left, right)
    }
}

impl BinaryOp for AndOp {
    fn calculate(&self, left: Value, right: Value) -> ResultValue {
        match (left, right) {
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Some(Value::Bool(b1 && b2))),
            _ => panic!("Invalid types for *"),
        }
    }

    fn type_check(&self, left: Value, right: Value) -> ResultValue {
        match (&left, &right) {
            (Value::Type(Type::Bool), Value::Type(Type::Bool)) => Ok(Some(Value::Type(Type::Bool))),
            _ => Err("TypeError: Expected bool".to_string()),
        }
    }
    fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        let (left, right) = self.values_to_string_pair(left, right);

        TACInstruction::BinaryOp(temp, BinaryOperator::And, left, right)
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

    pub fn type_check(&self, value: Value) -> ResultValue {
        self.op.type_check(value)
    }

    pub fn to_tac(&self, temp: String, value: Value) -> TACInstruction {
        self.op.to_tac(temp, value)
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

    pub fn type_check(&self, left: Value, right: Value) -> ResultValue {
        self.op.type_check(left, right)
    }

    pub fn to_tac(&self, temp: String, left: Value, right: Value) -> TACInstruction {
        self.op.to_tac(temp, left, right)
    }
}
