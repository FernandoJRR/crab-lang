use std::{collections::HashMap, rc::Rc};

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
            Self::Float(f) => &f.to_string(),
        };
        write!(f, "{}", printable)
    }
}

type ResultValue = Result<Option<Value>, String>;

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

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue;
}

pub struct PrintFunc;

impl Callable for PrintFunc {
    fn call(&self, _interpreter: &mut Interpreter, args: &[Value]) -> ResultValue {
        let v = args
            .first()
            .ok_or_else(|| "print: missing argument".to_string())?;
        println!("{}", v);
        Ok(None)
    }
}

pub struct UserFunc {
    params: Vec<String>,
    body: Node,
}

impl Callable for UserFunc {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue {
        if args.len() != self.params.len() {
            return Err(format!(
                "Expected {} args, got {}",
                self.params.len(),
                args.len()
            ));
        }
        self.body.visit(interpreter)
    }
}

pub struct Interpreter {
    pub sym_table: HashMap<String, Value>,
    func_table: HashMap<String, Rc<dyn Callable>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut fns: HashMap<String, Rc<dyn Callable>> = HashMap::new();
        fns.insert("print".to_string(), Rc::new(PrintFunc));
        Self {
            sym_table: HashMap::new(),
            func_table: fns,
        }
    }

    fn binary_op(&mut self, node: &Node, op_strategy: Box<dyn BinaryOp>) -> ResultValue {
        let [ref left, ref right] = node.children.as_ref().unwrap()[..] else {
            panic!("Binary op requires 2 children");
        };

        let l = left.visit(self).unwrap();
        let r = right.visit(self).unwrap();
        BinaryContext::new(op_strategy).execute(l.unwrap(), r.unwrap())
    }
}

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

impl<'src> Visitor<'src> for Interpreter {
    fn visit_null(&mut self, _node: &Node) -> ResultValue {
        Ok(Some(Value::Null))
    }

    fn visit_int(&mut self, _node: &Node, value: u64) -> ResultValue {
        Ok(Some(Value::Int(value)))
    }

    fn visit_float(&mut self, _node: &Node, value: f64) -> ResultValue {
        Ok(Some(Value::Float(value)))
    }

    fn visit_value(&mut self, _node: &Node, name: &'src str) -> ResultValue {
        match self.sym_table.get(name) {
            Some(value) => Ok(Some(value.clone())),
            None => panic!("Variable not initialized"),
        }
    }

    fn visit_neg(&mut self, node: &Node) -> ResultValue {
        let child = &node.children.as_ref().unwrap()[0];
        match child.visit(self) {
            Ok(Some(Value::Int(n))) => Ok(Some(Value::Int(-(n as i64) as u64))),
            Ok(Some(Value::Float(f))) => Ok(Some(Value::Float(-f))),
            _ => panic!("Invalid operand to neg"),
        }
    }

    fn visit_mult(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(MultOp))
    }

    fn visit_div(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(DivOp))
    }

    fn visit_add(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(AddOp))
    }

    fn visit_sub(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(SubOp))
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

        self.sym_table
            .insert(var_name.to_string(), value_result.unwrap());

        Ok(None)
    }

    fn visit_insts(&mut self, node: &Node) -> ResultValue {
        match &node.children {
            Some(children) => {
                for child in children {
                    let _ = child.visit(self);
                }
                Ok(None)
            }
            None => Ok(None),
        }
    }

    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue {
        let args: Vec<Value> = node
            .children
            .as_ref()
            .unwrap_or(&vec![])
            .iter()
            .map(|child| child.visit(self))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .map(|opt| opt.unwrap_or(Value::Null))
            .collect();

        let func = match self.func_table.get(fn_name).cloned() {
            Some(f) => f,
            None => return Err(format!("Unknown function '{}'", fn_name)),
        };

        func.call(self, &args)
    }
}
