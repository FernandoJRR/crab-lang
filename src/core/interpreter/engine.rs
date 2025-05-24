use std::collections::HashMap;
use std::rc::Rc;

use super::builtins::{Callable, PrintFunc};
use super::ops::*;
use super::visitor::Visitor;
use crate::core::analyzer::{Node, NodeKind};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Int(u64),
    Float(f64),
    Bool(bool),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printable = match *self {
            Self::Null => "null",
            Self::Int(n) => &n.to_string(),
            Self::Float(f) => &f.to_string(),
            Self::Bool(b) => &b.to_string(),
        };
        write!(f, "{}", printable)
    }
}

pub type ResultValue = Result<Option<Value>, String>;

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

    fn unary_op(&mut self, node: &Node, op_strategy: Box<dyn UnaryOp>) -> ResultValue {
        let [ref val] = node.children.as_ref().unwrap()[..] else {
            panic!("Unary op requires 1 children");
        };

        let v = val.visit(self).unwrap();
        UnaryContext::new(op_strategy).execute(v.unwrap())
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

    fn visit_bool(&mut self, _node: &Node, value: bool) -> ResultValue {
        Ok(Some(Value::Bool(value)))
    }

    fn visit_value(&mut self, _node: &Node, name: &'src str) -> ResultValue {
        match self.sym_table.get(name) {
            Some(value) => Ok(Some(value.clone())),
            None => panic!("Variable not initialized"),
        }
    }

    fn visit_neg(&mut self, node: &Node) -> ResultValue {
        self.unary_op(node, Box::new(NegOp))
    }

    fn visit_not(&mut self, node: &Node) -> ResultValue {
        self.unary_op(node, Box::new(NotOp))
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

    fn visit_eq(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(EqOp))
    }

    fn visit_neq(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(NeqOp))
    }

    fn visit_greater(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(GthOp))
    }

    fn visit_lesser(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(LthOp))
    }

    fn visit_greater_eq(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(GeqOp))
    }

    fn visit_lesser_eq(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(LeqOp))
    }

    fn visit_and(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(AndOp))
    }
    fn visit_or(&mut self, node: &Node) -> ResultValue {
        self.binary_op(node, Box::new(OrOp))
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

pub struct PrettyPrinter {
    indent: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        PrettyPrinter { indent: 0 }
    }

    fn indent_str(&self) -> String {
        " ".repeat(self.indent)
    }

    fn with_indent<F>(&mut self, f: F) -> ResultValue
    where
        F: FnOnce(&mut Self) -> ResultValue,
    {
        self.indent += 4;
        let res = f(self);
        self.indent -= 4;
        res
    }
}

impl<'src> Visitor<'src> for PrettyPrinter {
    fn visit_null(&mut self, _node: &Node) -> ResultValue {
        println!("{}Null", self.indent_str());
        Ok(Some(Value::Null))
    }

    fn visit_int(&mut self, _node: &Node, n: u64) -> ResultValue {
        println!("{}Int({})", self.indent_str(), n);
        Ok(Some(Value::Null))
    }

    fn visit_float(&mut self, _node: &Node, f: f64) -> ResultValue {
        println!("{}Float({})", self.indent_str(), f);
        Ok(Some(Value::Null))
    }

    fn visit_bool(&mut self, _node: &Node, b: bool) -> ResultValue {
        println!("{}Bool({})", self.indent_str(), b);
        Ok(Some(Value::Null))
    }

    fn visit_value(&mut self, _node: &Node, name: &'src str) -> ResultValue {
        println!("{}Value({})", self.indent_str(), name);
        Ok(Some(Value::Null))
    }

    fn visit_neg(&mut self, node: &Node) -> ResultValue {
        println!("{}Neg", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_not(&mut self, node: &Node) -> ResultValue {
        println!("{}Not", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_add(&mut self, node: &Node) -> ResultValue {
        println!("{}Add", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_sub(&mut self, node: &Node) -> ResultValue {
        println!("{}Sub", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_mult(&mut self, node: &Node) -> ResultValue {
        println!("{}Mult", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_div(&mut self, node: &Node) -> ResultValue {
        println!("{}Div", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_eq(&mut self, node: &Node) -> ResultValue {
        println!("{}Eq", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_neq(&mut self, node: &Node) -> ResultValue {
        println!("{}Neq", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_greater(&mut self, node: &Node) -> ResultValue {
        println!("{}Gth", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_lesser(&mut self, node: &Node) -> ResultValue {
        println!("{}Lth", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_greater_eq(&mut self, node: &Node) -> ResultValue {
        println!("{}Geq", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_lesser_eq(&mut self, node: &Node) -> ResultValue {
        println!("{}Leq", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_and(&mut self, node: &Node) -> ResultValue {
        println!("{}And", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }
    fn visit_or(&mut self, node: &Node) -> ResultValue {
        println!("{}Or", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(Some(Value::Null))
    }

    fn visit_decl(&mut self, node: &Node) -> ResultValue {
        println!("{}Decl", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(None)
    }

    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue {
        println!("{}FnCall({})", self.indent_str(), fn_name);
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(None)
    }

    fn visit_insts(&mut self, node: &Node) -> ResultValue {
        println!("{}Insts", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(None)
    }
}
