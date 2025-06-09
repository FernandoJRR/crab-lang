use std::collections::HashMap;
use std::rc::Rc;

use chumsky::error::EmptyErr;

use super::super::facade::visitor::Visitor;
use crate::core::analyzer::{Node, NodeKind, Type};
use crate::core::facade::builtins::{Callable, Param, PrintFunc, UserFunc};
use crate::core::facade::ops::*;
use crate::core::facade::sym_table::ScopedSymTable;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Int(u64),
    Float(f64),
    Bool(bool),
    Type(Type),
    String(String),
    Params(Vec<Self>),
    Param(String, Type),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Type(t) => write!(f, "{}", t),
            Value::Params(params) => {
                let formatted_params: Vec<String> =
                    params.iter().map(|p| format!("{}", p)).collect();
                write!(f, "[{}]", formatted_params.join(", "))
            }
            Value::Param(name, ty) => write!(f, "{}: {}", name, ty),
        }
    }
}

pub type ResultValue = Result<Option<Value>, String>;

pub struct Interpreter {
    func_table: HashMap<String, Rc<dyn Callable>>,
    sym_table: ScopedSymTable,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut fns: HashMap<String, Rc<dyn Callable>> = HashMap::new();
        fns.insert("print".to_string(), Rc::new(PrintFunc));

        Self {
            sym_table: ScopedSymTable::new(),
            func_table: fns,
        }
    }

    pub fn interpret(&mut self, result: (Option<Node>, Vec<EmptyErr>)) {
        if let (Some(result), _) = &result {
            let _ = result.visit(self);
            let _ = match self.func_table.get("main").cloned() {
                Some(fn_main) => fn_main.call(self, &[]),
                None => Ok(None),
            };
        }
    }

    pub fn push_scope(&mut self) {
        self.sym_table.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.sym_table.pop_scope();
    }

    pub fn set_var(&mut self, name: String, val: Value) {
        self.sym_table.set(name, val);
    }

    pub fn get_var(&mut self, name: &str) -> Option<Value> {
        self.sym_table.get(name)
    }

    pub fn define_fn(&mut self, name: String, params: Vec<Value>, body: Node) {
        let mut params_conv = Vec::with_capacity(params.len());
        for v in params {
            match v {
                Value::Param(n, t) => params_conv.push(Param {
                    name: n,
                    var_type: t,
                }),
                _ => panic!("expected Value::Param"),
            }
        }

        let user_fn = UserFunc {
            params: params_conv,
            body,
        };
        self.func_table.insert(name, Rc::new(user_fn));
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

    fn visit_string(&mut self, _node: &Node, string: &'src str) -> ResultValue {
        Ok(Some(Value::String(string.to_string())))
    }

    fn visit_type(&mut self, _node: &Node, var_type: Type) -> ResultValue {
        Ok(Some(Value::Type(var_type)))
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

    fn visit_fn(&mut self, node: &Node, name: &'src str) -> ResultValue {
        let children = node.children.as_ref().unwrap();
        match children.as_slice() {
            [params_node, body] => {
                let result_params = params_node.visit(self);
                match result_params {
                    Ok(Some(Value::Params(pms))) => {
                        self.define_fn(name.to_string(), pms, body.clone())
                    }
                    _ => panic!("Cannot create function"),
                }
            }
            [params_node, _return_type, body] => {
                let result_params = params_node.visit(self);
                match result_params {
                    Ok(Some(Value::Params(pms))) => {
                        self.define_fn(name.to_string(), pms, body.clone())
                    }
                    _ => panic!("Cannot create function"),
                }
            }
            _ => panic!("Expected 2 to 3 children, got {}", children.len()),
        }

        Ok(None)
    }

    fn visit_params(&mut self, node: &Node) -> ResultValue {
        let children = match &node.children {
            Some(c) => c,
            None => return Ok(Some(Value::Params(vec![]))),
        };

        let mut values = Vec::with_capacity(children.len());
        for child in children {
            match child.visit(self)? {
                Some(v) => values.push(v),
                None => return Err("Expected value from parameter node".into()),
            }
        }

        Ok(Some(Value::Params(values)))
    }

    fn visit_param(&mut self, node: &Node, name: &'src str) -> ResultValue {
        let [ref var_type] = node.children.as_ref().unwrap()[..] else {
            panic!("Param requires 1 child");
        };

        let var_type = var_type.visit(self).unwrap();

        match var_type {
            Some(Value::Type(type_var)) => Ok(Some(Value::Param(name.to_string(), type_var))),
            _ => Err("Not valid".to_string()),
        }
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
            .set(var_name.to_string(), value_result.unwrap());

        Ok(None)
    }

    fn visit_assign(&mut self, node: &Node) -> ResultValue {
        let [ref name, ref value] = node.children.as_ref().unwrap()[..] else {
            panic!("Declaration requires 2 children");
        };

        let var_name = match &name.kind {
            NodeKind::Value(n) => n,
            _ => panic!("Invalid name for declaration"),
        };

        let value_result = value.visit(self).unwrap();

        let existing_var = self.sym_table.get(var_name);

        match existing_var {
            Some(value) => {
                let type_match = matches!(
                    (value, &value_result.clone().unwrap()),
                    (Value::Int(_), Value::Int(_))
                        | (Value::Float(_), Value::Float(_))
                        | (Value::Bool(_), Value::Bool(_))
                        | (Value::String(_), Value::String(_))
                );

                if type_match {
                    self.sym_table
                        .set(var_name.to_string(), value_result.unwrap());
                    Ok(None)
                } else {
                    Err("mismatched types".to_string())
                }
            }
            None => Err("Cannot find the variable in the scope".to_string()),
        }
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

    fn visit_string(&mut self, _node: &Node, string: &'src str) -> ResultValue {
        println!("{}String({})", self.indent_str(), string);
        Ok(Some(Value::Null))
    }

    fn visit_value(&mut self, _node: &Node, name: &'src str) -> ResultValue {
        println!("{}Value({})", self.indent_str(), name);
        Ok(Some(Value::Null))
    }

    fn visit_type(&mut self, _node: &Node, r#type: Type) -> ResultValue {
        println!("{}Type({})", self.indent_str(), r#type);
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

    fn visit_assign(&mut self, node: &Node) -> ResultValue {
        println!("{}Assign", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(None)
    }

    fn visit_fn(&mut self, node: &Node, name: &'src str) -> ResultValue {
        println!("{}Fn({})", self.indent_str(), name);
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(None)
    }

    fn visit_params(&mut self, node: &Node) -> ResultValue {
        println!("{}Params", self.indent_str());
        let _ = self.with_indent(|v| node.visit_children(v));
        Ok(None)
    }

    fn visit_param(&mut self, node: &Node, name: &'src str) -> ResultValue {
        println!("{}Param({})", self.indent_str(), name);
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
