use std::{collections::HashMap, rc::Rc};

use crate::core::{
    analyzer::{Node, NodeKind, Type},
    interpreter::engine::{ResultValue, Value},
};

use super::{
    builtins::{Callable, Param, PrintFunc, UserFunc},
    ops::*,
    sym_table::ScopedTypeSymTable,
    visitor::Visitor,
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    func_table: HashMap<String, Rc<dyn Callable>>,
    sym_table: ScopedTypeSymTable,
    errors: Vec<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut fns: HashMap<String, Rc<dyn Callable>> = HashMap::new();
        fns.insert("print".to_string(), Rc::new(PrintFunc));

        Self {
            func_table: fns,
            sym_table: ScopedTypeSymTable::new(),
            errors: Vec::new(),
        }
    }

    pub fn get_tables(&mut self) -> (&ScopedTypeSymTable, &HashMap<String, Rc<dyn Callable>>) {
        (&self.sym_table, &self.func_table)
    }

    pub fn push_scope(&mut self) {
        self.sym_table.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.sym_table.pop_scope();
    }

    pub fn set_var(&mut self, name: String, val: Type) {
        self.sym_table.set(name, val);
    }

    pub fn get_var(&mut self, name: &str) -> Option<Type> {
        self.sym_table.get(name)
    }

    fn unary_op_check(&mut self, value: Value, op_strategy: Box<dyn UnaryOp>) -> ResultValue {
        UnaryContext::new(op_strategy).type_check(value)
    }

    fn binary_op_check(
        &mut self,
        lv: Value,
        rv: Value,
        op_strategy: Box<dyn BinaryOp>,
    ) -> ResultValue {
        BinaryContext::new(op_strategy).type_check(lv, rv)
    }
}

impl<'src> Visitor<'src> for SemanticAnalyzer {
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
        match self.get_var(name) {
            Some(var_type) => Ok(Some(Value::Type(var_type))),
            None => {
                self.errors
                    .push(format!("Variable '{}' not declared", name));
                Ok(None)
            }
        }
    }

    fn visit_decl(&mut self, node: &Node) -> ResultValue {
        let [ref name_node, ref value_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Declaration requires 2 children".to_string());
        };

        let var_name = match &name_node.kind {
            NodeKind::Value(n) => n,
            _ => return Err("Invalid name for declaration".to_string()),
        };

        let resultado = value_node.visit(self)?;
        let value_type = match resultado {
            Some(Value::Type(t)) => t,
            Some(Value::Int(_)) => Type::Int,
            Some(Value::Float(_)) => Type::Float,
            Some(Value::Bool(_)) => Type::Bool,
            Some(Value::String(_)) => Type::String,
            _ => return Err("Unsupported value type in declaration".to_string()),
        };

        self.set_var(var_name.to_string(), value_type);
        Ok(None)
    }

    fn visit_assign(&mut self, node: &Node) -> ResultValue {
        let [ref name_node, ref value_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Assignment requires 2 children".to_string());
        };

        let var_name = match &name_node.kind {
            NodeKind::Value(n) => n,
            _ => return Err("Invalid name for assignment".to_string()),
        };

        let existing_type = self.get_var(var_name);
        if existing_type.clone().is_none() {
            self.errors
                .push(format!("Variable '{}' not declared", var_name));
            return Ok(None);
        }

        let value_type = match value_node.visit(self)? {
            Some(Value::Type(t)) => t,
            Some(Value::Int(_)) => Type::Int,
            Some(Value::Float(_)) => Type::Float,
            Some(Value::Bool(_)) => Type::Bool,
            Some(Value::String(_)) => Type::String,
            _ => return Err("Unsupported value type in assignment".to_string()),
        };

        if existing_type.clone().unwrap() != value_type {
            self.errors.push(format!(
                "Type mismatch in assignment to '{}': expected {:?}, got {:?}",
                var_name,
                existing_type.unwrap(),
                value_type
            ));
        }

        Ok(None)
    }

    fn visit_insts(&mut self, node: &Node) -> ResultValue {
        //println!("instrucciones entrando");
        if let Some(children) = &node.children {
            for child in children {
                child.visit(self)?;
            }
        }
        Ok(None)
    }

    fn visit_neg(&mut self, node: &Node) -> ResultValue {
        let [ref val_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Negation expects 1 child".to_string());
        };
        let value = val_node.visit(self)?.ok_or("Missing value in negation")?;
        self.unary_op_check(value, Box::new(NegOp))
    }

    fn visit_not(&mut self, node: &Node) -> ResultValue {
        let [ref val_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Not expects 1 child".to_string());
        };
        let value = val_node.visit(self)?.ok_or("Missing value in not")?;
        self.unary_op_check(value, Box::new(NotOp))
    }

    fn visit_add(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Addition expects 2 children".to_string());
        };
        let lv = l.visit(self)?;
        let rv = r.visit(self)?;
        self.binary_op_check(lv.unwrap(), rv.unwrap(), Box::new(AddOp))
    }

    fn visit_sub(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Subtraction expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in sub")?;
        let rv = r.visit(self)?.ok_or("Missing right value in sub")?;
        self.binary_op_check(lv, rv, Box::new(SubOp))
    }

    fn visit_mult(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Muliplication expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in sub")?;
        let rv = r.visit(self)?.ok_or("Missing right value in sub")?;
        self.binary_op_check(lv, rv, Box::new(SubOp))
    }

    fn visit_div(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Division expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in sub")?;
        let rv = r.visit(self)?.ok_or("Missing right value in sub")?;
        self.binary_op_check(lv, rv, Box::new(SubOp))
    }

    fn visit_eq(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Eq expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in eq")?;
        let rv = r.visit(self)?.ok_or("Missing right value in eq")?;
        self.binary_op_check(lv, rv, Box::new(EqOp))
    }

    fn visit_neq(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Neq expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in neq")?;
        let rv = r.visit(self)?.ok_or("Missing right value in neq")?;
        self.binary_op_check(lv, rv, Box::new(NeqOp))
    }

    fn visit_and(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("And expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in and")?;
        let rv = r.visit(self)?.ok_or("Missing right value in and")?;
        self.binary_op_check(lv, rv, Box::new(AndOp))
    }

    fn visit_or(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Or expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in or")?;
        let rv = r.visit(self)?.ok_or("Missing right value in or")?;
        self.binary_op_check(lv, rv, Box::new(OrOp))
    }

    fn visit_lesser(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Lesser expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in lesser")?;
        let rv = r.visit(self)?.ok_or("Missing right value in lesser")?;
        self.binary_op_check(lv, rv, Box::new(LthOp))
    }

    fn visit_greater(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Greater expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in greater")?;
        let rv = r.visit(self)?.ok_or("Missing right value in greater")?;
        self.binary_op_check(lv, rv, Box::new(GthOp))
    }

    fn visit_lesser_eq(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Leq expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in leq")?;
        let rv = r.visit(self)?.ok_or("Missing right value in leq")?;
        self.binary_op_check(lv, rv, Box::new(LeqOp))
    }

    fn visit_greater_eq(&mut self, node: &Node) -> ResultValue {
        let [ref l, ref r] = node.children.as_ref().unwrap()[..] else {
            return Err("Geq expects 2 children".to_string());
        };
        let lv = l.visit(self)?.ok_or("Missing left value in geq")?;
        let rv = r.visit(self)?.ok_or("Missing right value in geq")?;
        self.binary_op_check(lv, rv, Box::new(GeqOp))
    }

    fn visit_params(&mut self, node: &Node) -> ResultValue {
        if let Some(children) = &node.children {
            for child in children {
                child.visit(self)?;
            }
        }
        Ok(None)
    }

    fn visit_param(&mut self, node: &Node, name: &'src str) -> ResultValue {
        if let Some(children) = &node.children {
            for child in children {
                child.visit(self)?;
            }
        }
        Ok(None)
    }

    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue {
        let [ref args_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Function call requires 1 children".to_string());
        };

        let func = self.func_table.get(fn_name).cloned();
        if func.is_none() {
            self.errors
                .push(format!("Function '{}' not found", fn_name));
            return Ok(None);
        }

        let callable = func.unwrap();

        let mut args = Vec::new();
        if let Some(arg_nodes) = &args_node.children {
            for arg_node in arg_nodes {
                let arg = arg_node.visit(self)?;
                if let Some(val) = arg {
                    args.push(val);
                }
            }
        }

        if let Err(err) = callable.check(&args) {
            self.errors
                .push(format!("Function '{}' argument error: {}", fn_name, err));
            return Ok(None);
        }

        Ok(None)
    }

    fn visit_fn(&mut self, node: &Node, name: &'src str) -> ResultValue {
        let children = node.children.as_ref().unwrap();

        let (params_node, _ret_type_opt, body_node) = match children.as_slice() {
            [params, body] => (params, None, body),
            [params, _ret_type, body] => (params, Some(_ret_type), body),
            _ => return Err(format!("Function '{}' must have 2 or 3 children", name)),
        };

        let mut params = Vec::new();
        if let Some(param_nodes) = &params_node.children {
            for p in param_nodes {
                if let NodeKind::Param(param_name) = &p.kind {
                    let [ref type_node] = p.children.as_ref().unwrap()[..] else {
                        return Err(format!("Param '{}' missing type", param_name));
                    };
                    let param_type = match type_node.visit(self)? {
                        Some(Value::Type(t)) => t,
                        _ => return Err(format!("Invalid type for param '{}'", param_name)),
                    };
                    params.push(Param {
                        name: param_name.to_string(),
                        var_type: param_type,
                    });
                } else {
                    return Err("Invalid node in parameter list".to_string());
                }
            }
        }

        let func = UserFunc {
            params,
            body: body_node.clone(),
        };

        //println!("CUERPO {:?}", body_node);
        let _ = body_node.visit(self);

        self.func_table.insert(name.to_string(), Rc::new(func));
        Ok(None)
    }
}
