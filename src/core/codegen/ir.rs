use crate::core::{
    analyzer::{Node, NodeKind, Type},
    facade::{
        ops::{
            AddOp, AndOp, BinaryContext, BinaryOp, DivOp, EqOp, GeqOp, GthOp, LeqOp, LthOp, MultOp,
            NegOp, NeqOp, NotOp, OrOp, SubOp, UnaryContext, UnaryOp,
        },
        visitor::Visitor,
    },
    interpreter::engine::{ResultValue, Value},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,
    Eq,  // Equal
    Neq, // Not equal
    Gt,  // Greater than
    Lt,  // Less than
    Ge,  // Greater than or equal to
    Le,  // Less than or equal to
    And, // Logical AND
    Or,  // Logical OR
}

#[derive(Debug)]
pub enum TACInstruction {
    Assign(String, String),
    BinaryOp(String, BinaryOperator, String, String), // temp_res, op, arg1, arg2
    UnaryOp(String, UnaryOperator, String),           // temp_res, op, arg
    Label(String),
    Goto(String),
    IfGoto(String, String),
    Param(String),
    Call(String, usize),
    Return(Option<String>),
}

// Builder for TAC instructions
pub struct TACBuilder {
    instructions: Vec<TACInstruction>,
    temp_counter: usize,
}

impl TACBuilder {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            temp_counter: 0,
        }
    }

    fn new_temp(&mut self) -> String {
        let temp = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        temp
    }

    pub fn emit(&mut self, tac_instruction: TACInstruction) {
        self.instructions.push(tac_instruction);
    }
}

pub struct TACGenerator {
    builder: TACBuilder,
}

impl TACGenerator {
    pub fn new() -> Self {
        Self {
            builder: TACBuilder::new(),
        }
    }

    pub fn generate(&mut self, ast: &Node) {
        ast.visit(self);
    }

    fn generate_unary_op(
        &mut self,
        node: &Node,
        op_strategy: Box<dyn UnaryOp>,
    ) -> (String, TACInstruction) {
        let [ref val] = node.children.as_ref().unwrap()[..] else {
            panic!("Unary op requires 1 children");
        };

        let v = val.visit(self).unwrap();

        let temp = self.builder.new_temp();
        let tac = UnaryContext::new(op_strategy).to_tac(temp.clone(), v.unwrap());
        (temp, tac)
    }

    fn generate_binary_op(
        &mut self,
        node: &Node,
        op_strategy: Box<dyn BinaryOp>,
    ) -> (String, TACInstruction) {
        let [ref left, ref right] = node.children.as_ref().unwrap()[..] else {
            panic!("Binary op requires 2 children");
        };

        let l = left.visit(self).unwrap();
        let r = right.visit(self).unwrap();

        let temp = self.builder.new_temp();
        let tac = BinaryContext::new(op_strategy).to_tac(temp.clone(), l.unwrap(), r.unwrap());
        (temp, tac)
    }

    pub fn get_instructions(&self) -> &Vec<TACInstruction> {
        &self.builder.instructions
    }
}

impl<'src> Visitor<'src> for TACGenerator {
    fn visit_null(&mut self, _node: &Node) -> ResultValue {
        let temp = self.builder.new_temp();

        self.builder
            .emit(TACInstruction::Assign(temp.clone(), "null".to_string()));

        Ok(Some(Value::String(temp)))
    }
    fn visit_int(&mut self, _node: &Node, value: u64) -> ResultValue {
        let temp = self.builder.new_temp();
        self.builder
            .emit(TACInstruction::Assign(temp.clone(), value.to_string()));
        Ok(Some(Value::String(temp)))
    }

    fn visit_float(&mut self, _node: &Node, value: f64) -> ResultValue {
        let temp = self.builder.new_temp();
        self.builder
            .emit(TACInstruction::Assign(temp.clone(), value.to_string()));
        Ok(Some(Value::String(temp)))
    }

    fn visit_bool(&mut self, _node: &Node, value: bool) -> ResultValue {
        let temp = self.builder.new_temp();
        self.builder
            .emit(TACInstruction::Assign(temp.clone(), value.to_string()));
        Ok(Some(Value::String(temp)))
    }

    fn visit_string(&mut self, _node: &Node, value: &'src str) -> ResultValue {
        let temp = self.builder.new_temp();
        self.builder
            .emit(TACInstruction::Assign(temp.clone(), value.to_string()));
        Ok(Some(Value::String(temp)))
    }

    fn visit_type(&mut self, _node: &Node, r#type: Type) -> ResultValue {
        Ok(Some(Value::Type(r#type)))
    }

    fn visit_value(&mut self, _node: &Node, name: &'src str) -> ResultValue {
        Ok(Some(Value::String(name.to_string())))
    }

    fn visit_decl(&mut self, node: &Node) -> ResultValue {
        let [ref name_node, ref value_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Declaration requires 2 children".to_string());
        };

        let var_name = match &name_node.kind {
            NodeKind::Value(n) => n.clone(),
            _ => return Err("Invalid name for declaration".to_string()),
        };

        let result = value_node.visit(self)?;
        let temp_result = match result {
            Some(Value::String(t)) => t,
            _ => return Err("Expected a temporary variable string from value".to_string()),
        };

        self.builder
            .emit(TACInstruction::Assign(var_name.clone(), temp_result));

        Ok(None)
    }

    fn visit_assign(&mut self, node: &Node) -> ResultValue {
        let [ref name_node, ref value_node] = node.children.as_ref().unwrap()[..] else {
            return Err("Assignment requires 2 children".to_string());
        };

        let var_name = match &name_node.kind {
            NodeKind::Value(n) => n.clone(),
            _ => return Err("Invalid name for assignment".to_string()),
        };

        let result = value_node.visit(self)?;
        let temp_result = match result {
            Some(Value::String(t)) => t,
            _ => return Err("Expected a temporary variable string from value".to_string()),
        };

        self.builder
            .emit(TACInstruction::Assign(var_name.clone(), temp_result));

        Ok(None)
    }

    fn visit_insts(&mut self, node: &Node) -> ResultValue {
        if let Some(children) = &node.children {
            for child in children {
                child.visit(self)?;
            }
        }
        Ok(None)
    }

    fn visit_neg(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_unary_op(node, Box::new(NegOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_not(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_unary_op(node, Box::new(NotOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_add(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(AddOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_sub(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(SubOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_mult(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(MultOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_div(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(DivOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_eq(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(EqOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_neq(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(NeqOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_and(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(AndOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_or(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(OrOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_lesser(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(LthOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_greater(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(GthOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_lesser_eq(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(LeqOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_greater_eq(&mut self, node: &Node) -> ResultValue {
        let (temp, op_tac) = self.generate_binary_op(node, Box::new(GeqOp));
        self.builder.emit(op_tac);

        Ok(Some(Value::String(temp)))
    }

    fn visit_params(&mut self, node: &Node) -> ResultValue {
        Ok(None)
    }

    fn visit_param(&mut self, node: &Node, name: &'src str) -> ResultValue {
        Ok(None)
    }

    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue {
        let mut arg_temps = Vec::new();

        if let Some(children) = &node.children {
            for child in children {
                let v = child.visit(self)?;
                if let Some(Value::String(temp)) = v {
                    arg_temps.push(temp);
                } else {
                    return Err("Expected string temp as argument".into());
                }
            }
        }

        for arg in arg_temps.iter().rev() {
            self.builder.emit(TACInstruction::Param(arg.clone()));
        }

        let result_temp = self.builder.new_temp();

        self.builder.emit(TACInstruction::Call(fn_name.to_string(), arg_temps.len()));

        self.builder.emit(TACInstruction::Return(Some(result_temp.clone())));

        Ok(Some(Value::String(result_temp)))
    }

    fn visit_fn(&mut self, node: &Node, fn_name: &'src str) -> ResultValue {
        // Label for entering function
        self.builder
            .emit(TACInstruction::Label(fn_name.to_string()));

        // Process parameters
        if let Some(children) = &node.children {
            if let Some(params_node) = children
                .iter()
                .find(|child| matches!(child.kind, NodeKind::Params))
            {
                params_node.visit(self)?;
            }

            // Process function body
            if let Some(body_node) = children
                .iter()
                .find(|child| matches!(child.kind, NodeKind::Insts))
            {
                body_node.visit(self)?;
            }
        }

        Ok(None)
    }
}
