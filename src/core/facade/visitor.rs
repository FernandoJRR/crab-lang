use crate::core::analyzer::{Node, NodeKind, Type};

use super::super::interpreter::engine::ResultValue;

pub trait Visitor<'src> {
    fn visit_null(&mut self, node: &Node) -> ResultValue;
    fn visit_int(&mut self, node: &Node, value: u64) -> ResultValue;
    fn visit_float(&mut self, node: &Node, value: f64) -> ResultValue;
    fn visit_bool(&mut self, node: &Node, value: bool) -> ResultValue;
    fn visit_value(&mut self, node: &Node, name: &'src str) -> ResultValue;
    fn visit_string(&mut self, node: &Node, string: &'src str) -> ResultValue;
    fn visit_type(&mut self, node: &Node, r#type: Type) -> ResultValue;

    fn visit_neg(&mut self, node: &Node) -> ResultValue;
    fn visit_not(&mut self, node: &Node) -> ResultValue;
    fn visit_mult(&mut self, node: &Node) -> ResultValue;
    fn visit_div(&mut self, node: &Node) -> ResultValue;
    fn visit_add(&mut self, node: &Node) -> ResultValue;
    fn visit_sub(&mut self, node: &Node) -> ResultValue;

    fn visit_eq(&mut self, node: &Node) -> ResultValue;
    fn visit_neq(&mut self, node: &Node) -> ResultValue;
    fn visit_greater(&mut self, node: &Node) -> ResultValue;
    fn visit_lesser(&mut self, node: &Node) -> ResultValue;
    fn visit_greater_eq(&mut self, node: &Node) -> ResultValue;
    fn visit_lesser_eq(&mut self, node: &Node) -> ResultValue;

    fn visit_or(&mut self, node: &Node) -> ResultValue;
    fn visit_and(&mut self, node: &Node) -> ResultValue;

    fn visit_decl(&mut self, node: &Node) -> ResultValue;
    fn visit_assign(&mut self, node: &Node) -> ResultValue;

    fn visit_insts(&mut self, node: &Node) -> ResultValue;

    fn visit_fn(&mut self, node: &Node, name: &'src str) -> ResultValue;
    fn visit_params(&mut self, node: &Node) -> ResultValue;
    fn visit_param(&mut self, node: &Node, name: &'src str) -> ResultValue;
    fn visit_fn_call(&mut self, node: &Node, fn_name: &'src str) -> ResultValue;
}

impl<'src> Node {
    pub fn visit<V: Visitor<'src>>(&'src self, visitor: &mut V) -> ResultValue {
        match &self.kind {
            NodeKind::Null => visitor.visit_null(self),
            NodeKind::Int(n) => visitor.visit_int(self, *n),
            NodeKind::Float(f) => visitor.visit_float(self, *f),
            NodeKind::Bool(b) => visitor.visit_bool(self, *b),
            NodeKind::Value(name) => visitor.visit_value(self, name),
            NodeKind::String(s) => visitor.visit_string(self, s),
            NodeKind::Type(t) => visitor.visit_type(self, t.clone()),

            NodeKind::Neg => visitor.visit_neg(self),
            NodeKind::Not => visitor.visit_not(self),
            NodeKind::Mult => visitor.visit_mult(self),
            NodeKind::Div => visitor.visit_div(self),
            NodeKind::Add => visitor.visit_add(self),
            NodeKind::Sub => visitor.visit_sub(self),

            NodeKind::Eq => visitor.visit_eq(self),
            NodeKind::Neq => visitor.visit_neq(self),
            NodeKind::Gth => visitor.visit_greater(self),
            NodeKind::Lth => visitor.visit_lesser(self),
            NodeKind::Geq => visitor.visit_greater_eq(self),
            NodeKind::Leq => visitor.visit_lesser_eq(self),

            NodeKind::Or => visitor.visit_or(self),
            NodeKind::And => visitor.visit_and(self),

            NodeKind::Decl => visitor.visit_decl(self),
            NodeKind::Assign => visitor.visit_assign(self),

            NodeKind::Fn(name) => visitor.visit_fn(self, name),
            NodeKind::Params => visitor.visit_params(self),
            NodeKind::Param(name) => visitor.visit_param(self, name),
            NodeKind::FnCall(fn_name) => visitor.visit_fn_call(self, fn_name),

            NodeKind::Insts => visitor.visit_insts(self),
        }
    }

    pub fn visit_children<V: Visitor<'src>>(&'src self, visitor: &mut V) -> ResultValue {
        if let Some(children) = &self.children {
            for child in children {
                child.visit(visitor)?;
            }
        }
        Ok(None)
    }
}

