use crate::core::analyzer::Node;

use super::engine::ResultValue;

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
