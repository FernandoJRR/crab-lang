use super::*;
use crate::core::analyzer::{NodeBuilder, NodeKind};

fn span() -> chumsky::prelude::SimpleSpan {
    chumsky::prelude::SimpleSpan { start: 0, end: 0, context: () }
}

fn node(kind: NodeKind) -> Node {
    NodeBuilder::new().kind(kind).span(span()).build()
}

fn node_with_children(kind: NodeKind, children: Vec<Node>) -> Node {
    NodeBuilder::new().kind(kind).span(span()).children(children).build()
}

fn tac_from(root: &Node) -> Vec<String> {
    let mut generator = TACGenerator::new();
    generator.generate(root);
    generator.get_instructions().iter().map(|i| i.to_string()).collect()
}

// --- Literal tests ---

#[test]
fn int_literal() {
    let tac = tac_from(&node(NodeKind::Int(42)));
    assert_eq!(tac, vec!["t0 = 42"]);
}

#[test]
fn float_literal() {
    let tac = tac_from(&node(NodeKind::Float(3.14)));
    assert_eq!(tac, vec!["t0 = 3.14"]);
}

#[test]
fn bool_literal() {
    let tac = tac_from(&node(NodeKind::Bool(true)));
    assert_eq!(tac, vec!["t0 = true"]);
}

#[test]
fn null_literal() {
    let tac = tac_from(&node(NodeKind::Null));
    assert_eq!(tac, vec!["t0 = null"]);
}

#[test]
fn string_literal() {
    let tac = tac_from(&node(NodeKind::String("hello".into())));
    assert_eq!(tac, vec!["t0 = hello"]);
}

// --- Unary operator tests ---

#[test]
fn negation() {
    let ast = node_with_children(NodeKind::Neg, vec![node(NodeKind::Int(5))]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 5", "t1 = -t0"]);
}

#[test]
fn logical_not() {
    let ast = node_with_children(NodeKind::Not, vec![node(NodeKind::Bool(true))]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = true", "t1 = !t0"]);
}

// --- Binary operator tests ---

#[test]
fn addition() {
    let ast = node_with_children(
        NodeKind::Add,
        vec![node(NodeKind::Int(1)), node(NodeKind::Int(2))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 1", "t1 = 2", "t2 = t0 + t1"]);
}

#[test]
fn subtraction() {
    let ast = node_with_children(
        NodeKind::Sub,
        vec![node(NodeKind::Int(10)), node(NodeKind::Int(3))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 10", "t1 = 3", "t2 = t0 - t1"]);
}

#[test]
fn multiplication() {
    let ast = node_with_children(
        NodeKind::Mult,
        vec![node(NodeKind::Int(4)), node(NodeKind::Int(5))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 4", "t1 = 5", "t2 = t0 * t1"]);
}

#[test]
fn division() {
    let ast = node_with_children(
        NodeKind::Div,
        vec![node(NodeKind::Int(10)), node(NodeKind::Int(2))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 10", "t1 = 2", "t2 = t0 / t1"]);
}

#[test]
fn comparison_eq() {
    let ast = node_with_children(
        NodeKind::Eq,
        vec![node(NodeKind::Int(1)), node(NodeKind::Int(1))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 1", "t1 = 1", "t2 = t0 == t1"]);
}

#[test]
fn comparison_neq() {
    let ast = node_with_children(
        NodeKind::Neq,
        vec![node(NodeKind::Int(1)), node(NodeKind::Int(2))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 1", "t1 = 2", "t2 = t0 != t1"]);
}

#[test]
fn comparison_gt() {
    let ast = node_with_children(
        NodeKind::Gth,
        vec![node(NodeKind::Int(5)), node(NodeKind::Int(3))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 5", "t1 = 3", "t2 = t0 > t1"]);
}

#[test]
fn comparison_lt() {
    let ast = node_with_children(
        NodeKind::Lth,
        vec![node(NodeKind::Int(1)), node(NodeKind::Int(2))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 1", "t1 = 2", "t2 = t0 < t1"]);
}

#[test]
fn comparison_geq() {
    let ast = node_with_children(
        NodeKind::Geq,
        vec![node(NodeKind::Int(5)), node(NodeKind::Int(5))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 5", "t1 = 5", "t2 = t0 >= t1"]);
}

#[test]
fn comparison_leq() {
    let ast = node_with_children(
        NodeKind::Leq,
        vec![node(NodeKind::Int(3)), node(NodeKind::Int(5))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 3", "t1 = 5", "t2 = t0 <= t1"]);
}

#[test]
fn logical_and() {
    let ast = node_with_children(
        NodeKind::And,
        vec![node(NodeKind::Bool(true)), node(NodeKind::Bool(false))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = true", "t1 = false", "t2 = t0 && t1"]);
}

#[test]
fn logical_or() {
    let ast = node_with_children(
        NodeKind::Or,
        vec![node(NodeKind::Bool(false)), node(NodeKind::Bool(true))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = false", "t1 = true", "t2 = t0 || t1"]);
}

// --- Declaration and assignment ---

#[test]
fn declaration() {
    let ast = node_with_children(
        NodeKind::Decl,
        vec![node(NodeKind::Value("x".into())), node(NodeKind::Int(10))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 10", "x = t0"]);
}

#[test]
fn assignment() {
    let ast = node_with_children(
        NodeKind::Assign,
        vec![node(NodeKind::Value("x".into())), node(NodeKind::Int(20))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 20", "x = t0"]);
}

// --- Nested expressions ---

#[test]
fn nested_arithmetic() {
    // (1 + 2) * 3
    let add = node_with_children(
        NodeKind::Add,
        vec![node(NodeKind::Int(1)), node(NodeKind::Int(2))],
    );
    let ast = node_with_children(NodeKind::Mult, vec![add, node(NodeKind::Int(3))]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 1", "t1 = 2", "t2 = t0 + t1", "t3 = 3", "t4 = t2 * t3"]);
}

// --- Instructions block ---

#[test]
fn instruction_block() {
    let ast = node_with_children(
        NodeKind::Insts,
        vec![
            node_with_children(
                NodeKind::Decl,
                vec![node(NodeKind::Value("x".into())), node(NodeKind::Int(1))],
            ),
            node_with_children(
                NodeKind::Decl,
                vec![node(NodeKind::Value("y".into())), node(NodeKind::Int(2))],
            ),
        ],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 1", "x = t0", "t1 = 2", "y = t1"]);
}

// --- Function definition ---

#[test]
fn function_without_params() {
    let body = node_with_children(
        NodeKind::Insts,
        vec![node_with_children(
            NodeKind::Decl,
            vec![node(NodeKind::Value("x".into())), node(NodeKind::Int(1))],
        )],
    );
    let params = node(NodeKind::Params);
    let ast = node_with_children(NodeKind::Fn("main".into()), vec![params, body]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["main:", "t0 = 1", "x = t0", "return"]);
}

#[test]
fn function_with_params() {
    let param_a = node_with_children(
        NodeKind::Param("a".into()),
        vec![node(NodeKind::Type(Type::Int))],
    );
    let param_b = node_with_children(
        NodeKind::Param("b".into()),
        vec![node(NodeKind::Type(Type::Int))],
    );
    let params = node_with_children(NodeKind::Params, vec![param_a, param_b]);
    let body = node_with_children(
        NodeKind::Insts,
        vec![node_with_children(
            NodeKind::Decl,
            vec![node(NodeKind::Value("c".into())), node(NodeKind::Int(0))],
        )],
    );
    let ast = node_with_children(NodeKind::Fn("add".into()), vec![params, body]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["add:", "param a", "param b", "t0 = 0", "c = t0", "return"]);
}

#[test]
fn function_with_return_type() {
    let params = node_with_children(
        NodeKind::Params,
        vec![node_with_children(
            NodeKind::Param("x".into()),
            vec![node(NodeKind::Type(Type::Int))],
        )],
    );
    let return_type = node(NodeKind::Type(Type::Int));
    let body = node_with_children(
        NodeKind::Insts,
        vec![node_with_children(
            NodeKind::Decl,
            vec![node(NodeKind::Value("r".into())), node(NodeKind::Int(1))],
        )],
    );
    let ast = node_with_children(NodeKind::Fn("square".into()), vec![params, return_type, body]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["square:", "param x", "t0 = 1", "r = t0", "return"]);
}

// --- Function call ---

#[test]
fn function_call() {
    let ast = node_with_children(
        NodeKind::FnCall("foo".into()),
        vec![node(NodeKind::Int(42))],
    );
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = 42", "param t0", "t1 = call foo, 1"]);
}

#[test]
fn function_call_multiple_args() {
    let ast = node_with_children(
        NodeKind::FnCall("bar".into()),
        vec![node(NodeKind::Int(1)), node(NodeKind::Int(2)), node(NodeKind::Int(3))],
    );
    let tac = tac_from(&ast);
    assert_eq!(
        tac,
        vec!["t0 = 1", "t1 = 2", "t2 = 3", "param t0", "param t1", "param t2", "t3 = call bar, 3"]
    );
}

#[test]
fn function_call_no_args() {
    let ast = node_with_children(NodeKind::FnCall("nop".into()), vec![]);
    let tac = tac_from(&ast);
    assert_eq!(tac, vec!["t0 = call nop, 0"]);
}

// --- Display formatting ---

#[test]
fn display_assign() {
    assert_eq!(TACInstruction::Assign("x".into(), "5".into()).to_string(), "x = 5");
}

#[test]
fn display_label() {
    assert_eq!(TACInstruction::Label("main".into()).to_string(), "main:");
}

#[test]
fn display_goto() {
    assert_eq!(TACInstruction::Goto("L0".into()).to_string(), "goto L0");
}

#[test]
fn display_if_goto() {
    assert_eq!(TACInstruction::IfGoto("t0".into(), "L1".into()).to_string(), "if t0 goto L1");
}

#[test]
fn display_call() {
    assert_eq!(TACInstruction::Call("foo".into(), 2).to_string(), "call foo, 2");
}

#[test]
fn display_call_assign() {
    assert_eq!(TACInstruction::CallAssign("t0".into(), "foo".into(), 2).to_string(), "t0 = call foo, 2");
}

#[test]
fn display_return_value() {
    assert_eq!(TACInstruction::Return(Some("t0".into())).to_string(), "return t0");
}

#[test]
fn display_return_void() {
    assert_eq!(TACInstruction::Return(None).to_string(), "return");
}

// --- Builder tests ---

#[test]
fn builder_temp_counter() {
    let mut builder = TACBuilder::new();
    assert_eq!(builder.new_temp(), "t0");
    assert_eq!(builder.new_temp(), "t1");
    assert_eq!(builder.new_temp(), "t2");
}

#[test]
fn builder_label_counter() {
    let mut builder = TACBuilder::new();
    assert_eq!(builder.new_label(), "L0");
    assert_eq!(builder.new_label(), "L1");
    assert_eq!(builder.new_label(), "L2");
}
