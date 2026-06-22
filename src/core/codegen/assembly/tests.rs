use super::*;
use super::x86::X86Backend;
use crate::core::codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator};

fn x86(instructions: &[TACInstruction]) -> String {
    let backend = X86Backend;
    backend.generate(instructions)
}

fn contains_lines(output: &str, expected: &[&str]) {
    for line in expected {
        assert!(
            output.contains(line),
            "Expected line not found: '{}'\nGot:\n{}",
            line,
            output
        );
    }
}

// --- CodegenContext ---

#[test]
fn context_delegates_to_backend() {
    let ctx = CodegenContext::new(Box::new(X86Backend));
    let input = vec![TACInstruction::Return(None)];
    let output = ctx.generate(&input);
    assert!(output.contains("ret"));
}

// --- Data section ---

#[test]
fn emits_data_section() {
    let output = x86(&[
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Assign("x".into(), "t0".into()),
    ]);
    contains_lines(&output, &["section .data", "t0 dq 0", "x dq 0"]);
}

// --- Text section ---

#[test]
fn emits_text_section() {
    let output = x86(&[TACInstruction::Return(None)]);
    contains_lines(&output, &["section .text", "global _start"]);
}

// --- Assign ---

#[test]
fn assign_constant() {
    let output = x86(&[TACInstruction::Assign("t0".into(), "42".into())]);
    contains_lines(&output, &["mov rax, 42", "mov [t0], rax"]);
}

#[test]
fn assign_variable() {
    let output = x86(&[TACInstruction::Assign("x".into(), "t0".into())]);
    contains_lines(&output, &["mov rax, [t0]", "mov [x], rax"]);
}

#[test]
fn assign_bool_true() {
    let output = x86(&[TACInstruction::Assign("t0".into(), "true".into())]);
    contains_lines(&output, &["mov rax, 1", "mov [t0], rax"]);
}

#[test]
fn assign_bool_false() {
    let output = x86(&[TACInstruction::Assign("t0".into(), "false".into())]);
    contains_lines(&output, &["mov rax, 0", "mov [t0], rax"]);
}

// --- Binary operations ---

#[test]
fn binary_add() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Add, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["mov rax, [t0]", "add rax, [t1]", "mov [t2], rax"]);
}

#[test]
fn binary_sub() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Sub, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["mov rax, [t0]", "sub rax, [t1]", "mov [t2], rax"]);
}

#[test]
fn binary_mult() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Mult, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["imul rax, [t1]", "mov [t2], rax"]);
}

#[test]
fn binary_div() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Div, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["mov rax, [t0]", "cqo", "idiv rbx", "mov [t2], rax"]);
}

#[test]
fn binary_eq() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Eq, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["cmp rax, [t1]", "sete al", "movzx rax, al"]);
}

#[test]
fn binary_gt() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Gt, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["setg al"]);
}

#[test]
fn binary_and() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::And, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["and rax, [t1]"]);
}

#[test]
fn binary_or() {
    let output = x86(&[TACInstruction::BinaryOp(
        "t2".into(), BinaryOperator::Or, "t0".into(), "t1".into(),
    )]);
    contains_lines(&output, &["or rax, [t1]"]);
}

// --- Unary operations ---

#[test]
fn unary_neg() {
    let output = x86(&[TACInstruction::UnaryOp(
        "t1".into(), UnaryOperator::Neg, "t0".into(),
    )]);
    contains_lines(&output, &["mov rax, [t0]", "neg rax", "mov [t1], rax"]);
}

#[test]
fn unary_not() {
    let output = x86(&[TACInstruction::UnaryOp(
        "t1".into(), UnaryOperator::Not, "t0".into(),
    )]);
    contains_lines(&output, &["mov rax, [t0]", "xor rax, 1", "mov [t1], rax"]);
}

// --- Control flow ---

#[test]
fn label() {
    let output = x86(&[TACInstruction::Label("main".into())]);
    assert!(output.contains("main:"));
}

#[test]
fn goto() {
    let output = x86(&[TACInstruction::Goto("L0".into())]);
    contains_lines(&output, &["jmp L0"]);
}

#[test]
fn if_goto() {
    let output = x86(&[TACInstruction::IfGoto("t0".into(), "L1".into())]);
    contains_lines(&output, &["mov rax, [t0]", "cmp rax, 0", "jne L1"]);
}

// --- Function calls ---

#[test]
fn param() {
    let output = x86(&[TACInstruction::Param("t0".into())]);
    contains_lines(&output, &["push [t0]"]);
}

#[test]
fn param_constant() {
    let output = x86(&[TACInstruction::Param("42".into())]);
    contains_lines(&output, &["push 42"]);
}

#[test]
fn call() {
    let output = x86(&[TACInstruction::Call("foo".into(), 2)]);
    contains_lines(&output, &["call foo"]);
}

#[test]
fn call_assign() {
    let output = x86(&[TACInstruction::CallAssign("t0".into(), "foo".into(), 1)]);
    contains_lines(&output, &["call foo", "mov [t0], rax"]);
}

// --- Return ---

#[test]
fn return_value() {
    let output = x86(&[TACInstruction::Return(Some("t0".into()))]);
    contains_lines(&output, &["mov rax, [t0]", "ret"]);
}

#[test]
fn return_void() {
    let output = x86(&[TACInstruction::Return(None)]);
    assert!(output.contains("    ret"));
}

// --- Full function ---

#[test]
fn full_function() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Assign("t1".into(), "4".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
        TACInstruction::Assign("x".into(), "t2".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &[
        "section .data",
        "section .text",
        "main:",
        "mov rax, 3",
        "add rax, [t1]",
        "mov [x], rax",
        "ret",
    ]);
}
