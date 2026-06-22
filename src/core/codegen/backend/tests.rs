use super::*;
use super::x86::X86Backend;
use super::llvm::LlvmBackend;
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

// --- Structure ---

#[test]
fn x86_emits_text_section() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["section .text", "global _start"]);
}

#[test]
fn x86_emits_start_entry() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["_start:", "call main", "mov rax, 60", "syscall"]);
}

#[test]
fn x86_function_prologue() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["main:", "push rbp", "mov rbp, rsp"]);
}

#[test]
fn x86_function_epilogue() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["mov rsp, rbp", "pop rbp", "ret"]);
}

// --- Assign ---

#[test]
fn x86_assign_constant() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["mov rax, 42", "mov qword [rbp - 8], rax"]);
}

#[test]
fn x86_assign_bool_true() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "true".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["mov rax, 1"]);
}

// --- Binary operations ---

#[test]
fn x86_binary_add() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Assign("t1".into(), "4".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["add rax,"]);
}

#[test]
fn x86_binary_sub() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "3".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Sub, "t0".into(), "t1".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["sub rax,"]);
}

#[test]
fn x86_binary_mult() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "4".into()),
        TACInstruction::Assign("t1".into(), "5".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Mult, "t0".into(), "t1".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["imul rax,"]);
}

#[test]
fn x86_binary_div() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "2".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Div, "t0".into(), "t1".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["cqo", "idiv rbx"]);
}

#[test]
fn x86_binary_eq() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "1".into()),
        TACInstruction::Assign("t1".into(), "1".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Eq, "t0".into(), "t1".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["sete al", "movzx rax, al"]);
}

#[test]
fn x86_binary_gt() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Assign("t1".into(), "3".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Gt, "t0".into(), "t1".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["setg al"]);
}

// --- Unary operations ---

#[test]
fn x86_unary_neg() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Neg, "t0".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["neg rax"]);
}

#[test]
fn x86_unary_not() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "true".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Not, "t0".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["xor rax, 1"]);
}

// --- Function calls ---

#[test]
fn x86_call_with_args() {
    let output = x86(&[
        TACInstruction::Label("foo".into()),
        TACInstruction::Param("a".into()),
        TACInstruction::Return(None),
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Param("t0".into()),
        TACInstruction::CallAssign("t1".into(), "foo".into(), 1),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["mov rdi,", "call foo"]);
}

#[test]
fn x86_return_value() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Return(Some("t0".into())),
    ]);
    contains_lines(&output, &["mov rax, qword [rbp - 8]", "ret"]);
}

// --- Full function ---

#[test]
fn x86_full_function() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Assign("t1".into(), "4".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
        TACInstruction::Assign("x".into(), "t2".into()),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &[
        "section .text",
        "main:",
        "push rbp",
        "mov rbp, rsp",
        "sub rsp,",
        "mov rax, 3",
        "add rax,",
        "mov rsp, rbp",
        "pop rbp",
        "ret",
        "_start:",
        "call main",
        "mov rax, 60",
        "syscall",
    ]);
}

#[test]
fn x86_function_with_params() {
    let output = x86(&[
        TACInstruction::Label("add_fn".into()),
        TACInstruction::Param("a".into()),
        TACInstruction::Param("b".into()),
        TACInstruction::BinaryOp("t0".into(), BinaryOperator::Add, "a".into(), "b".into()),
        TACInstruction::Return(Some("t0".into())),
    ]);
    contains_lines(&output, &[
        "add_fn:",
        "push rbp",
        "mov qword [rbp - 8], rdi",
        "mov qword [rbp - 16], rsi",
        "add rax,",
        "ret",
    ]);
}

// =====================
// LLVM Backend tests
// =====================

fn llvm_gen(instructions: &[TACInstruction]) -> String {
    let backend = LlvmBackend;
    backend.generate(instructions)
}

#[test]
fn llvm_context_delegates() {
    let ctx = CodegenContext::new(Box::new(LlvmBackend));
    let input = vec![
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ];
    let output = ctx.generate(&input);
    assert!(output.contains("define"));
    assert!(output.contains("ret"));
}

#[test]
fn llvm_simple_function() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ]);
    assert!(output.contains("define i64 @main()"));
    assert!(output.contains("ret void"));
}

#[test]
fn llvm_assign_constant() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Return(Some("t0".into())),
    ]);
    assert!(output.contains("store i64 42"));
    assert!(output.contains("alloca i64"));
    assert!(output.contains("ret i64"));
}

#[test]
fn llvm_addition() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Assign("t1".into(), "4".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
        TACInstruction::Return(Some("t2".into())),
    ]);
    assert!(output.contains("add i64"));
    assert!(output.contains("ret i64"));
}

#[test]
fn llvm_subtraction() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "3".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Sub, "t0".into(), "t1".into()),
        TACInstruction::Return(Some("t2".into())),
    ]);
    assert!(output.contains("sub i64"));
}

#[test]
fn llvm_multiplication() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "6".into()),
        TACInstruction::Assign("t1".into(), "7".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Mult, "t0".into(), "t1".into()),
        TACInstruction::Return(Some("t2".into())),
    ]);
    assert!(output.contains("mul i64"));
}

#[test]
fn llvm_division() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "2".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Div, "t0".into(), "t1".into()),
        TACInstruction::Return(Some("t2".into())),
    ]);
    assert!(output.contains("sdiv i64"));
}

#[test]
fn llvm_comparison() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Assign("t1".into(), "3".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Gt, "t0".into(), "t1".into()),
        TACInstruction::Return(Some("t2".into())),
    ]);
    assert!(output.contains("icmp sgt i64"));
    assert!(output.contains("zext i1"));
}

#[test]
fn llvm_negation() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Neg, "t0".into()),
        TACInstruction::Return(Some("t1".into())),
    ]);
    assert!(output.contains("sub i64 0"));
}

#[test]
fn llvm_logical_not() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "true".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Not, "t0".into()),
        TACInstruction::Return(Some("t1".into())),
    ]);
    assert!(output.contains("xor i64"));
}

#[test]
fn llvm_function_call() {
    let output = llvm_gen(&[
        TACInstruction::Label("foo".into()),
        TACInstruction::Return(Some("42".into())),
        TACInstruction::Label("main".into()),
        TACInstruction::CallAssign("t0".into(), "foo".into(), 0),
        TACInstruction::Return(Some("t0".into())),
    ]);
    assert!(output.contains("define i64 @foo()"));
    assert!(output.contains("define i64 @main()"));
    assert!(output.contains("call i64 @foo()"));
}

#[test]
fn llvm_multiple_functions() {
    let output = llvm_gen(&[
        TACInstruction::Label("add".into()),
        TACInstruction::Return(None),
        TACInstruction::Label("main".into()),
        TACInstruction::Return(None),
    ]);
    assert!(output.contains("define i64 @add()"));
    assert!(output.contains("define i64 @main()"));
}

#[test]
fn llvm_backend_name() {
    let backend = LlvmBackend;
    assert_eq!(backend.name(), "llvm");
}

#[test]
fn llvm_print_int() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Param("t0".into()),
        TACInstruction::Call("print".into(), 1),
        TACInstruction::Return(None),
    ]);
    assert!(output.contains("declare i32 @printf"));
    assert!(output.contains("call i32"));
    assert!(output.contains("fmt_int"));
}

#[test]
fn llvm_print_string() {
    let output = llvm_gen(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "hello world".into()),
        TACInstruction::Param("t0".into()),
        TACInstruction::Call("print".into(), 1),
        TACInstruction::Return(None),
    ]);
    assert!(output.contains("hello world"));
    assert!(output.contains("call i32"));
    assert!(output.contains("fmt_str"));
}

// --- x86 print tests ---

#[test]
fn x86_print_int() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Param("t0".into()),
        TACInstruction::Call("print".into(), 1),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["call _print_int", "_print_int:"]);
}

#[test]
fn x86_print_string() {
    let output = x86(&[
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "hello world".into()),
        TACInstruction::Param("t0".into()),
        TACInstruction::Call("print".into(), 1),
        TACInstruction::Return(None),
    ]);
    contains_lines(&output, &["call _print_str", "_print_str:", "hello world"]);
}
