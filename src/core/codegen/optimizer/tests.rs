use super::*;
use super::constant_folding::ConstantFoldingPass;
use super::constant_propagation::ConstantPropagationPass;
use super::dead_code_elimination::DeadCodeEliminationPass;
use crate::core::codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator};

fn to_strings(instructions: &[TACInstruction]) -> Vec<String> {
    instructions.iter().map(|i| i.to_string()).collect()
}

// --- Constant Propagation ---

#[test]
fn propagation_substitutes_constant() {
    let pass = ConstantPropagationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::BinaryOp("t1".into(), BinaryOperator::Add, "t0".into(), "3".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 5", "t1 = 5 + 3"]);
}

#[test]
fn propagation_chains_through_assigns() {
    let pass = ConstantPropagationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "t0".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t1".into(), "1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 10", "t1 = 10", "t2 = 10 + 1"]);
}

#[test]
fn propagation_clears_on_label() {
    let pass = ConstantPropagationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Label("L0".into()),
        TACInstruction::BinaryOp("t1".into(), BinaryOperator::Add, "t0".into(), "1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 5", "L0:", "t1 = t0 + 1"]);
}

#[test]
fn propagation_into_param() {
    let pass = ConstantPropagationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Param("t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 42", "param 42"]);
}

#[test]
fn propagation_into_unary() {
    let pass = ConstantPropagationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Neg, "t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 5", "t1 = -5"]);
}

// --- Constant Folding ---

#[test]
fn folding_int_addition() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Assign("t1".into(), "4".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 3", "t1 = 4", "t2 = 7"]);
}

#[test]
fn folding_int_subtraction() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "3".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Sub, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 10", "t1 = 3", "t2 = 7"]);
}

#[test]
fn folding_int_multiplication() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "6".into()),
        TACInstruction::Assign("t1".into(), "7".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Mult, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 6", "t1 = 7", "t2 = 42"]);
}

#[test]
fn folding_int_division() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "2".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Div, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 10", "t1 = 2", "t2 = 5"]);
}

#[test]
fn folding_skips_division_by_zero() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "10".into()),
        TACInstruction::Assign("t1".into(), "0".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Div, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 10", "t1 = 0", "t2 = t0 / t1"]);
}

#[test]
fn folding_comparison() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Assign("t1".into(), "3".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Gt, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 5", "t1 = 3", "t2 = true"]);
}

#[test]
fn folding_boolean_and() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "true".into()),
        TACInstruction::Assign("t1".into(), "false".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::And, "t0".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = true", "t1 = false", "t2 = false"]);
}

#[test]
fn folding_negation() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Neg, "t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 5", "t1 = -5"]);
}

#[test]
fn folding_not() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "true".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Not, "t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = true", "t1 = false"]);
}

#[test]
fn folding_non_constant_unchanged() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "x".into(), "y".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t2 = x + y"]);
}

#[test]
fn folding_clears_on_label() {
    let pass = ConstantFoldingPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Label("L0".into()),
        TACInstruction::UnaryOp("t1".into(), UnaryOperator::Neg, "t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 3", "L0:", "t1 = -t0"]);
}

// --- Dead Code Elimination ---

#[test]
fn dce_removes_unused_temp() {
    let pass = DeadCodeEliminationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Assign("t1".into(), "10".into()),
        TACInstruction::Assign("x".into(), "t1".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t1 = 10", "x = t1"]);
}

#[test]
fn dce_keeps_used_temp() {
    let pass = DeadCodeEliminationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Assign("x".into(), "t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 5", "x = t0"]);
}

#[test]
fn dce_preserves_non_temp_assignments() {
    let pass = DeadCodeEliminationPass;
    let input = vec![
        TACInstruction::Assign("x".into(), "5".into()),
        TACInstruction::Assign("y".into(), "10".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["x = 5", "y = 10"]);
}

#[test]
fn dce_preserves_labels_and_returns() {
    let pass = DeadCodeEliminationPass;
    let input = vec![
        TACInstruction::Label("main".into()),
        TACInstruction::Assign("t0".into(), "5".into()),
        TACInstruction::Return(None),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["main:", "return"]);
}

#[test]
fn dce_keeps_temp_used_in_param() {
    let pass = DeadCodeEliminationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "42".into()),
        TACInstruction::Param("t0".into()),
        TACInstruction::CallAssign("t1".into(), "foo".into(), 1),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 42", "param t0", "t1 = call foo, 1"]);
}

#[test]
fn dce_removes_unused_binary_op() {
    let pass = DeadCodeEliminationPass;
    let input = vec![
        TACInstruction::Assign("t0".into(), "1".into()),
        TACInstruction::BinaryOp("t1".into(), BinaryOperator::Add, "t0".into(), "t0".into()),
        TACInstruction::Assign("x".into(), "t0".into()),
    ];
    let result = to_strings(&pass.optimize(input));
    assert_eq!(result, vec!["t0 = 1", "x = t0"]);
}

// --- Pipeline ---

#[test]
fn pipeline_all_passes() {
    // t0 = 3, t1 = 4, t2 = t0 + t1, x = t2
    // After propagation: t0=3, t1=4, t2 = 3 + 4, x = t2
    // After folding: t0=3, t1=4, t2 = 7, x = t2
    // After DCE: t2 = 7, x = t2
    let mut pipeline = OptimizerPipeline::new();
    pipeline.add_pass(Box::new(ConstantPropagationPass));
    pipeline.add_pass(Box::new(ConstantFoldingPass));
    pipeline.add_pass(Box::new(DeadCodeEliminationPass));

    let input = vec![
        TACInstruction::Assign("t0".into(), "3".into()),
        TACInstruction::Assign("t1".into(), "4".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
        TACInstruction::Assign("x".into(), "t2".into()),
    ];

    let result = to_strings(&pipeline.run(input));
    assert_eq!(result, vec!["t2 = 7", "x = t2"]);
}

#[test]
fn pipeline_preserves_non_optimizable() {
    let mut pipeline = OptimizerPipeline::new();
    pipeline.add_pass(Box::new(ConstantPropagationPass));
    pipeline.add_pass(Box::new(ConstantFoldingPass));
    pipeline.add_pass(Box::new(DeadCodeEliminationPass));

    let input = vec![
        TACInstruction::Label("main".into()),
        TACInstruction::Param("a".into()),
        TACInstruction::Param("b".into()),
        TACInstruction::BinaryOp("t0".into(), BinaryOperator::Add, "a".into(), "b".into()),
        TACInstruction::Assign("x".into(), "t0".into()),
        TACInstruction::Return(None),
    ];

    let result = to_strings(&pipeline.run(input));
    assert_eq!(result, vec![
        "main:", "param a", "param b", "t0 = a + b", "x = t0", "return"
    ]);
}

#[test]
fn pipeline_empty_passes() {
    let pipeline = OptimizerPipeline::new();
    let input = vec![
        TACInstruction::Assign("t0".into(), "5".into()),
    ];
    let result = to_strings(&pipeline.run(input));
    assert_eq!(result, vec!["t0 = 5"]);
}

#[test]
fn pipeline_nested_constant_expression() {
    // let x = (1 + 2) * 3  →  TAC: t0=1, t1=2, t2=t0+t1, t3=3, t4=t2*t3, x=t4
    // Should optimize to: t4 = 9, x = t4
    let mut pipeline = OptimizerPipeline::new();
    pipeline.add_pass(Box::new(ConstantPropagationPass));
    pipeline.add_pass(Box::new(ConstantFoldingPass));
    pipeline.add_pass(Box::new(DeadCodeEliminationPass));

    let input = vec![
        TACInstruction::Assign("t0".into(), "1".into()),
        TACInstruction::Assign("t1".into(), "2".into()),
        TACInstruction::BinaryOp("t2".into(), BinaryOperator::Add, "t0".into(), "t1".into()),
        TACInstruction::Assign("t3".into(), "3".into()),
        TACInstruction::BinaryOp("t4".into(), BinaryOperator::Mult, "t2".into(), "t3".into()),
        TACInstruction::Assign("x".into(), "t4".into()),
    ];

    let result = to_strings(&pipeline.run(input));
    assert_eq!(result, vec!["t4 = 9", "x = t4"]);
}
