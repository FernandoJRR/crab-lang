use std::collections::HashMap;

use crate::core::codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator};

use super::OptimizationPass;

pub struct ConstantFoldingPass;

impl ConstantFoldingPass {
    fn try_fold_binary(op: BinaryOperator, left: &str, right: &str) -> Option<String> {
        if let (Ok(l), Ok(r)) = (left.parse::<i64>(), right.parse::<i64>()) {
            return Self::fold_int(op, l, r);
        }

        if let (Ok(l), Ok(r)) = (left.parse::<f64>(), right.parse::<f64>()) {
            return Self::fold_float(op, l, r);
        }

        match op {
            BinaryOperator::And | BinaryOperator::Or => {
                Self::fold_bool(op, left, right)
            }
            _ => None,
        }
    }

    fn fold_int(op: BinaryOperator, l: i64, r: i64) -> Option<String> {
        match op {
            BinaryOperator::Add => Some((l + r).to_string()),
            BinaryOperator::Sub => Some((l - r).to_string()),
            BinaryOperator::Mult => Some((l * r).to_string()),
            BinaryOperator::Div if r != 0 => Some((l / r).to_string()),
            BinaryOperator::Eq => Some((l == r).to_string()),
            BinaryOperator::Neq => Some((l != r).to_string()),
            BinaryOperator::Gt => Some((l > r).to_string()),
            BinaryOperator::Lt => Some((l < r).to_string()),
            BinaryOperator::Ge => Some((l >= r).to_string()),
            BinaryOperator::Le => Some((l <= r).to_string()),
            _ => None,
        }
    }

    fn fold_float(op: BinaryOperator, l: f64, r: f64) -> Option<String> {
        match op {
            BinaryOperator::Add => Some((l + r).to_string()),
            BinaryOperator::Sub => Some((l - r).to_string()),
            BinaryOperator::Mult => Some((l * r).to_string()),
            BinaryOperator::Div if r != 0.0 => Some((l / r).to_string()),
            BinaryOperator::Eq => Some((l == r).to_string()),
            BinaryOperator::Neq => Some((l != r).to_string()),
            BinaryOperator::Gt => Some((l > r).to_string()),
            BinaryOperator::Lt => Some((l < r).to_string()),
            BinaryOperator::Ge => Some((l >= r).to_string()),
            BinaryOperator::Le => Some((l <= r).to_string()),
            _ => None,
        }
    }

    fn fold_bool(op: BinaryOperator, left: &str, right: &str) -> Option<String> {
        let l = left.parse::<bool>().ok()?;
        let r = right.parse::<bool>().ok()?;
        match op {
            BinaryOperator::And => Some((l && r).to_string()),
            BinaryOperator::Or => Some((l || r).to_string()),
            BinaryOperator::Eq => Some((l == r).to_string()),
            BinaryOperator::Neq => Some((l != r).to_string()),
            _ => None,
        }
    }

    fn try_fold_unary(op: UnaryOperator, operand: &str) -> Option<String> {
        match op {
            UnaryOperator::Neg => {
                if let Ok(n) = operand.parse::<i64>() {
                    return Some((-n).to_string());
                }
                if let Ok(f) = operand.parse::<f64>() {
                    return Some((-f).to_string());
                }
                None
            }
            UnaryOperator::Not => {
                operand.parse::<bool>().ok().map(|b| (!b).to_string())
            }
        }
    }
}

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &str {
        "constant_folding"
    }

    fn optimize(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        let mut constants: HashMap<String, String> = HashMap::new();
        let mut result = Vec::with_capacity(instructions.len());

        for inst in instructions {
            match inst {
                TACInstruction::Assign(ref dest, ref src) => {
                    constants.insert(dest.clone(), src.clone());
                    result.push(inst);
                }
                TACInstruction::BinaryOp(ref dest, op, ref left, ref right) => {
                    let l_val = constants.get(left).cloned().unwrap_or_else(|| left.clone());
                    let r_val = constants.get(right).cloned().unwrap_or_else(|| right.clone());

                    if let Some(folded) = Self::try_fold_binary(op, &l_val, &r_val) {
                        constants.insert(dest.clone(), folded.clone());
                        result.push(TACInstruction::Assign(dest.clone(), folded));
                    } else {
                        constants.remove(dest);
                        result.push(inst);
                    }
                }
                TACInstruction::UnaryOp(ref dest, op, ref operand) => {
                    let val = constants.get(operand).cloned().unwrap_or_else(|| operand.clone());

                    if let Some(folded) = Self::try_fold_unary(op, &val) {
                        constants.insert(dest.clone(), folded.clone());
                        result.push(TACInstruction::Assign(dest.clone(), folded));
                    } else {
                        constants.remove(dest);
                        result.push(inst);
                    }
                }
                TACInstruction::Label(_) => {
                    constants.clear();
                    result.push(inst);
                }
                _ => {
                    result.push(inst);
                }
            }
        }

        result
    }
}
