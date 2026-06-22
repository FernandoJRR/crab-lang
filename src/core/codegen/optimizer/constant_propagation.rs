use std::collections::HashMap;

use crate::core::codegen::ir::{TACInstruction, BinaryOperator, UnaryOperator};

use super::OptimizationPass;

pub struct ConstantPropagationPass;

impl ConstantPropagationPass {
    fn is_constant(value: &str) -> bool {
        value.parse::<u64>().is_ok()
            || value.parse::<f64>().is_ok()
            || value == "true"
            || value == "false"
            || value == "null"
    }

    fn propagate(value: &str, constants: &HashMap<String, String>) -> String {
        constants.get(value).cloned().unwrap_or_else(|| value.to_string())
    }
}

impl OptimizationPass for ConstantPropagationPass {
    fn name(&self) -> &str {
        "constant_propagation"
    }

    fn optimize(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        let mut constants: HashMap<String, String> = HashMap::new();
        let mut result = Vec::with_capacity(instructions.len());

        for inst in instructions {
            let new_inst = match inst {
                TACInstruction::Assign(ref dest, ref src) => {
                    let propagated_src = Self::propagate(src, &constants);
                    if Self::is_constant(&propagated_src) {
                        constants.insert(dest.clone(), propagated_src.clone());
                    } else {
                        constants.remove(dest);
                    }
                    TACInstruction::Assign(dest.clone(), propagated_src)
                }
                TACInstruction::BinaryOp(ref dest, op, ref left, ref right) => {
                    let prop_left = Self::propagate(left, &constants);
                    let prop_right = Self::propagate(right, &constants);
                    constants.remove(dest);
                    TACInstruction::BinaryOp(dest.clone(), op, prop_left, prop_right)
                }
                TACInstruction::UnaryOp(ref dest, op, ref operand) => {
                    let prop_operand = Self::propagate(operand, &constants);
                    constants.remove(dest);
                    TACInstruction::UnaryOp(dest.clone(), op, prop_operand)
                }
                TACInstruction::IfGoto(ref cond, ref label) => {
                    let prop_cond = Self::propagate(cond, &constants);
                    TACInstruction::IfGoto(prop_cond, label.clone())
                }
                TACInstruction::Param(ref param) => {
                    let prop_param = Self::propagate(param, &constants);
                    TACInstruction::Param(prop_param)
                }
                TACInstruction::Label(_) => {
                    constants.clear();
                    inst
                }
                _ => inst,
            };
            result.push(new_inst);
        }

        result
    }
}
