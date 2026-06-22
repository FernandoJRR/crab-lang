use std::collections::HashSet;

use crate::core::codegen::ir::TACInstruction;

use super::OptimizationPass;

pub struct DeadCodeEliminationPass;

impl DeadCodeEliminationPass {
    fn is_temp(name: &str) -> bool {
        name.starts_with('t') && name[1..].parse::<usize>().is_ok()
    }

    fn collect_used_temps(instructions: &[TACInstruction]) -> HashSet<String> {
        let mut used = HashSet::new();

        for inst in instructions {
            match inst {
                TACInstruction::Assign(_, src) => {
                    used.insert(src.clone());
                }
                TACInstruction::BinaryOp(_, _, left, right) => {
                    used.insert(left.clone());
                    used.insert(right.clone());
                }
                TACInstruction::UnaryOp(_, _, operand) => {
                    used.insert(operand.clone());
                }
                TACInstruction::IfGoto(cond, _) => {
                    used.insert(cond.clone());
                }
                TACInstruction::Param(param) => {
                    used.insert(param.clone());
                }
                TACInstruction::Return(Some(val)) => {
                    used.insert(val.clone());
                }
                _ => {}
            }
        }

        used
    }
}

impl OptimizationPass for DeadCodeEliminationPass {
    fn name(&self) -> &str {
        "dead_code_elimination"
    }

    fn optimize(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        let used = Self::collect_used_temps(&instructions);

        instructions
            .into_iter()
            .filter(|inst| {
                match inst {
                    TACInstruction::Assign(dest, _)
                    | TACInstruction::BinaryOp(dest, _, _, _)
                    | TACInstruction::UnaryOp(dest, _, _) => {
                        !Self::is_temp(dest) || used.contains(dest)
                    }
                    _ => true,
                }
            })
            .collect()
    }
}
