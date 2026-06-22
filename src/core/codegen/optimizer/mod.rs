pub mod constant_folding;
pub mod constant_propagation;
pub mod dead_code_elimination;

#[cfg(test)]
mod tests;

use super::ir::TACInstruction;

pub trait OptimizationPass {
    fn name(&self) -> &str;
    fn optimize(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction>;
}

pub struct OptimizationContext {
    pass: Box<dyn OptimizationPass>,
}

impl OptimizationContext {
    pub fn new(pass: Box<dyn OptimizationPass>) -> Self {
        Self { pass }
    }

    pub fn apply(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        self.pass.optimize(instructions)
    }
}

pub struct OptimizerPipeline {
    passes: Vec<OptimizationContext>,
}

impl OptimizerPipeline {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass(&mut self, pass: Box<dyn OptimizationPass>) {
        self.passes.push(OptimizationContext::new(pass));
    }

    pub fn run(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        self.passes
            .iter()
            .fold(instructions, |insts, ctx| ctx.apply(insts))
    }
}
