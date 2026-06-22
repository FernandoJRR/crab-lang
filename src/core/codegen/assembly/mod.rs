pub mod x86;

#[cfg(test)]
mod tests;

use super::ir::TACInstruction;

pub trait CodegenBackend {
    fn name(&self) -> &str;
    fn generate(&self, instructions: &[TACInstruction]) -> String;
}

pub struct CodegenContext {
    backend: Box<dyn CodegenBackend>,
}

impl CodegenContext {
    pub fn new(backend: Box<dyn CodegenBackend>) -> Self {
        Self { backend }
    }

    pub fn generate(&self, instructions: &[TACInstruction]) -> String {
        self.backend.generate(instructions)
    }
}
