use crate::core::{
    analyzer::{self},
    codegen::{
        assembly::{x86::X86Backend, CodegenContext},
        ir::TACGenerator,
        optimizer::{
            constant_folding::ConstantFoldingPass,
            constant_propagation::ConstantPropagationPass,
            dead_code_elimination::DeadCodeEliminationPass,
            OptimizerPipeline,
        },
    },
};

use super::sem_analyzer::SemanticAnalyzer;

pub struct CompilerFacade {
    sem_analyser: SemanticAnalyzer,
    tac_generator: TACGenerator,
    optimizer: OptimizerPipeline,
    codegen: CodegenContext,
}

impl CompilerFacade {
    pub fn new() -> Self {
        let mut optimizer = OptimizerPipeline::new();
        optimizer.add_pass(Box::new(ConstantPropagationPass));
        optimizer.add_pass(Box::new(ConstantFoldingPass));
        optimizer.add_pass(Box::new(DeadCodeEliminationPass));

        Self {
            sem_analyser: SemanticAnalyzer::new(),
            tac_generator: TACGenerator::new(),
            optimizer,
            codegen: CodegenContext::new(Box::new(X86Backend)),
        }
    }

    pub fn compile(&mut self, src: &str) {
        let result = analyzer::analyze(src);

        if let (Some(result), _) = &result {
            let analysis_result = result.visit(&mut self.sem_analyser);

            match analysis_result {
                Ok(None) => {
                    let (res_sym_table, res_func_table) = self.sem_analyser.get_tables();
                    println!("{:?}", res_sym_table);
                    println!("{:?}", res_func_table);

                    self.tac_generator.generate(result);

                    let tac_instructions = self.tac_generator.get_instructions().clone();
                    let optimized = self.optimizer.run(tac_instructions);
                    let assembly = self.codegen.generate(&optimized);
                    println!("{}", assembly);
                }
                Err(error) => panic!("{error}"),
                _ => panic!("Unexpected")
            }
        }
    }
}
