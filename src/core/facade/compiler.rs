use crate::core::{analyzer::{self}, codegen::ir::TACGenerator};

use super::sem_analyzer::SemanticAnalyzer;

pub struct CompilerFacade {
    sem_analyser: SemanticAnalyzer,
    tac_generator: TACGenerator,
}

impl CompilerFacade {
    pub fn new() -> Self {
        Self {
            sem_analyser: SemanticAnalyzer::new(),
            tac_generator: TACGenerator::new()
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

                    let tac_instructions = self.tac_generator.get_instructions();
                    println!("{:?}", tac_instructions);
                }
                Err(error) => panic!("{error}"),
                _ => panic!("Unexpected")
            }
        }
    }
}
