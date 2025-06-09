use crate::core::{analyzer, facade::compiler::CompilerFacade, interpreter::{Interpreter, PrettyPrinter}};
pub fn run() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        match args[1].as_str() {
            "compile" => {
                let file_path = args.get(2).unwrap();
                let src = std::fs::read_to_string(file_path).unwrap();
                let mut compiler_facade = CompilerFacade::new();
                compiler_facade.compile(&src);
            }
            "run" => {
                let file_path = args.get(2).unwrap();
                let src = std::fs::read_to_string(file_path).unwrap();
                let result = analyzer::analyze(&src);
                let mut interpreter = Interpreter::new();
                interpreter.interpret(result);
            },
            "print" => {
                let file_path = args.get(2).unwrap();
                let src = std::fs::read_to_string(file_path).unwrap();
                let result = analyzer::analyze(&src);
                let mut printer = PrettyPrinter::new();
                if let (Some(result), _) = &result {
                    let _ = result.visit(&mut printer);
                }
            },
            _ => println!("Unknown command."),
        }
    } else {
        println!("No command provided.");
    }
}
