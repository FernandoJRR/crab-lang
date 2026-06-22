use crate::core::{
    analyzer,
    codegen::backend::{x86::X86Backend, llvm::LlvmBackend, CodegenBackend},
    facade::compiler::CompilerFacade,
    interpreter::{Interpreter, PrettyPrinter},
};

fn parse_target(args: &[String]) -> Box<dyn CodegenBackend> {
    let target = args
        .windows(2)
        .find(|w| w[0] == "--target")
        .map(|w| w[1].as_str());

    match target {
        Some("llvm") => Box::new(LlvmBackend),
        Some("x86") | None => Box::new(X86Backend),
        Some(unknown) => {
            eprintln!("Unknown target '{}'. Available: x86, llvm", unknown);
            std::process::exit(1);
        }
    }
}

pub fn run() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 {
        match args[1].as_str() {
            "compile" => {
                let file_path = args.get(2).unwrap();
                let src = std::fs::read_to_string(file_path).unwrap();
                let backend = parse_target(&args);
                let mut compiler_facade = CompilerFacade::new(backend);
                compiler_facade.compile(&src);
            }
            "run" => {
                let file_path = args.get(2).unwrap();
                let src = std::fs::read_to_string(file_path).unwrap();
                let result = analyzer::analyze(&src);
                let mut interpreter = Interpreter::new();
                interpreter.interpret(result);
            }
            "print" => {
                let file_path = args.get(2).unwrap();
                let src = std::fs::read_to_string(file_path).unwrap();
                let result = analyzer::analyze(&src);
                let mut printer = PrettyPrinter::new();
                if let (Some(result), _) = &result {
                    let _ = result.visit(&mut printer);
                }
            }
            _ => println!("Unknown command."),
        }
    } else {
        println!("No command provided.");
    }
}
