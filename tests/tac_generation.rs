use crab_lang::core::{analyzer, codegen::ir::TACGenerator};

fn generate_tac(src: &str) -> Vec<String> {
    let (ast, errors) = analyzer::analyze(src);
    assert!(errors.is_empty(), "Parse errors: {:?}", errors);
    let mut generator = TACGenerator::new();
    generator.generate(&ast.unwrap());
    generator.get_instructions().iter().map(|i| i.to_string()).collect()
}

#[test]
fn simple_declaration() {
    let tac = generate_tac("fn main() { let x = 10; }");
    assert_eq!(tac[0], "main:");
    assert!(tac.contains(&"t0 = 10".to_string()));
    assert!(tac.contains(&"x = t0".to_string()));
    assert_eq!(tac.last().unwrap(), "return");
}

#[test]
fn arithmetic_expression() {
    let tac = generate_tac("fn main() { let x = 1 + 2; }");
    assert!(tac.contains(&"t0 = 1".to_string()));
    assert!(tac.contains(&"t1 = 2".to_string()));
    assert!(tac.contains(&"t2 = t0 + t1".to_string()));
    assert!(tac.contains(&"x = t2".to_string()));
}

#[test]
fn operator_precedence() {
    let tac = generate_tac("fn main() { let x = 1 + 2 * 3; }");
    assert!(tac.contains(&"t1 = 2".to_string()));
    assert!(tac.contains(&"t2 = 3".to_string()));
    assert!(tac.contains(&"t3 = t1 * t2".to_string()));
    assert!(tac.contains(&"t4 = t0 + t3".to_string()));
    assert!(tac.contains(&"x = t4".to_string()));
}

#[test]
fn negation_expression() {
    let tac = generate_tac("fn main() { let x = -5; }");
    assert!(tac.contains(&"t0 = 5".to_string()));
    assert!(tac.contains(&"t1 = -t0".to_string()));
    assert!(tac.contains(&"x = t1".to_string()));
}

#[test]
fn boolean_comparison() {
    let tac = generate_tac("fn main() { let x = 1 > 2; }");
    assert!(tac.contains(&"t2 = t0 > t1".to_string()));
}

#[test]
fn equality_operators() {
    let tac = generate_tac("fn main() { let x = 1 == 2; let y = 3 != 4; }");
    assert!(tac.iter().any(|i| i.contains("==")));
    assert!(tac.iter().any(|i| i.contains("!=")));
}

#[test]
fn variable_assignment() {
    let tac = generate_tac("fn main() { let x = 10; x = 20; }");
    assert!(tac.contains(&"x = t0".to_string()));
    assert!(tac.contains(&"t1 = 20".to_string()));
    assert!(tac.contains(&"x = t1".to_string()));
}

#[test]
fn function_with_params() {
    let tac = generate_tac("fn add(a: int, b: int) { let c = 1; }");
    assert_eq!(tac[0], "add:");
    assert_eq!(tac[1], "param a");
    assert_eq!(tac[2], "param b");
    assert_eq!(tac.last().unwrap(), "return");
}

#[test]
fn function_with_return_type() {
    let tac = generate_tac("fn square(x: int) -> int { let r = 1; }");
    assert_eq!(tac[0], "square:");
    assert_eq!(tac[1], "param x");
    assert_eq!(tac.last().unwrap(), "return");
}

#[test]
fn function_call() {
    let tac = generate_tac("fn foo(a: int) { let x = 1; } fn main() { let r = foo(42); }");
    let main_start = tac.iter().position(|i| i == "main:").unwrap();
    let main_tac: Vec<_> = tac[main_start..].to_vec();
    assert!(main_tac.iter().any(|i| i.starts_with("param ")));
    assert!(main_tac.iter().any(|i| i.contains("= call foo, 1")));
}

#[test]
fn multiple_functions() {
    let tac = generate_tac(
        "fn add(a: int, b: int) { let c = 1; } fn main() { let x = 5; }",
    );
    let add_pos = tac.iter().position(|i| i == "add:").unwrap();
    let main_pos = tac.iter().position(|i| i == "main:").unwrap();
    assert!(add_pos < main_pos);

    let returns: Vec<_> = tac.iter().filter(|i| *i == "return").collect();
    assert_eq!(returns.len(), 2);
}

#[test]
fn complex_expression_in_declaration() {
    let tac = generate_tac("fn main() { let x = (1 + 2) * (3 - 4); }");
    assert!(tac.iter().any(|i| i.contains("+")));
    assert!(tac.iter().any(|i| i.contains("-")));
    assert!(tac.iter().any(|i| i.contains("*")));
    assert!(tac.contains(&"main:".to_string()));
    assert!(tac.iter().any(|i| i.starts_with("x = ")));
}
