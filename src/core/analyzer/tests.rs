use super::*;
use chumsky::Parser as _;

fn lex(src: &str) -> Vec<Token> {
    let (tokens, errors) = lexer().parse(src).into_output_errors();
    assert!(errors.is_empty(), "Lexer errors: {:?}", errors);
    tokens.unwrap().into_iter().map(|(tok, _)| tok).collect()
}

fn parse(src: &str) -> Node {
    let (ast, errors) = analyze(src);
    assert!(errors.is_empty(), "Parse errors: {:?}", errors);
    ast.unwrap()
}

fn first_fn_body_expr(src: &str) -> Node {
    let root = parse(src);
    let func = &root.children.as_ref().unwrap()[0];
    let body = func
        .children
        .as_ref()
        .unwrap()
        .iter()
        .find(|c| matches!(c.kind, NodeKind::Insts))
        .unwrap();
    body.children.as_ref().unwrap()[0].clone()
}

// Lexer tests

#[test]
fn lex_integer() {
    let tokens = lex("42");
    assert_eq!(tokens, vec![Token::Int(42)]);
}

#[test]
fn lex_float() {
    let tokens = lex("3.14");
    assert_eq!(tokens, vec![Token::Float(3.14)]);
}

#[test]
fn lex_bool_true() {
    let tokens = lex("true");
    assert_eq!(tokens, vec![Token::Bool(true)]);
}

#[test]
fn lex_bool_false() {
    let tokens = lex("false");
    assert_eq!(tokens, vec![Token::Bool(false)]);
}

#[test]
fn lex_null() {
    let tokens = lex("null");
    assert_eq!(tokens, vec![Token::Null]);
}

#[test]
fn lex_string() {
    let tokens = lex("\"hello\"");
    assert_eq!(tokens, vec![Token::String("hello")]);
}

#[test]
fn lex_variable() {
    let tokens = lex("myVar");
    assert_eq!(tokens, vec![Token::Var("myVar")]);
}

#[test]
fn lex_let_keyword() {
    let tokens = lex("let");
    assert_eq!(tokens, vec![Token::Let]);
}

#[test]
fn lex_fn_keyword() {
    let tokens = lex("fn");
    assert_eq!(tokens, vec![Token::Fn]);
}

#[test]
fn lex_type_int() {
    let tokens = lex("int");
    assert_eq!(tokens, vec![Token::Type(Type::Int)]);
}

#[test]
fn lex_type_float() {
    let tokens = lex("float");
    assert_eq!(tokens, vec![Token::Type(Type::Float)]);
}

#[test]
fn lex_type_bool() {
    let tokens = lex("bool");
    assert_eq!(tokens, vec![Token::Type(Type::Bool)]);
}

#[test]
fn lex_arithmetic_operators() {
    let tokens = lex("+ - * /");
    assert_eq!(
        tokens,
        vec![
            Token::Op("+"),
            Token::Op("-"),
            Token::Op("*"),
            Token::Op("/"),
        ]
    );
}

#[test]
fn lex_comparison_operators() {
    let tokens = lex("== != > < >= <=");
    assert_eq!(
        tokens,
        vec![
            Token::Op("=="),
            Token::Op("!="),
            Token::Op(">"),
            Token::Op("<"),
            Token::Op(">="),
            Token::Op("<="),
        ]
    );
}

#[test]
fn lex_arrow_operator() {
    let tokens = lex("->");
    assert_eq!(tokens, vec![Token::Op("->")]);
}

#[test]
fn lex_not_operator() {
    let tokens = lex("!");
    assert_eq!(tokens, vec![Token::Op("!")]);
}

#[test]
fn lex_control_tokens() {
    let tokens = lex("( ) { } ; , :");
    assert_eq!(
        tokens,
        vec![
            Token::Ctrl("("),
            Token::Ctrl(")"),
            Token::Ctrl("{"),
            Token::Ctrl("}"),
            Token::Ctrl(";"),
            Token::Ctrl(","),
            Token::Ctrl(":"),
        ]
    );
}

#[test]
fn lex_comment_ignored() {
    let tokens = lex("42 // this is a comment\n 10");
    assert_eq!(tokens, vec![Token::Int(42), Token::Int(10)]);
}

#[test]
fn lex_declaration_statement() {
    let tokens = lex("let x = 10;");
    assert_eq!(
        tokens,
        vec![
            Token::Let,
            Token::Var("x"),
            Token::Op("="),
            Token::Int(10),
            Token::Ctrl(";"),
        ]
    );
}

#[test]
fn lex_function_definition() {
    let tokens = lex("fn add(a: int) { }");
    assert_eq!(
        tokens,
        vec![
            Token::Fn,
            Token::Var("add"),
            Token::Ctrl("("),
            Token::Var("a"),
            Token::Ctrl(":"),
            Token::Type(Type::Int),
            Token::Ctrl(")"),
            Token::Ctrl("{"),
            Token::Ctrl("}"),
        ]
    );
}

// Parser tests

#[test]
fn parse_int_literal() {
    let expr = first_fn_body_expr("fn main() { 42 }");
    assert!(matches!(expr.kind, NodeKind::Int(42)));
}

#[test]
fn parse_float_literal() {
    let expr = first_fn_body_expr("fn main() { 3.14 }");
    assert!(matches!(expr.kind, NodeKind::Float(f) if (f - 3.14).abs() < f64::EPSILON));
}

#[test]
fn parse_bool_literal() {
    let expr = first_fn_body_expr("fn main() { true }");
    assert!(matches!(expr.kind, NodeKind::Bool(true)));
}

#[test]
fn parse_null_literal() {
    let expr = first_fn_body_expr("fn main() { null }");
    assert!(matches!(expr.kind, NodeKind::Null));
}

#[test]
fn parse_string_literal() {
    let expr = first_fn_body_expr("fn main() { \"hello\" }");
    assert!(matches!(expr.kind, NodeKind::String(ref s) if s == "hello"));
}

#[test]
fn parse_variable_reference() {
    let expr = first_fn_body_expr("fn main() { x }");
    assert!(matches!(expr.kind, NodeKind::Value(ref s) if s == "x"));
}

#[test]
fn parse_addition() {
    let expr = first_fn_body_expr("fn main() { 1 + 2 }");
    assert!(matches!(expr.kind, NodeKind::Add));
    let children = expr.children.as_ref().unwrap();
    assert_eq!(children.len(), 2);
    assert!(matches!(children[0].kind, NodeKind::Int(1)));
    assert!(matches!(children[1].kind, NodeKind::Int(2)));
}

#[test]
fn parse_subtraction() {
    let expr = first_fn_body_expr("fn main() { 10 - 3 }");
    assert!(matches!(expr.kind, NodeKind::Sub));
}

#[test]
fn parse_multiplication() {
    let expr = first_fn_body_expr("fn main() { 4 * 5 }");
    assert!(matches!(expr.kind, NodeKind::Mult));
}

#[test]
fn parse_division() {
    let expr = first_fn_body_expr("fn main() { 10 / 2 }");
    assert!(matches!(expr.kind, NodeKind::Div));
}

#[test]
fn parse_operator_precedence() {
    // 1 + 2 * 3  should parse as  1 + (2 * 3)
    let expr = first_fn_body_expr("fn main() { 1 + 2 * 3 }");
    assert!(matches!(expr.kind, NodeKind::Add));
    let children = expr.children.as_ref().unwrap();
    assert!(matches!(children[0].kind, NodeKind::Int(1)));
    assert!(matches!(children[1].kind, NodeKind::Mult));
    let mult_children = children[1].children.as_ref().unwrap();
    assert!(matches!(mult_children[0].kind, NodeKind::Int(2)));
    assert!(matches!(mult_children[1].kind, NodeKind::Int(3)));
}

#[test]
fn parse_grouped_expression() {
    // (1 + 2) * 3  should parse as  (1 + 2) * 3
    let expr = first_fn_body_expr("fn main() { (1 + 2) * 3 }");
    assert!(matches!(expr.kind, NodeKind::Mult));
    let children = expr.children.as_ref().unwrap();
    assert!(matches!(children[0].kind, NodeKind::Add));
    assert!(matches!(children[1].kind, NodeKind::Int(3)));
}

#[test]
fn parse_negation() {
    let expr = first_fn_body_expr("fn main() { -5 }");
    assert!(matches!(expr.kind, NodeKind::Neg));
    let children = expr.children.as_ref().unwrap();
    assert!(matches!(children[0].kind, NodeKind::Int(5)));
}

#[test]
fn parse_not() {
    let expr = first_fn_body_expr("fn main() { !true }");
    assert!(matches!(expr.kind, NodeKind::Not));
    let children = expr.children.as_ref().unwrap();
    assert!(matches!(children[0].kind, NodeKind::Bool(true)));
}

#[test]
fn parse_comparison_eq() {
    let expr = first_fn_body_expr("fn main() { 1 == 2 }");
    assert!(matches!(expr.kind, NodeKind::Eq));
}

#[test]
fn parse_comparison_neq() {
    let expr = first_fn_body_expr("fn main() { 1 != 2 }");
    assert!(matches!(expr.kind, NodeKind::Neq));
}

#[test]
fn parse_comparison_gt() {
    let expr = first_fn_body_expr("fn main() { 1 > 2 }");
    assert!(matches!(expr.kind, NodeKind::Gth));
}

#[test]
fn parse_comparison_lt() {
    let expr = first_fn_body_expr("fn main() { 1 < 2 }");
    assert!(matches!(expr.kind, NodeKind::Lth));
}

#[test]
fn parse_comparison_geq() {
    let expr = first_fn_body_expr("fn main() { 1 >= 2 }");
    assert!(matches!(expr.kind, NodeKind::Geq));
}

#[test]
fn parse_comparison_leq() {
    let expr = first_fn_body_expr("fn main() { 1 <= 2 }");
    assert!(matches!(expr.kind, NodeKind::Leq));
}

#[test]
fn parse_declaration() {
    let stmt = first_fn_body_expr("fn main() { let x = 10; }");
    assert!(matches!(stmt.kind, NodeKind::Decl));
    let children = stmt.children.as_ref().unwrap();
    assert_eq!(children.len(), 2);
    assert!(matches!(children[0].kind, NodeKind::Value(ref s) if s == "x"));
    assert!(matches!(children[1].kind, NodeKind::Int(10)));
}

#[test]
fn parse_assignment() {
    let root = parse("fn main() { let x = 10; x = 20; }");
    let func = &root.children.as_ref().unwrap()[0];
    let body = func
        .children
        .as_ref()
        .unwrap()
        .iter()
        .find(|c| matches!(c.kind, NodeKind::Insts))
        .unwrap();
    let stmts = body.children.as_ref().unwrap();
    assert!(matches!(stmts[0].kind, NodeKind::Decl));
    assert!(matches!(stmts[1].kind, NodeKind::Assign));
    let assign_children = stmts[1].children.as_ref().unwrap();
    assert!(matches!(assign_children[0].kind, NodeKind::Value(ref s) if s == "x"));
    assert!(matches!(assign_children[1].kind, NodeKind::Int(20)));
}

#[test]
fn parse_function_definition() {
    let root = parse("fn add(a: int, b: int) { let c = 1; }");
    let func = &root.children.as_ref().unwrap()[0];
    assert!(matches!(func.kind, NodeKind::Fn(ref name) if name == "add"));

    let children = func.children.as_ref().unwrap();
    assert!(matches!(children[0].kind, NodeKind::Params));

    let params = children[0].children.as_ref().unwrap();
    assert_eq!(params.len(), 2);
    assert!(matches!(params[0].kind, NodeKind::Param(ref s) if s == "a"));
    assert!(matches!(params[1].kind, NodeKind::Param(ref s) if s == "b"));
}

#[test]
fn parse_function_with_return_type() {
    let root = parse("fn square(x: int) -> int { let r = 1; }");
    let func = &root.children.as_ref().unwrap()[0];
    assert!(matches!(func.kind, NodeKind::Fn(ref name) if name == "square"));

    let children = func.children.as_ref().unwrap();
    assert_eq!(children.len(), 3); // params, return_type, body
    assert!(matches!(children[1].kind, NodeKind::Type(Type::Int)));
}

#[test]
fn parse_function_call() {
    let expr = first_fn_body_expr("fn main() { foo(1, 2) }");
    assert!(matches!(expr.kind, NodeKind::FnCall(ref name) if name == "foo"));
    let args = expr.children.as_ref().unwrap();
    assert_eq!(args.len(), 2);
    assert!(matches!(args[0].kind, NodeKind::Int(1)));
    assert!(matches!(args[1].kind, NodeKind::Int(2)));
}

#[test]
fn parse_function_call_no_args() {
    let expr = first_fn_body_expr("fn main() { foo() }");
    assert!(matches!(expr.kind, NodeKind::FnCall(ref name) if name == "foo"));
    let args = expr.children.as_ref().unwrap();
    assert_eq!(args.len(), 0);
}

#[test]
fn parse_multiple_functions() {
    let root = parse("fn foo() { let x = 1; } fn bar() { let y = 2; }");
    assert!(matches!(root.kind, NodeKind::Insts));
    let funcs = root.children.as_ref().unwrap();
    assert_eq!(funcs.len(), 2);
    assert!(matches!(funcs[0].kind, NodeKind::Fn(ref name) if name == "foo"));
    assert!(matches!(funcs[1].kind, NodeKind::Fn(ref name) if name == "bar"));
}

// Interpolation parser tests

#[test]
fn interpolation_plain_text() {
    let parts = interpolation_parser()
        .parse("hello world")
        .into_output()
        .unwrap();
    assert_eq!(parts.len(), 1);
    assert!(matches!(&parts[0], InterpolationPart::Text(s) if s == "hello world"));
}

#[test]
fn interpolation_single_variable() {
    let parts = interpolation_parser()
        .parse("{name}")
        .into_output()
        .unwrap();
    assert_eq!(parts.len(), 1);
    assert!(matches!(parts[0], InterpolationPart::Variable("name")));
}

#[test]
fn interpolation_mixed() {
    let parts = interpolation_parser()
        .parse("hello {name} world")
        .into_output()
        .unwrap();
    assert_eq!(parts.len(), 3);
    assert!(matches!(&parts[0], InterpolationPart::Text(s) if s == "hello "));
    assert!(matches!(parts[1], InterpolationPart::Variable("name")));
    assert!(matches!(&parts[2], InterpolationPart::Text(s) if s == " world"));
}

#[test]
fn interpolation_multiple_variables() {
    let parts = interpolation_parser()
        .parse("{a} and {b}")
        .into_output()
        .unwrap();
    assert_eq!(parts.len(), 3);
    assert!(matches!(parts[0], InterpolationPart::Variable("a")));
    assert!(matches!(&parts[1], InterpolationPart::Text(s) if s == " and "));
    assert!(matches!(parts[2], InterpolationPart::Variable("b")));
}
