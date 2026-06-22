use std::collections::HashSet;

use crate::core::codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator};

use super::CodegenBackend;

pub struct X86Backend;

impl X86Backend {
    fn resolve_operand(operand: &str) -> String {
        if operand.parse::<i64>().is_ok() || operand.parse::<f64>().is_ok() {
            operand.to_string()
        } else if operand == "true" {
            "1".to_string()
        } else if operand == "false" || operand == "null" {
            "0".to_string()
        } else {
            format!("[{}]", operand)
        }
    }

    fn collect_variables(instructions: &[TACInstruction]) -> Vec<String> {
        let mut vars = HashSet::new();

        for inst in instructions {
            match inst {
                TACInstruction::Assign(dest, _)
                | TACInstruction::BinaryOp(dest, _, _, _)
                | TACInstruction::UnaryOp(dest, _, _)
                | TACInstruction::CallAssign(dest, _, _) => {
                    vars.insert(dest.clone());
                }
                _ => {}
            }
        }

        let mut sorted: Vec<String> = vars.into_iter().collect();
        sorted.sort();
        sorted
    }

    fn emit_data_section(variables: &[String]) -> String {
        let mut output = String::new();
        output.push_str("section .data\n");
        for var in variables {
            output.push_str(&format!("    {} dq 0\n", var));
        }
        output
    }

    fn emit_instruction(inst: &TACInstruction) -> String {
        match inst {
            TACInstruction::Assign(dest, src) => {
                let src_val = Self::resolve_operand(src);
                format!(
                    "    mov rax, {}\n    mov [{}], rax",
                    src_val, dest
                )
            }
            TACInstruction::BinaryOp(dest, op, left, right) => {
                let l = Self::resolve_operand(left);
                let r = Self::resolve_operand(right);
                let op_asm = match op {
                    BinaryOperator::Add => format!(
                        "    mov rax, {}\n    add rax, {}\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Sub => format!(
                        "    mov rax, {}\n    sub rax, {}\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Mult => format!(
                        "    mov rax, {}\n    imul rax, {}\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Div => format!(
                        "    mov rax, {}\n    cqo\n    mov rbx, {}\n    idiv rbx\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Eq => format!(
                        "    mov rax, {}\n    cmp rax, {}\n    sete al\n    movzx rax, al\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Neq => format!(
                        "    mov rax, {}\n    cmp rax, {}\n    setne al\n    movzx rax, al\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Gt => format!(
                        "    mov rax, {}\n    cmp rax, {}\n    setg al\n    movzx rax, al\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Lt => format!(
                        "    mov rax, {}\n    cmp rax, {}\n    setl al\n    movzx rax, al\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Ge => format!(
                        "    mov rax, {}\n    cmp rax, {}\n    setge al\n    movzx rax, al\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Le => format!(
                        "    mov rax, {}\n    cmp rax, {}\n    setle al\n    movzx rax, al\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::And => format!(
                        "    mov rax, {}\n    and rax, {}\n    mov [{}], rax",
                        l, r, dest
                    ),
                    BinaryOperator::Or => format!(
                        "    mov rax, {}\n    or rax, {}\n    mov [{}], rax",
                        l, r, dest
                    ),
                };
                op_asm
            }
            TACInstruction::UnaryOp(dest, op, operand) => {
                let val = Self::resolve_operand(operand);
                match op {
                    UnaryOperator::Neg => format!(
                        "    mov rax, {}\n    neg rax\n    mov [{}], rax",
                        val, dest
                    ),
                    UnaryOperator::Not => format!(
                        "    mov rax, {}\n    xor rax, 1\n    mov [{}], rax",
                        val, dest
                    ),
                }
            }
            TACInstruction::Label(name) => format!("{}:", name),
            TACInstruction::Goto(label) => format!("    jmp {}", label),
            TACInstruction::IfGoto(cond, label) => {
                let cond_val = Self::resolve_operand(cond);
                format!(
                    "    mov rax, {}\n    cmp rax, 0\n    jne {}",
                    cond_val, label
                )
            }
            TACInstruction::Param(val) => {
                let val_resolved = Self::resolve_operand(val);
                format!("    push {}", val_resolved)
            }
            TACInstruction::Call(name, _) => format!("    call {}", name),
            TACInstruction::CallAssign(dest, name, _) => {
                format!("    call {}\n    mov [{}], rax", name, dest)
            }
            TACInstruction::Return(Some(val)) => {
                let val_resolved = Self::resolve_operand(val);
                format!("    mov rax, {}\n    ret", val_resolved)
            }
            TACInstruction::Return(None) => "    ret".to_string(),
        }
    }
}

impl CodegenBackend for X86Backend {
    fn name(&self) -> &str {
        "x86_64"
    }

    fn generate(&self, instructions: &[TACInstruction]) -> String {
        let variables = Self::collect_variables(instructions);
        let mut output = Self::emit_data_section(&variables);

        output.push_str("\nsection .text\n");
        output.push_str("    global _start\n\n");

        for inst in instructions {
            output.push_str(&Self::emit_instruction(inst));
            output.push('\n');
        }

        output
    }
}
