use std::collections::{HashMap, HashSet};

use crate::core::codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator};

use super::CodegenBackend;

pub struct X86Backend;

const ARG_REGISTERS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

fn emit(out: &mut String, line: &str) {
    out.push_str("    ");
    out.push_str(line);
    out.push('\n');
}

fn is_literal(value: &str) -> bool {
    value.parse::<i64>().is_ok()
        || value == "true"
        || value == "false"
        || value == "null"
}

fn is_string_literal(value: &str) -> bool {
    !is_literal(value) && !value.starts_with('t') && !value.chars().next().map_or(false, |c| c.is_alphabetic())
        || (!is_literal(value) && value.contains(' '))
}

struct StringTable {
    strings: Vec<(String, String)>,
    label_counter: usize,
    value_to_label: HashMap<String, String>,
}

impl StringTable {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            label_counter: 0,
            value_to_label: HashMap::new(),
        }
    }

    fn add(&mut self, value: &str) -> String {
        if let Some(label) = self.value_to_label.get(value) {
            return label.clone();
        }
        let label = format!("_str{}", self.label_counter);
        self.label_counter += 1;
        self.strings.push((label.clone(), value.to_string()));
        self.value_to_label.insert(value.to_string(), label.clone());
        label
    }
}

struct FunctionContext {
    stack_offsets: HashMap<String, i64>,
    stack_size: i64,
    param_count: usize,
    string_vars: HashSet<String>,
}

impl FunctionContext {
    fn new(body: &[TACInstruction]) -> Self {
        let mut offsets = HashMap::new();
        let mut slot: i64 = 0;

        let param_count = body
            .iter()
            .take_while(|inst| matches!(inst, TACInstruction::Param(_)))
            .count();

        for inst in body.iter().take(param_count) {
            if let TACInstruction::Param(name) = inst {
                slot += 8;
                offsets.insert(name.clone(), slot);
            }
        }

        for inst in body.iter().skip(param_count) {
            let dest = match inst {
                TACInstruction::Assign(dest, _)
                | TACInstruction::BinaryOp(dest, _, _, _)
                | TACInstruction::UnaryOp(dest, _, _)
                | TACInstruction::CallAssign(dest, _, _) => Some(dest.as_str()),
                _ => None,
            };
            if let Some(d) = dest {
                if !offsets.contains_key(d) {
                    slot += 8;
                    offsets.insert(d.to_string(), slot);
                }
            }
        }

        let aligned = if slot % 16 == 0 { slot } else { slot + (16 - slot % 16) };

        Self {
            stack_offsets: offsets,
            stack_size: aligned,
            param_count,
            string_vars: HashSet::new(),
        }
    }

    fn resolve(&self, operand: &str) -> String {
        if let Some(offset) = self.stack_offsets.get(operand) {
            format!("qword [rbp - {}]", offset)
        } else if operand.parse::<i64>().is_ok() {
            operand.to_string()
        } else if operand == "true" {
            "1".to_string()
        } else if operand == "false" || operand == "null" {
            "0".to_string()
        } else {
            operand.to_string()
        }
    }

    fn mark_as_string(&mut self, var: &str) {
        self.string_vars.insert(var.to_string());
    }

    fn is_string(&self, var: &str) -> bool {
        self.string_vars.contains(var)
    }
}

impl X86Backend {
    fn collect_functions(instructions: &[TACInstruction]) -> Vec<(&str, &[TACInstruction])> {
        let mut functions = Vec::new();
        let mut i = 0;

        while i < instructions.len() {
            if let TACInstruction::Label(name) = &instructions[i] {
                let start = i + 1;
                let end = instructions[start..]
                    .iter()
                    .position(|inst| matches!(inst, TACInstruction::Label(_)))
                    .map(|pos| start + pos)
                    .unwrap_or(instructions.len());
                functions.push((name.as_str(), &instructions[start..end]));
                i = end;
            } else {
                i += 1;
            }
        }

        functions
    }

    fn emit_function(name: &str, body: &[TACInstruction], strings: &mut StringTable) -> String {
        let mut ctx = FunctionContext::new(body);
        let mut out = String::new();

        out.push_str(&format!("{}:\n", name));
        emit(&mut out, "push rbp");
        emit(&mut out, "mov rbp, rsp");
        if ctx.stack_size > 0 {
            emit(&mut out, &format!("sub rsp, {}", ctx.stack_size));
        }

        for (i, inst) in body.iter().take(ctx.param_count).enumerate() {
            if let TACInstruction::Param(param_name) = inst {
                let offset = ctx.stack_offsets.get(param_name.as_str()).unwrap();
                if i < ARG_REGISTERS.len() {
                    emit(&mut out, &format!("mov qword [rbp - {}], {}", offset, ARG_REGISTERS[i]));
                } else {
                    let stack_arg_offset = 16 + (i - ARG_REGISTERS.len()) * 8;
                    emit(&mut out, &format!("mov rax, qword [rbp + {}]", stack_arg_offset));
                    emit(&mut out, &format!("mov qword [rbp - {}], rax", offset));
                }
            }
        }

        let mut pending_args: Vec<String> = Vec::new();
        let mut pending_is_string: Vec<bool> = Vec::new();

        for inst in body.iter().skip(ctx.param_count) {
            match inst {
                TACInstruction::Assign(dest, src) => {
                    let dest_offset = *ctx.stack_offsets.get(dest.as_str()).unwrap();
                    let is_str_src = !is_literal(src) && !ctx.stack_offsets.contains_key(src.as_str());
                    let propagate_str = ctx.is_string(src);
                    if is_str_src {
                        let label = strings.add(src);
                        ctx.mark_as_string(dest);
                        emit(&mut out, &format!("lea rax, [rel {}]", label));
                        emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                    } else {
                        let src_val = ctx.resolve(src);
                        if propagate_str {
                            ctx.mark_as_string(dest);
                        }
                        emit(&mut out, &format!("mov rax, {}", src_val));
                        emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                    }
                }
                TACInstruction::BinaryOp(dest, op, left, right) => {
                    let l = ctx.resolve(left);
                    let r = ctx.resolve(right);
                    let dest_offset = ctx.stack_offsets.get(dest.as_str()).unwrap();

                    match op {
                        BinaryOperator::Add => {
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, &format!("add rax, {}", r));
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        BinaryOperator::Sub => {
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, &format!("sub rax, {}", r));
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        BinaryOperator::Mult => {
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, &format!("imul rax, {}", r));
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        BinaryOperator::Div => {
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, "cqo");
                            emit(&mut out, &format!("mov rbx, {}", r));
                            emit(&mut out, "idiv rbx");
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        BinaryOperator::Eq | BinaryOperator::Neq | BinaryOperator::Gt
                        | BinaryOperator::Lt | BinaryOperator::Ge | BinaryOperator::Le => {
                            let setcc = match op {
                                BinaryOperator::Eq => "sete",
                                BinaryOperator::Neq => "setne",
                                BinaryOperator::Gt => "setg",
                                BinaryOperator::Lt => "setl",
                                BinaryOperator::Ge => "setge",
                                BinaryOperator::Le => "setle",
                                _ => unreachable!(),
                            };
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, &format!("cmp rax, {}", r));
                            emit(&mut out, &format!("{} al", setcc));
                            emit(&mut out, "movzx rax, al");
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        BinaryOperator::And => {
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, &format!("and rax, {}", r));
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        BinaryOperator::Or => {
                            emit(&mut out, &format!("mov rax, {}", l));
                            emit(&mut out, &format!("or rax, {}", r));
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                    }
                }
                TACInstruction::UnaryOp(dest, op, operand) => {
                    let val = ctx.resolve(operand);
                    let dest_offset = ctx.stack_offsets.get(dest.as_str()).unwrap();
                    match op {
                        UnaryOperator::Neg => {
                            emit(&mut out, &format!("mov rax, {}", val));
                            emit(&mut out, "neg rax");
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                        UnaryOperator::Not => {
                            emit(&mut out, &format!("mov rax, {}", val));
                            emit(&mut out, "xor rax, 1");
                            emit(&mut out, &format!("mov qword [rbp - {}], rax", dest_offset));
                        }
                    }
                }
                TACInstruction::Param(val) => {
                    let is_str = ctx.is_string(val);
                    pending_args.push(ctx.resolve(val));
                    pending_is_string.push(is_str);
                }
                TACInstruction::Call(func_name, _) => {
                    if func_name == "print" {
                        Self::emit_print_call(&mut out, &pending_args, &pending_is_string);
                    } else {
                        Self::emit_call_args(&mut out, &pending_args);
                        emit(&mut out, &format!("call {}", func_name));
                        Self::emit_call_cleanup(&mut out, pending_args.len());
                    }
                    pending_args.clear();
                    pending_is_string.clear();
                }
                TACInstruction::CallAssign(dest, func_name, _) => {
                    if func_name == "print" {
                        Self::emit_print_call(&mut out, &pending_args, &pending_is_string);
                    } else {
                        let dest_offset = ctx.stack_offsets.get(dest.as_str()).unwrap();
                        Self::emit_call_args(&mut out, &pending_args);
                        emit(&mut out, &format!("call {}", func_name));
                        Self::emit_call_cleanup(&mut out, pending_args.len());
                        emit(&mut out, &format!("mov qword [rbp - {}], rax", *dest_offset));
                    }
                    pending_args.clear();
                    pending_is_string.clear();
                }
                TACInstruction::Goto(label) => {
                    emit(&mut out, &format!("jmp {}", label));
                }
                TACInstruction::IfGoto(cond, label) => {
                    let cond_val = ctx.resolve(cond);
                    emit(&mut out, &format!("mov rax, {}", cond_val));
                    emit(&mut out, "cmp rax, 0");
                    emit(&mut out, &format!("jne {}", label));
                }
                TACInstruction::Return(Some(val)) => {
                    let ret_val = ctx.resolve(val);
                    emit(&mut out, &format!("mov rax, {}", ret_val));
                    emit(&mut out, "mov rsp, rbp");
                    emit(&mut out, "pop rbp");
                    emit(&mut out, "ret");
                }
                TACInstruction::Return(None) => {
                    emit(&mut out, "xor rax, rax");
                    emit(&mut out, "mov rsp, rbp");
                    emit(&mut out, "pop rbp");
                    emit(&mut out, "ret");
                }
                TACInstruction::Label(_) => {}
            }
        }

        out
    }

    fn emit_print_call(out: &mut String, args: &[String], is_string: &[bool]) {
        if args.is_empty() {
            return;
        }
        emit(out, &format!("mov rdi, {}", args[0]));
        if is_string.first().copied().unwrap_or(false) {
            emit(out, "call _print_str");
        } else {
            emit(out, "call _print_int");
        }
    }

    fn emit_call_args(out: &mut String, args: &[String]) {
        for (i, arg) in args.iter().enumerate() {
            if i < ARG_REGISTERS.len() {
                emit(out, &format!("mov {}, {}", ARG_REGISTERS[i], arg));
            } else {
                emit(out, &format!("push {}", arg));
            }
        }
    }

    fn emit_call_cleanup(out: &mut String, arg_count: usize) {
        if arg_count > ARG_REGISTERS.len() {
            let stack_args = arg_count - ARG_REGISTERS.len();
            emit(out, &format!("add rsp, {}", stack_args * 8));
        }
    }

    fn emit_print_int_routine() -> &'static str {
        concat!(
            "_print_int:\n",
            "    push rbp\n",
            "    mov rbp, rsp\n",
            "    push r12\n",
            "    push r13\n",
            "    sub rsp, 32\n",
            "    mov rax, rdi\n",
            "    mov r12, 0\n",
            "    cmp rax, 0\n",
            "    jge .pi_positive\n",
            "    neg rax\n",
            "    mov r12, 1\n",
            ".pi_positive:\n",
            "    lea r13, [rbp - 48]\n",
            "    mov rcx, 0\n",
            "    mov r10, 10\n",
            ".pi_loop:\n",
            "    xor rdx, rdx\n",
            "    div r10\n",
            "    add dl, '0'\n",
            "    mov byte [r13 + rcx], dl\n",
            "    inc rcx\n",
            "    cmp rax, 0\n",
            "    jne .pi_loop\n",
            "    mov rsi, 0\n",
            "    mov rdi, rcx\n",
            "    dec rdi\n",
            ".pi_reverse:\n",
            "    cmp rsi, rdi\n",
            "    jge .pi_done_reverse\n",
            "    mov al, byte [r13 + rsi]\n",
            "    mov bl, byte [r13 + rdi]\n",
            "    mov byte [r13 + rsi], bl\n",
            "    mov byte [r13 + rdi], al\n",
            "    inc rsi\n",
            "    dec rdi\n",
            "    jmp .pi_reverse\n",
            ".pi_done_reverse:\n",
            "    mov byte [r13 + rcx], 10\n",
            "    inc rcx\n",
            "    cmp r12, 1\n",
            "    jne .pi_write\n",
            "    push rcx\n",
            "    mov rax, 1\n",
            "    mov rdi, 1\n",
            "    lea rsi, [rel _minus_sign]\n",
            "    mov rdx, 1\n",
            "    syscall\n",
            "    pop rcx\n",
            ".pi_write:\n",
            "    mov rax, 1\n",
            "    mov rdi, 1\n",
            "    mov rsi, r13\n",
            "    mov rdx, rcx\n",
            "    syscall\n",
            "    add rsp, 32\n",
            "    pop r13\n",
            "    pop r12\n",
            "    pop rbp\n",
            "    ret\n",
        )
    }

    fn emit_print_str_routine() -> &'static str {
        concat!(
            "_print_str:\n",
            "    push rbp\n",
            "    mov rbp, rsp\n",
            "    mov rsi, rdi\n",
            "    xor rcx, rcx\n",
            ".ps_len:\n",
            "    cmp byte [rsi + rcx], 0\n",
            "    je .ps_write\n",
            "    inc rcx\n",
            "    jmp .ps_len\n",
            ".ps_write:\n",
            "    mov rdx, rcx\n",
            "    mov rax, 1\n",
            "    mov rdi, 1\n",
            "    syscall\n",
            "    mov rax, 1\n",
            "    mov rdi, 1\n",
            "    lea rsi, [rel _newline]\n",
            "    mov rdx, 1\n",
            "    syscall\n",
            "    mov rsp, rbp\n",
            "    pop rbp\n",
            "    ret\n",
        )
    }
}

impl CodegenBackend for X86Backend {
    fn name(&self) -> &str {
        "x86_64"
    }

    fn generate(&self, instructions: &[TACInstruction]) -> String {
        let functions = Self::collect_functions(instructions);
        let mut strings = StringTable::new();
        let mut output = String::new();

        let mut code = String::new();
        for (name, body) in &functions {
            code.push_str(&Self::emit_function(name, body, &mut strings));
            code.push('\n');
        }

        code.push_str("_start:\n");
        emit(&mut code, "call main");
        emit(&mut code, "mov rdi, rax");
        emit(&mut code, "mov rax, 60");
        emit(&mut code, "syscall");
        code.push('\n');

        code.push_str(Self::emit_print_int_routine());
        code.push('\n');
        code.push_str(Self::emit_print_str_routine());
        code.push('\n');

        output.push_str("section .data\n");
        emit(&mut output, "_newline: db 10");
        emit(&mut output, "_minus_sign: db '-'");
        for (label, value) in &strings.strings {
            emit(&mut output, &format!("{}: db \"{}\", 0", label, value));
        }
        output.push('\n');

        output.push_str("section .text\n");
        emit(&mut output, "global _start");
        output.push('\n');
        output.push_str(&code);

        output
    }
}
