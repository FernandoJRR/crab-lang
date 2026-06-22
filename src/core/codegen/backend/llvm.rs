use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::IntPredicate;

use crate::core::codegen::ir::{BinaryOperator, TACInstruction, UnaryOperator};

use super::CodegenBackend;

pub struct LlvmBackend;

impl LlvmBackend {
    fn collect_functions(instructions: &[TACInstruction]) -> Vec<&str> {
        instructions
            .iter()
            .filter_map(|inst| match inst {
                TACInstruction::Label(name) => Some(name.as_str()),
                _ => None,
            })
            .collect()
    }

    fn collect_function_instructions(instructions: &[TACInstruction]) -> Vec<(&str, &[TACInstruction])> {
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

    fn collect_variables(body: &[TACInstruction]) -> Vec<String> {
        let mut vars = Vec::new();
        let mut seen = std::collections::HashSet::new();

        for inst in body {
            let dest = match inst {
                TACInstruction::Assign(dest, _)
                | TACInstruction::BinaryOp(dest, _, _, _)
                | TACInstruction::UnaryOp(dest, _, _)
                | TACInstruction::CallAssign(dest, _, _) => Some(dest.as_str()),
                _ => None,
            };
            if let Some(d) = dest {
                if seen.insert(d.to_string()) {
                    vars.push(d.to_string());
                }
            }
        }

        vars
    }

    fn is_constant(value: &str) -> bool {
        value.parse::<i64>().is_ok()
            || value == "true"
            || value == "false"
            || value == "null"
    }

    fn const_to_i64(value: &str) -> i64 {
        if let Ok(n) = value.parse::<i64>() {
            return n;
        }
        match value {
            "true" => 1,
            "false" | "null" => 0,
            _ => 0,
        }
    }

    fn resolve_value<'ctx>(
        value: &str,
        builder: &Builder<'ctx>,
        variables: &HashMap<String, PointerValue<'ctx>>,
        context: &'ctx Context,
    ) -> IntValue<'ctx> {
        if Self::is_constant(value) {
            let i64_type = context.i64_type();
            i64_type.const_int(Self::const_to_i64(value) as u64, true)
        } else if let Some(ptr) = variables.get(value) {
            builder
                .build_load(context.i64_type(), *ptr, value)
                .unwrap()
                .into_int_value()
        } else {
            context.i64_type().const_int(0, false)
        }
    }

    fn build_function<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        fn_value: FunctionValue<'ctx>,
        body: &[TACInstruction],
        all_functions: &HashMap<&str, FunctionValue<'ctx>>,
    ) {
        let entry = context.append_basic_block(fn_value, "entry");
        builder.position_at_end(entry);

        let i64_type = context.i64_type();
        let vars = Self::collect_variables(body);
        let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();

        for var in &vars {
            let alloca = builder.build_alloca(i64_type, var).unwrap();
            variables.insert(var.clone(), alloca);
        }

        // Store function parameters into their corresponding variables
        for (i, param) in fn_value.get_params().iter().enumerate() {
            let param_name = format!("param_{}", i);
            if let Some(ptr) = variables.get(&param_name) {
                builder.build_store(*ptr, *param).unwrap();
            }
        }

        let mut pending_args: Vec<IntValue<'ctx>> = Vec::new();

        for inst in body {
            match inst {
                TACInstruction::Assign(dest, src) => {
                    let val = Self::resolve_value(src, builder, &variables, context);
                    if let Some(ptr) = variables.get(dest.as_str()) {
                        builder.build_store(*ptr, val).unwrap();
                    }
                }
                TACInstruction::BinaryOp(dest, op, left, right) => {
                    let l = Self::resolve_value(left, builder, &variables, context);
                    let r = Self::resolve_value(right, builder, &variables, context);

                    let result = match op {
                        BinaryOperator::Add => builder.build_int_add(l, r, "add").unwrap(),
                        BinaryOperator::Sub => builder.build_int_sub(l, r, "sub").unwrap(),
                        BinaryOperator::Mult => builder.build_int_mul(l, r, "mul").unwrap(),
                        BinaryOperator::Div => builder.build_int_signed_div(l, r, "div").unwrap(),
                        BinaryOperator::Eq => {
                            let cmp = builder.build_int_compare(IntPredicate::EQ, l, r, "eq").unwrap();
                            builder.build_int_z_extend(cmp, i64_type, "zext").unwrap()
                        }
                        BinaryOperator::Neq => {
                            let cmp = builder.build_int_compare(IntPredicate::NE, l, r, "neq").unwrap();
                            builder.build_int_z_extend(cmp, i64_type, "zext").unwrap()
                        }
                        BinaryOperator::Gt => {
                            let cmp = builder.build_int_compare(IntPredicate::SGT, l, r, "gt").unwrap();
                            builder.build_int_z_extend(cmp, i64_type, "zext").unwrap()
                        }
                        BinaryOperator::Lt => {
                            let cmp = builder.build_int_compare(IntPredicate::SLT, l, r, "lt").unwrap();
                            builder.build_int_z_extend(cmp, i64_type, "zext").unwrap()
                        }
                        BinaryOperator::Ge => {
                            let cmp = builder.build_int_compare(IntPredicate::SGE, l, r, "ge").unwrap();
                            builder.build_int_z_extend(cmp, i64_type, "zext").unwrap()
                        }
                        BinaryOperator::Le => {
                            let cmp = builder.build_int_compare(IntPredicate::SLE, l, r, "le").unwrap();
                            builder.build_int_z_extend(cmp, i64_type, "zext").unwrap()
                        }
                        BinaryOperator::And => builder.build_and(l, r, "and").unwrap(),
                        BinaryOperator::Or => builder.build_or(l, r, "or").unwrap(),
                    };

                    if let Some(ptr) = variables.get(dest.as_str()) {
                        builder.build_store(*ptr, result).unwrap();
                    }
                }
                TACInstruction::UnaryOp(dest, op, operand) => {
                    let val = Self::resolve_value(operand, builder, &variables, context);

                    let result = match op {
                        UnaryOperator::Neg => {
                            let zero = i64_type.const_int(0, false);
                            builder.build_int_sub(zero, val, "neg").unwrap()
                        }
                        UnaryOperator::Not => {
                            let one = i64_type.const_int(1, false);
                            builder.build_xor(val, one, "not").unwrap()
                        }
                    };

                    if let Some(ptr) = variables.get(dest.as_str()) {
                        builder.build_store(*ptr, result).unwrap();
                    }
                }
                TACInstruction::Param(val) => {
                    let arg = Self::resolve_value(val, builder, &variables, context);
                    pending_args.push(arg);
                }
                TACInstruction::Call(name, _) => {
                    if let Some(callee) = all_functions.get(name.as_str()) {
                        let args: Vec<BasicMetadataTypeEnum> = pending_args
                            .iter()
                            .map(|_| i64_type.into())
                            .collect();
                        let _ = args;
                        let call_args: Vec<inkwell::values::BasicMetadataValueEnum> = pending_args
                            .iter()
                            .map(|a| (*a).into())
                            .collect();
                        builder.build_call(*callee, &call_args, "call").unwrap();
                    }
                    pending_args.clear();
                }
                TACInstruction::CallAssign(dest, name, _) => {
                    if let Some(callee) = all_functions.get(name.as_str()) {
                        let call_args: Vec<inkwell::values::BasicMetadataValueEnum> = pending_args
                            .iter()
                            .map(|a| (*a).into())
                            .collect();
                        let call_val = builder
                            .build_call(*callee, &call_args, "call")
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_basic()
                            .into_int_value();
                        if let Some(ptr) = variables.get(dest.as_str()) {
                            builder.build_store(*ptr, call_val).unwrap();
                        }
                    }
                    pending_args.clear();
                }
                TACInstruction::Return(Some(val)) => {
                    let ret_val = Self::resolve_value(val, builder, &variables, context);
                    builder.build_return(Some(&ret_val)).unwrap();
                }
                TACInstruction::Return(None) => {
                    builder.build_return(None).unwrap();
                }
                TACInstruction::Goto(_) | TACInstruction::IfGoto(_, _) | TACInstruction::Label(_) => {}
            }
        }
    }
}

impl CodegenBackend for LlvmBackend {
    fn name(&self) -> &str {
        "llvm"
    }

    fn generate(&self, instructions: &[TACInstruction]) -> String {
        let context = Context::create();
        let module = context.create_module("crab_module");
        let builder = context.create_builder();
        let i64_type = context.i64_type();

        let fn_names = Self::collect_functions(instructions);
        let mut all_functions: HashMap<&str, FunctionValue> = HashMap::new();

        for name in &fn_names {
            let fn_type = i64_type.fn_type(&[], false);
            let fn_value = module.add_function(name, fn_type, None);
            all_functions.insert(name, fn_value);
        }

        let function_bodies = Self::collect_function_instructions(instructions);

        for (name, body) in &function_bodies {
            if let Some(fn_value) = all_functions.get(name) {
                Self::build_function(&context, &module, &builder, *fn_value, body, &all_functions);
            }
        }

        module.print_to_string().to_string()
    }
}
