use crate::core::analyzer::{Node, Type};

use super::{engine::{ResultValue, Value, Interpreter}};

pub struct Param {
    pub name: String,
    pub var_type: Type
}

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue;
}

pub struct PrintFunc;

impl Callable for PrintFunc {
    fn call(&self, _interpreter: &mut Interpreter, args: &[Value]) -> ResultValue {
        let v = args
            .first()
            .ok_or_else(|| "print: missing argument".to_string())?;
        println!("{}", v);
        Ok(None)
    }
}

pub struct UserFunc {
    pub params: Vec<Param>,
    pub body: Node,
}

impl Callable for UserFunc {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue {
        if args.len() != self.params.len() {
            return Err(format!(
                "Expected {} args, got {}",
                self.params.len(),
                args.len()
            ));
        }

        for (param, arg) in self.params.iter().zip(args.iter()) {
            let type_match = match (arg, &param.var_type) {
                (Value::Int(_), Type::Int) => true,
                (Value::Float(_), Type::Float) => true,
                (Value::Bool(_), Type::Bool) => true,

                (v, expected) => {
                    return Err(format!(
                        "Type mismatch for '{}': expected {:?}, got {:?}",
                        param.name, expected, v
                    ));
                }
            };

            if !type_match {
                return Err(format!(
                    "Type mismatch for '{}': expected {:?}, got {:?}",
                    param.name, param.var_type, arg
                ));
            }
        }

        interpreter.push_scope();

        for (param, arg) in self.params.iter().zip(args.iter()) {
            interpreter.set_var(param.name.clone(), arg.clone());
        }

        let result = self.body.visit(interpreter);

        interpreter.pop_scope();

        result
    }
}
