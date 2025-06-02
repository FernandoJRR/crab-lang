use crate::core::{
    analyzer::{Node, Type},
    interpreter::interpolator::{Interpolator, StringInterpolator},
};

use super::engine::{Interpreter, ResultValue, Value};

pub struct CallContext<'a> {
    interpreter: &'a mut Interpreter,
    args: Vec<Value>,
    params: &'a Vec<Param>,
}

impl<'a> CallContext<'a> {
    pub fn new(interpreter: &'a mut Interpreter, args: Vec<Value>, params: &'a Vec<Param>) -> Self {
        Self {
            interpreter,
            args,
            params,
        }
    }

    pub fn enter(&mut self) -> Result<(), String> {
        if self.args.len() != self.params.len() {
            return Err(format!(
                "Expected {} args, got {}",
                self.params.len(),
                self.args.len()
            ));
        }

        for (param, arg) in self.params.iter().zip(self.args.iter()) {
            let type_match = matches!(
                (arg, &param.var_type),
                (Value::Int(_), Type::Int)
                    | (Value::Float(_), Type::Float)
                    | (Value::Bool(_), Type::Bool)
            );

            if !type_match {
                return Err(format!(
                    "Type mismatch for '{}': expected {:?}, got {:?}",
                    param.name, param.var_type, arg
                ));
            }
        }

        self.interpreter.push_scope();

        for (param, arg) in self.params.iter().zip(self.args.iter()) {
            self.interpreter.set_var(param.name.clone(), arg.clone());
        }

        Ok(())
    }

    pub fn exit(&mut self) {
        self.interpreter.pop_scope();
    }
}

pub struct Param {
    pub name: String,
    pub var_type: Type,
}

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue;
}

pub struct PrintFunc;

impl Callable for PrintFunc {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue {
        if args.len() != 1 {
            return Err(format!("print: expected 1 argument, got {}", args.len()));
        }

        match &args[0] {
            Value::String(template) => {
                let mut interpolator = StringInterpolator { interpreter };
                let output = interpolator.interpolate(template);
                println!("{}", output);
            }
            other => {
                println!("{}", other);
            }
        }

        Ok(None)
    }
}

pub struct UserFunc {
    pub params: Vec<Param>,
    pub body: Node,
}

impl Callable for UserFunc {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> ResultValue {
        let mut context = CallContext::new(interpreter, args.to_vec(), &self.params);
        context.enter()?;

        let result = self.body.visit(context.interpreter);

        context.exit();

        result
    }
}
