use std::fmt;

use crate::core::{
    analyzer::{Node, Type},
    codegen::ir::{TACBuilder, TACInstruction},
    facade::interpolator::{Interpolator, StringInterpolator},
    interpreter::{
        Interpreter,
        engine::{ResultValue, Value},
    },
};

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
    fn check(&self, args: &[Value]) -> Result<(), String>;
    fn to_tac(&self, builder: &mut TACBuilder, res_temp: &str, args: &[Value]) -> Result<(), String>;
}

impl fmt::Debug for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Callable {{ ... }}")
    }
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

    fn check(&self, args: &[Value]) -> Result<(), String> {
        if args.len() != 1 {
            Err(format!("print: expected 1 argument, got {}", args.len()))
        } else {
            Ok(())
        }
    }

    fn to_tac(&self, builder: &mut TACBuilder, res_temp: &str, args: &[Value]) -> Result<(), String> {
        self.check(args)?;
        let arg0 = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err("print: invalid arg for TAC".into()),
        };
        builder.emit(TACInstruction::Param(arg0.clone()));
        builder.emit(TACInstruction::Call("print".to_string(), 1));
        Ok(()) 
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
    fn check(&self, args: &[Value]) -> Result<(), String> {
        if args.len() != self.params.len() {
            return Err(format!(
                "Expected {} args, got {}",
                self.params.len(),
                args.len()
            ));
        }

        for (param, arg) in self.params.iter().zip(args.iter()) {
            let type_match = matches!(
                (arg, &param.var_type),
                (Value::Int(_), Type::Int)
                    | (Value::Float(_), Type::Float)
                    | (Value::Bool(_), Type::Bool)
                    | (Value::String(_), Type::String)
            );

            if !type_match {
                return Err(format!(
                    "Type mismatch for '{}': expected {:?}, got {:?}",
                    param.name, param.var_type, arg
                ));
            }
        }

        Ok(())
    }

    fn to_tac(
        &self,
        builder: &mut TACBuilder,
        result_temp: &str,
        args: &[Value],
    ) -> Result<(), String> {
        for arg in args {
            let arg_repr = match arg {
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::String(s) => format!("\"{}\"", s),
                Value::Null => "null".into(),
                Value::Param(_, _) | Value::Params(_) | Value::Type(_) => {
                    return Err("Argumento inválido para llamada".into());
                }
            };
            builder.emit(TACInstruction::Param(arg_repr));
        }

        // Emitimos la instrucción de llamada
        let arity = args.len();
        builder.emit(TACInstruction::Call(result_temp.to_string(), arity));

        Ok(())
    }
}
