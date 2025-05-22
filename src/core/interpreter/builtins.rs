use crate::core::analyzer::Node;

use super::{engine::{ResultValue, Value, Interpreter}};

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
    params: Vec<String>,
    body: Node,
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
        self.body.visit(interpreter)
    }
}
