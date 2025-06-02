use crate::core::analyzer::{InterpolationPart, interpolation_parser};
use chumsky::Parser;

use super::engine::{Interpreter};

pub trait Interpolator {
    fn resolve_variable(&mut self, name: &str) -> String;

    fn interpolate(&mut self, template: &str) -> String {
        let (parts, _err) = interpolation_parser().parse(template).into_output_errors();

        let mut result = String::new();
        match parts {
            Some(parts) => {
                for part in parts {
                    match part {
                        InterpolationPart::Text(text) => result.push_str(&text),
                        InterpolationPart::Variable(name) => {
                            let var = self.resolve_variable(name);
                            result.push_str(&var);
                        }
                    }
                }
            }
            None => return "".to_string(),
        }
        result
    }
}

pub struct StringInterpolator<'a> {
    pub interpreter: &'a mut Interpreter,
}

impl Interpolator for StringInterpolator<'_> {
    fn resolve_variable(&mut self, name: &str) -> String {
        match self.interpreter.get_var(name) {
            Some(val) => val.to_string(),
            None => format!("{{{}}}", name),
        }
    }
}
