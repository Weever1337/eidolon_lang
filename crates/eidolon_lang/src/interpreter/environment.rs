use super::value::EidolonValue;
use crate::interpreter::RuntimeError;
use crate::parser::ast::FunctionDef;
use std::collections::HashMap;

pub type NativeFunction = fn(Vec<EidolonValue>, usize) -> Result<EidolonValue, RuntimeError>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec2 {
    pub x: f64,
    pub y: f64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

pub struct Environment<'a> {
    pub variables: HashMap<String, EidolonValue>,
    pub functions: HashMap<String, FunctionDef>,
    pub native_functions: HashMap<String, NativeFunction>,
    pub constants: HashMap<String, EidolonValue>,
    pub globals: &'a HashMap<String, EidolonValue>,
}

impl<'a> Environment<'a> {
    pub fn new(globals: &'a HashMap<String, EidolonValue>) -> Self {
        Environment {
            variables: HashMap::new(),
            functions: HashMap::new(),
            native_functions: HashMap::new(),
            constants: HashMap::new(),
            globals,
        }
    }

    pub fn get_var(&self, name: &str) -> Option<EidolonValue> {
        self.variables
            .get(name)
            .or_else(|| self.constants.get(name))
            .cloned()
    }

    pub fn set_var(&mut self, name: String, value: EidolonValue) {
        self.variables.insert(name, value);
    }
}