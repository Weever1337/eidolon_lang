use crate::interpreter::environment::{Vec2, Vec3};
use crate::interpreter::RuntimeError;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum EidolonValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Vec2(Vec2),
    Vec3(Vec3),
    List(Vec<EidolonValue>),
    Unit,
}

impl EidolonValue {
    pub fn as_number(&self, line: usize) -> Result<f64, RuntimeError> {
        match self {
            EidolonValue::Number(n) => Ok(*n),
            _ => Err(RuntimeError::new(
                format!("Expected a number, but found: {:?}", self),
                line,
            )),
        }
    }

    pub fn as_vec2(&self, line: usize) -> Result<Vec2, RuntimeError> {
        match self {
            EidolonValue::Vec2(v) => Ok(*v),
            _ => Err(RuntimeError::new(
                format!("Expected Vec2, but found {:?}", self),
                line,
            )),
        }
    }

    pub fn as_vec3(&self, line: usize) -> Result<Vec3, RuntimeError> {
        match self {
            EidolonValue::Vec3(v) => Ok(*v),
            _ => Err(RuntimeError::new(
                format!("Expected Vec3, but found {:?}", self),
                line,
            )),
        }
    }

    pub fn as_list(&self, line: usize) -> Result<&Vec<EidolonValue>, RuntimeError> {
        match self {
            EidolonValue::List(l) => Ok(l),
            _ => Err(RuntimeError::new(
                format!("Expected a List, but found {:?}", self),
                line,
            )),
        }
    }
}

impl fmt::Display for EidolonValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EidolonValue::Number(n) => write!(f, "{}", n),
            EidolonValue::Boolean(b) => write!(f, "{}", b),
            EidolonValue::String(s) => write!(f, "{}", s),
            EidolonValue::Vec2(v) => write!(f, "Vec2({}, {})", v.x, v.y),
            EidolonValue::Vec3(v) => write!(f, "Vec3({}, {}, {})", v.x, v.y, v.z),
            EidolonValue::List(list) => {
                let items: Vec<String> = list.iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", items.join(", "))
            }
            EidolonValue::Unit => write!(f, "()"),
        }
    }
}