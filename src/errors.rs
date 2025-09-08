use crate::parser::lexer::TokenKind;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
}
impl ParseError {
    pub fn new(message: impl Into<String>) -> Self {
        ParseError {
            message: message.into(),
        }
    }

    pub fn unexpected_token(expected: &str, found: TokenKind) -> Self {
        ParseError::new(format!("Expected {}, but found {:?}", expected, found))
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse Error: {}", self.message)
    }
}

impl std::error::Error for ParseError {}