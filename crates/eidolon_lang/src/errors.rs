use crate::parser::lexer::Token;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
}

impl ParseError {
    pub fn new(message: impl Into<String>, line: usize) -> Self {
        ParseError {
            message: message.into(),
            line,
        }
    }

    pub fn unexpected_token(expected: &str, found: &Token) -> Self {
        ParseError::new(
            format!("expected {}, but found {:?}", expected, found.kind),
            found.line,
        )
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}] parse error: {}", self.line, self.message)
    }
}

impl std::error::Error for ParseError {}