pub mod errors;
pub mod interpreter;
pub mod parser;

use crate::parser::ast::Program;
use crate::parser::lexer::Lexer;
use crate::parser::Parser;
use errors::ParseError;

pub fn parse_eidolon_source(source: &str) -> Result<Program, ParseError> {
    let lexer = Lexer::new(source);
    let tokens: Vec<_> = lexer.collect();
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}
