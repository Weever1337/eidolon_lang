pub mod errors;
pub mod interpreter;
pub mod parser;

use parser::{lexer::Lexer, Parser};

pub fn parse_eidolon_source(
    source: &str,
) -> Result<parser::ast::Program, errors::ParseError> {
    let lexer = Lexer::new(source);
    let tokens: Vec<_> = lexer.collect();
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}