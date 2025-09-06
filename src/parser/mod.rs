pub mod ast;
pub mod lexer;

use crate::errors::ParseError;
use ast::*;
use lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> &TokenKind {
        self.tokens.get(self.pos).map_or(&TokenKind::Eof, |t| &t.kind)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if *self.peek() == expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::unexpected_token(&format!("{:?}", expected), self.peek().clone()))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError::unexpected_token("Identifier", self.peek().clone())),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();
        while self.peek() == &TokenKind::Let || self.peek() == &TokenKind::Fun {
            statements.push(self.parse_statement()?);
        }

        let final_expr = self.parse_expression(0)?;
        Ok(Program { statements, final_expr })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            TokenKind::Let => self.parse_let_binding().map(Statement::LetBinding),
            TokenKind::Fun => self.parse_function_def().map(Statement::FunctionDef),
            _ => Err(ParseError::unexpected_token("let or fun", self.peek().clone())),
        }
    }

    fn parse_let_binding(&mut self) -> Result<LetBinding, ParseError> {
        self.expect(TokenKind::Let)?;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expression(0)?;
        Ok(LetBinding { name, value })
    }

    fn parse_function_def(&mut self) -> Result<FunctionDef, ParseError> {
        self.expect(TokenKind::Fun)?;
        let name = self.expect_identifier()?;

        self.expect(TokenKind::LBracket)?;
        let mut params = Vec::new();
        if self.peek() != &TokenKind::RBracket {
            params.push(self.expect_identifier()?);
            while self.peek() == &TokenKind::Comma {
                self.advance();
                params.push(self.expect_identifier()?);
            }
        }
        self.expect(TokenKind::RBracket)?;
        self.expect(TokenKind::Equals)?;

        let body = if self.peek() == &TokenKind::LBrace {
            self.advance();
            let mut statements = Vec::new();
            while self.peek() == &TokenKind::Let {
                statements.push(self.parse_statement()?);
            }
            let final_expr = self.parse_expression(0)?;
            self.expect(TokenKind::RBrace)?;
            BlockBody { statements, final_expr: Box::new(final_expr) }
        } else {
            let expr = self.parse_expression(0)?;
            BlockBody { statements: vec![], final_expr: Box::new(expr) }
        };

        Ok(FunctionDef { name, params, body })
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.peek().clone() {
            TokenKind::Number(n) => {
                self.advance();
                Ok(Expression::Literal(n))
            }
            TokenKind::String(s) => {
                self.advance();
                Ok(Expression::StringLiteral(s))
            }
            TokenKind::Identifier(name) => {
                self.advance();
                if self.peek() == &TokenKind::LBracket {
                    self.parse_function_call(name)
                } else {
                    Ok(Expression::Variable(name))
                }
            }
            TokenKind::GlobalIdentifier(name) => {
                self.advance();
                Ok(Expression::GlobalVariable(name))
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::Minus => {
                self.advance();
                let op = UnaryOperator::Negate;
                let expr = self.parse_expression(Precedence::Unary as u8)?;
                Ok(Expression::UnaryOp(UnaryOp { op, expr: Box::new(expr) }))
            }
            TokenKind::If => self.parse_if_else(),
            TokenKind::Sum => self.parse_sum_loop(),
            _ => Err(ParseError::unexpected_token("expression", self.peek().clone())),
        }
    }

    fn parse_sum_loop(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::Sum)?;
        self.expect(TokenKind::LBracket)?;
        let var_name = self.expect_identifier()?;
        self.expect(TokenKind::In)?;
        let start = self.parse_expression(0)?;
        self.expect(TokenKind::DotDotDot)?;
        let end = self.parse_expression(0)?;
        self.expect(TokenKind::Comma)?;
        let body = self.parse_expression(0)?;
        self.expect(TokenKind::RBracket)?;
        Ok(Expression::SumLoop(SumLoop {
            var_name,
            start: Box::new(start),
            end: Box::new(end),
            body: Box::new(body),
        }))
    }

    fn parse_function_call(&mut self, name: String) -> Result<Expression, ParseError> {
        self.expect(TokenKind::LBracket)?;
        let mut args = Vec::new();
        if self.peek() != &TokenKind::RBracket {
            args.push(self.parse_expression(0)?);
            while self.peek() == &TokenKind::Comma {
                self.advance();
                args.push(self.parse_expression(0)?);
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(Expression::FunctionCall(FunctionCall { name, args }))
    }

    fn parse_if_else(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::If)?;
        let condition = self.parse_expression(0)?;
        self.expect(TokenKind::LBrace)?;
        let then_branch = self.parse_expression(0)?;
        self.expect(TokenKind::RBrace)?;
        self.expect(TokenKind::Else)?;
        self.expect(TokenKind::LBrace)?;
        let else_branch = self.parse_expression(0)?;
        self.expect(TokenKind::RBrace)?;
        Ok(Expression::IfElse(IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }))
    }

    fn parse_expression(&mut self, min_prec: u8) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some(op_prec) = self.infix_precedence(self.peek()) {
            if op_prec < min_prec {
                break;
            }

            let op = self.to_binary_operator(self.peek().clone())?;
            self.advance();

            let next_min_prec = if Self::is_right_associative(op) { op_prec } else { op_prec + 1 };
            let right = self.parse_expression(next_min_prec)?;

            left = Expression::BinaryOp(BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            });
        }
        Ok(left)
    }

    fn to_binary_operator(&self, kind: TokenKind) -> Result<BinaryOperator, ParseError> {
        match kind {
            TokenKind::Plus => Ok(BinaryOperator::Add),
            TokenKind::Minus => Ok(BinaryOperator::Subtract),
            TokenKind::Star => Ok(BinaryOperator::Multiply),
            TokenKind::Slash => Ok(BinaryOperator::Divide),
            TokenKind::Caret => Ok(BinaryOperator::Power),
            TokenKind::GreaterThan => Ok(BinaryOperator::GreaterThan),
            TokenKind::LessThan => Ok(BinaryOperator::LessThan),
            TokenKind::EqualsEquals => Ok(BinaryOperator::Equal),
            TokenKind::GreaterThanOrEqual => Ok(BinaryOperator::GreaterThanOrEqual),
            TokenKind::LessThanOrEqual => Ok(BinaryOperator::LessThanOrEqual),
            _ => Err(ParseError::new(format!("Not a binary operator: {:?}", kind)))
        }
    }

    fn is_right_associative(op: BinaryOperator) -> bool {
        matches!(op, BinaryOperator::Power)
    }

    fn infix_precedence(&self, kind: &TokenKind) -> Option<u8> {
        match kind {
            TokenKind::EqualsEquals | TokenKind::GreaterThan | TokenKind::LessThan | TokenKind::GreaterThanOrEqual | TokenKind::LessThanOrEqual => Some(Precedence::Comparison as u8),
            TokenKind::Plus | TokenKind::Minus => Some(Precedence::Sum as u8),
            TokenKind::Star | TokenKind::Slash => Some(Precedence::Product as u8),
            TokenKind::Caret => Some(Precedence::Power as u8),
            _ => None,
        }
    }
}

enum Precedence {
    _Lowest = 0,
    Comparison = 1,
    Sum = 2,
    Product = 3,
    Power = 4,
    Unary = 5,
}