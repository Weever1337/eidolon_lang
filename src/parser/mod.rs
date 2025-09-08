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

    fn peek_token(&self) -> &Token {
        self.tokens.get(self.pos).expect("unexpected EOF")
    }

    fn peek(&self) -> &TokenKind {
        &self.peek_token().kind
    }

    fn consume(&mut self) -> &Token {
        let token = &self.tokens[self.pos];
        if self.pos < self.tokens.len() -1 {
            self.pos += 1;
        }
        token
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        let token = self.peek_token();
        if token.kind == expected {
            let owned_token = token.clone();
            self.pos += 1;
            Ok(owned_token)
        } else {
            Err(ParseError::unexpected_token(&format!("{:?}", expected), token))
        }
    }

    fn expect_identifier(&mut self) -> Result<(String, usize), ParseError> {
        let token = self.peek_token();
        if let TokenKind::Identifier(name) = &token.kind {
            let name = name.clone();
            let line = token.line;
            self.pos += 1;
            Ok((name, line))
        } else {
            Err(ParseError::unexpected_token("Identifier", token))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut tokens_with_eof = self.tokens.clone();
        let eof_pos = tokens_with_eof.last().map_or(0, |t| t.pos + 1);
        let eof_line = tokens_with_eof.last().map_or(1, |t| t.line);
        tokens_with_eof.push(Token {
            kind: TokenKind::Eof,
            pos: eof_pos,
            line: eof_line,
        });
        self.tokens = tokens_with_eof;

        let mut statements = Vec::new();
        while self.peek() != &TokenKind::Eof {
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            TokenKind::Let => self.parse_let_binding().map(Statement::LetBinding),
            TokenKind::Fun => self.parse_function_def().map(Statement::FunctionDef),
            _ => self.parse_expression(0).map(Statement::Expression),
        }
    }

    fn parse_let_binding(&mut self) -> Result<LetBinding, ParseError> {
        self.expect(TokenKind::Let)?;
        let (name, _) = self.expect_identifier()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expression(0)?;
        Ok(LetBinding { name, value })
    }

    fn parse_function_def(&mut self) -> Result<FunctionDef, ParseError> {
        self.expect(TokenKind::Fun)?;
        let (name, _) = self.expect_identifier()?;

        self.expect(TokenKind::LBracket)?;
        let mut params = Vec::new();
        if self.peek() != &TokenKind::RBracket {
            params.push(self.expect_identifier()?.0);
            while self.peek() == &TokenKind::Comma {
                self.consume();
                params.push(self.expect_identifier()?.0);
            }
        }
        self.expect(TokenKind::RBracket)?;
        self.expect(TokenKind::Equals)?;

        let body = if self.peek() == &TokenKind::LBrace {
            self.consume();
            let mut statements = Vec::new();
            while self.peek() == &TokenKind::Let {
                statements.push(self.parse_statement()?);
            }
            let final_expr = Box::new(self.parse_expression(0)?);
            self.expect(TokenKind::RBrace)?;
            BlockBody { statements, final_expr }
        } else {
            let expr = self.parse_expression(0)?;
            BlockBody { statements: vec![], final_expr: Box::new(expr) }
        };

        Ok(FunctionDef { name, params, body })
    }

    fn parse_list_literal(&mut self) -> Result<Expression, ParseError> {
        let token = self.expect(TokenKind::LBracket)?;
        let line = token.line;
        let mut items = Vec::new();
        if self.peek() != &TokenKind::RBracket {
            items.push(self.parse_expression(0)?);
            while self.peek() == &TokenKind::Comma {
                self.consume();
                items.push(self.parse_expression(0)?);
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(Expression { kind: Box::new(Expr::ListLiteral(items)), line })
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let token = self.peek_token().clone();
        let line = token.line;

        match token.kind {
            TokenKind::Number(n) => {
                self.consume();
                Ok(Expression { kind: Box::new(Expr::Literal(n)), line })
            }
            TokenKind::String(s) => {
                self.consume();
                Ok(Expression { kind: Box::new(Expr::StringLiteral(s)), line })
            }
            TokenKind::Identifier(name) => {
                self.consume();
                if self.peek() == &TokenKind::LBracket {
                    self.parse_function_call(name, line)
                } else {
                    Ok(Expression { kind: Box::new(Expr::Variable(name)), line })
                }
            }
            TokenKind::GlobalIdentifier(name) => {
                self.consume();
                Ok(Expression { kind: Box::new(Expr::GlobalVariable(name)), line })
            }
            TokenKind::LParen => {
                self.consume();
                let expr = self.parse_expression(0)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::Minus => {
                self.consume();
                let op = UnaryOperator::Negate;
                let expr = self.parse_expression(Precedence::Unary as u8)?;
                Ok(Expression { kind: Box::new(Expr::UnaryOp(UnaryOp { op, expr })), line })
            }
            TokenKind::If => self.parse_if_else(),
            TokenKind::Sum => self.parse_sum_loop(),
            TokenKind::LBracket => self.parse_list_literal(),
            _ => Err(ParseError::unexpected_token("expression", self.peek_token())),
        }
    }

    fn parse_sum_loop(&mut self) -> Result<Expression, ParseError> {
        let line = self.expect(TokenKind::Sum)?.line;
        self.expect(TokenKind::LBracket)?;
        let (var_name, _) = self.expect_identifier()?;
        self.expect(TokenKind::In)?;
        let start = self.parse_expression(0)?;
        self.expect(TokenKind::DotDotDot)?;
        let end = self.parse_expression(0)?;
        self.expect(TokenKind::Comma)?;
        let body = self.parse_expression(0)?;
        self.expect(TokenKind::RBracket)?;
        Ok(Expression { kind: Box::new(Expr::SumLoop(SumLoop { var_name, start: start, end: end, body: body })), line })
    }

    fn parse_function_call(&mut self, name: String, line: usize) -> Result<Expression, ParseError> {
        self.expect(TokenKind::LBracket)?;
        let mut args = Vec::new();
        if self.peek() != &TokenKind::RBracket {
            args.push(self.parse_expression(0)?);
            while self.peek() == &TokenKind::Comma {
                self.consume();
                args.push(self.parse_expression(0)?);
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(Expression { kind: Box::new(Expr::FunctionCall(FunctionCall { name, args })), line })
    }

    fn parse_if_else(&mut self) -> Result<Expression, ParseError> {
        let line = self.expect(TokenKind::If)?.line;
        let condition = self.parse_expression(0)?;
        self.expect(TokenKind::LBrace)?;
        let then_branch = self.parse_expression(0)?;
        self.expect(TokenKind::RBrace)?;
        self.expect(TokenKind::Else)?;
        self.expect(TokenKind::LBrace)?;
        let else_branch = self.parse_expression(0)?;
        self.expect(TokenKind::RBrace)?;
        Ok(Expression { kind: Box::new(Expr::IfElse(IfElse { condition: condition, then_branch: then_branch, else_branch: else_branch })), line })
    }

    fn parse_expression(&mut self, min_prec: u8) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;

        loop {
            let peeked_token = self.peek_token();
            if let Some(op_prec) = self.infix_precedence(&peeked_token.kind) {
                if op_prec < min_prec {
                    break;
                }
                let op_token = self.consume().clone();
                let op = self.to_binary_operator(&op_token.kind)?;
                let line = op_token.line;
                let next_min_prec = if Self::is_right_associative(op) { op_prec } else { op_prec + 1 };
                let right = self.parse_expression(next_min_prec)?;

                left = Expression {
                    kind: Box::new(Expr::BinaryOp(BinaryOp { op, left, right })),
                    line,
                };

            } else if peeked_token.kind == TokenKind::Dot {
                let line = self.consume().line;
                let (member, _) = self.expect_identifier()?;
                left = Expression {
                    kind: Box::new(Expr::MemberAccess(MemberAccess { object: left, member })),
                    line,
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn to_binary_operator(&self, kind: &TokenKind) -> Result<BinaryOperator, ParseError> {
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
            _ => Err(ParseError::new(format!("Not a binary operator: {:?}", kind), self.peek_token().line))
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