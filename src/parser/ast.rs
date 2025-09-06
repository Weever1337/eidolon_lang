#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub final_expr: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetBinding(LetBinding),
    FunctionDef(FunctionDef),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetBinding {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockBody {
    pub statements: Vec<Statement>,
    pub final_expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: BlockBody,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(f64),
    StringLiteral(String),
    Variable(String),
    GlobalVariable(String),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    FunctionCall(FunctionCall),
    IfElse(IfElse),
    SumLoop(SumLoop),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SumLoop {
    pub var_name: String,
    pub start: Box<Expression>,
    pub end: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperator,
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfElse {
    pub condition: Box<Expression>,
    pub then_branch: Box<Expression>,
    pub else_branch: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    GreaterThan,
    LessThan,
    Equal,
    GreaterThanOrEqual,
    LessThanOrEqual,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Negate,
}