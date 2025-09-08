#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetBinding(LetBinding),
    FunctionDef(FunctionDef),
    Expression(Expression),
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
pub struct Expression {
    pub kind: Box<Expr>,
    pub line: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(f64),
    StringLiteral(String),
    Variable(String),
    GlobalVariable(String),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    FunctionCall(FunctionCall),
    IfElse(IfElse),
    SumLoop(SumLoop),
    ListLiteral(Vec<Expression>),
    MemberAccess(MemberAccess),
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub object: Expression,
    pub member: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SumLoop {
    pub var_name: String,
    pub start: Expression,
    pub end: Expression,
    pub body: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperator,
    pub expr: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfElse {
    pub condition: Expression,
    pub then_branch: Expression,
    pub else_branch: Expression,
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