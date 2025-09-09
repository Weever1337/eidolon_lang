pub mod environment;
pub mod value;

use crate::parser::ast::*;
use environment::{Environment, Vec2, Vec3};
use rand::Rng;
use std::collections::HashMap;
use value::EidolonValue;

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub message: String,
    pub line: usize,
}

impl RuntimeError {
    fn new(message: impl Into<String>, line: usize) -> Self {
        RuntimeError {
            message: message.into(),
            line,
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Runtime Error on line {}: {}", self.line, self.message)
    }
}

fn native_sin(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("sin() expects 1 arg", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.sin()))
}

fn native_cos(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("cos() expects 1 arg", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.cos()))
}

fn native_tan(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("tan() expects 1 arg", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.tan()))
}

fn native_sqrt(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("sqrt() expects 1 arg", line));
    }
    let n = args[0].as_number(line)?;
    if n < 0.0 {
        return Err(RuntimeError::new("sqrt() of a negative number", line));
    }
    Ok(EidolonValue::Number(n.sqrt()))
}

fn native_abs(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("abs() expects 1 arg", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.abs()))
}

fn native_log(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::new(
            "log() expects 2 arguments: base and number",
            line,
        ));
    }
    let base = args[0].as_number(line)?;
    let n = args[1].as_number(line)?;
    Ok(EidolonValue::Number(n.log(base)))
}

fn native_ln(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("ln() expects 1 argument", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.ln()))
}

fn native_floor(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("floor() expects 1 argument", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.floor()))
}

fn native_ceil(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("ceil() expects 1 argument", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.ceil()))
}

fn native_round(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("round() expects 1 argument", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(n.round()))
}

fn native_sig(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("sig() expects 1 arg", line));
    }
    let n = args[0].as_number(line)?;
    Ok(EidolonValue::Number(1.0 / (1.0 + (-n).exp())))
}

fn native_lerp(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::new("lerp() expects 3 arguments", line));
    }
    let t = args[2].as_number(line)?.clamp(0.0, 1.0);

    match (&args[0], &args[1]) {
        (EidolonValue::Number(a), EidolonValue::Number(b)) => {
            Ok(EidolonValue::Number(a + (b - a) * t))
        }
        (EidolonValue::Vec2(a), EidolonValue::Vec2(b)) => {
            let x = a.x + (b.x - a.x) * t;
            let y = a.y + (b.y - a.y) * t;
            Ok(EidolonValue::Vec2(Vec2 { x, y }))
        }
        (EidolonValue::Vec3(a), EidolonValue::Vec3(b)) => {
            let x = a.x + (b.x - a.x) * t;
            let y = a.y + (b.y - a.y) * t;
            let z = a.z + (b.z - a.z) * t;
            Ok(EidolonValue::Vec3(Vec3 { x, y, z }))
        }
        _ => Err(RuntimeError::new(
            "lerp() arguments must be all numbers or all vectors of the same type",
            line,
        )),
    }
}

fn native_clamp(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::new("clamp() expects 3 arguments", line));
    }
    let x = args[0].as_number(line)?;
    let min = args[1].as_number(line)?;
    let max = args[2].as_number(line)?;
    if min > max {
        return Err(RuntimeError::new(
            format!(
                "clamp() `min` argument ({}) cannot be greater than `max` argument ({})",
                min, max
            ),
            line,
        ));
    }
    Ok(EidolonValue::Number(x.clamp(min, max)))
}

fn native_map(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 5 {
        return Err(RuntimeError::new("map() expects 5 arguments", line));
    }
    let x = args[0].as_number(line)?;
    let in_min = args[1].as_number(line)?;
    let in_max = args[2].as_number(line)?;
    let out_min = args[3].as_number(line)?;
    let out_max = args[4].as_number(line)?;
    let t = (x - in_min) / (in_max - in_min);
    Ok(EidolonValue::Number(out_min + t * (out_max - out_min)))
}

fn native_random(_args: Vec<EidolonValue>, _line: usize) -> Result<EidolonValue, RuntimeError> {
    let mut rng = rand::rng();
    Ok(EidolonValue::Number(rng.random_range(0.0..1.0)))
}

fn native_vec2(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::new("vec2() expects 2 arguments", line));
    }
    let x = args[0].as_number(line)?;
    let y = args[1].as_number(line)?;
    Ok(EidolonValue::Vec2(Vec2 { x, y }))
}

fn native_vec3(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::new("vec3() expects 3 arguments", line));
    }
    let x = args[0].as_number(line)?;
    let y = args[1].as_number(line)?;
    let z = args[2].as_number(line)?;
    Ok(EidolonValue::Vec3(Vec3 { x, y, z }))
}

fn native_dot(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::new("dot() expects 2 arguments", line));
    }
    let v1 = args[0].as_vec2(line)?;
    let v2 = args[1].as_vec2(line)?;
    Ok(EidolonValue::Number(v1.x * v2.x + v1.y * v2.y))
}

fn native_length(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("length() expects 1 argument", line));
    }
    match &args[0] {
        EidolonValue::Vec2(v) => Ok(EidolonValue::Number((v.x.powi(2) + v.y.powi(2)).sqrt())),
        EidolonValue::Vec3(v) => Ok(EidolonValue::Number(
            (v.x.powi(2) + v.y.powi(2) + v.z.powi(2)).sqrt(),
        )),
        EidolonValue::List(l) => Ok(EidolonValue::Number(l.len() as f64)),
        _ => Err(RuntimeError::new(
            "length() expects a Vec2, Vec3 or List argument",
            line,
        )),
    }
}

fn native_cross(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::new("cross() expects 2 Vec3 arguments", line));
    }
    let v1 = args[0].as_vec3(line)?;
    let v2 = args[1].as_vec3(line)?;
    let x = v1.y * v2.z - v1.z * v2.y;
    let y = v1.z * v2.x - v1.x * v2.z;
    let z = v1.x * v2.y - v1.y * v2.x;
    Ok(EidolonValue::Vec3(Vec3 { x, y, z }))
}

fn native_factorial(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("factorial() expects 1 argument", line));
    }
    let n = args[0].as_number(line)? as u64;
    let mut result = 1;
    for i in 1..=n {
        result *= i;
    }
    Ok(EidolonValue::Number(result as f64))
}

fn native_print(args: Vec<EidolonValue>, _line: usize) -> Result<EidolonValue, RuntimeError> {
    let output = args
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<String>>()
        .join(" ");
    println!("{}", output);
    Ok(EidolonValue::Unit)
}

fn native_first(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("first() expects 1 list argument", line));
    }
    let list = args[0].as_list(line)?;
    list.first()
        .cloned()
        .ok_or_else(|| RuntimeError::new("Cannot get first element of an empty list", line))
}

fn native_last(args: Vec<EidolonValue>, line: usize) -> Result<EidolonValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::new("last() expects 1 list argument", line));
    }
    let list = args[0].as_list(line)?;
    list.last()
        .cloned()
        .ok_or_else(|| RuntimeError::new("Cannot get last element of an empty list", line))
}

fn load_builtins(env: &mut Environment) {
    env.constants
        .insert("PI".to_string(), EidolonValue::Number(std::f64::consts::PI));
    env.constants
        .insert("E".to_string(), EidolonValue::Number(std::f64::consts::E));
    env.native_functions.insert("sin".to_string(), native_sin);
    env.native_functions.insert("cos".to_string(), native_cos);
    env.native_functions.insert("tan".to_string(), native_tan);
    env.native_functions.insert("sqrt".to_string(), native_sqrt);
    env.native_functions.insert("abs".to_string(), native_abs);
    env.native_functions.insert("log".to_string(), native_log);
    env.native_functions.insert("ln".to_string(), native_ln);
    env.native_functions.insert("floor".to_string(), native_floor);
    env.native_functions.insert("ceil".to_string(), native_ceil);
    env.native_functions.insert("round".to_string(), native_round);
    env.native_functions.insert("sig".to_string(), native_sig);
    env.native_functions.insert("lerp".to_string(), native_lerp);
    env.native_functions.insert("clamp".to_string(), native_clamp);
    env.native_functions.insert("map".to_string(), native_map);
    env.native_functions.insert("random".to_string(), native_random);
    env.native_functions.insert("vec2".to_string(), native_vec2);
    env.native_functions.insert("vec3".to_string(), native_vec3);
    env.native_functions.insert("dot".to_string(), native_dot);
    env.native_functions.insert("length".to_string(), native_length);
    env.native_functions.insert("cross".to_string(), native_cross);
    env.native_functions.insert("factorial".to_string(), native_factorial);
    env.native_functions.insert("print".to_string(), native_print);
    env.native_functions.insert("first".to_string(), native_first);
    env.native_functions.insert("last".to_string(), native_last);
}

pub fn evaluate(
    program: &Program,
    globals: &HashMap<String, EidolonValue>,
) -> Result<EidolonValue, RuntimeError> {
    let mut env = Environment::new(globals);
    load_builtins(&mut env);

    for stmt in &program.statements {
        if let Statement::FunctionDef(func_def) = stmt {
            env.functions.insert(func_def.name.clone(), func_def.clone());
        }
    }

    let mut last_result = EidolonValue::Unit;

    for stmt in &program.statements {
        match stmt {
            Statement::FunctionDef(_) => continue,
            Statement::LetBinding(let_binding) => {
                let value = evaluate_expression(&let_binding.value, &mut env)?;
                env.set_var(let_binding.name.clone(), value.clone());
                last_result = value;
            }
            Statement::Expression(expr) => {
                last_result = evaluate_expression(expr, &mut env)?;
            }
        }
    }

    Ok(last_result)
}

fn evaluate_expression(
    expr: &Expression,
    env: &mut Environment,
) -> Result<EidolonValue, RuntimeError> {
    let line = expr.line;
    match &*expr.kind {
        Expr::Literal(n) => Ok(EidolonValue::Number(*n)),
        Expr::StringLiteral(s) => Ok(EidolonValue::String(s.clone())),
        Expr::Variable(name) => env
            .get_var(name)
            .ok_or_else(|| RuntimeError::new(format!("Variable '{}' not found", name), line)),
        Expr::GlobalVariable(name) => env
            .globals
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::new(format!("Global variable '${}' not found", name), line)),
        Expr::ListLiteral(items) => {
            let mut evaluated_items = Vec::new();
            for item_expr in items {
                evaluated_items.push(evaluate_expression(item_expr, env)?);
            }
            Ok(EidolonValue::List(evaluated_items))
        }
        Expr::MemberAccess(access) => {
            let object = evaluate_expression(&access.object, env)?;
            match object {
                EidolonValue::Vec2(v) => match access.member.as_str() {
                    "x" => Ok(EidolonValue::Number(v.x)),
                    "y" => Ok(EidolonValue::Number(v.y)),
                    _ => Err(RuntimeError::new(format!("Vec2 does not have member '{}'", access.member), line)),
                },
                EidolonValue::Vec3(v) => match access.member.as_str() {
                    "x" => Ok(EidolonValue::Number(v.x)),
                    "y" => Ok(EidolonValue::Number(v.y)),
                    "z" => Ok(EidolonValue::Number(v.z)),
                    _ => Err(RuntimeError::new(format!("Vec3 does not have member '{}'", access.member), line)),
                },
                _ => Err(RuntimeError::new("Member access is only supported for vectors", line)),
            }
        }
        Expr::UnaryOp(op) => {
            let val = evaluate_expression(&op.expr, env)?;
            match (op.op, val) {
                (UnaryOperator::Negate, EidolonValue::Number(n)) => Ok(EidolonValue::Number(-n)),
                _ => Err(RuntimeError::new("Type error in unary operation", line)),
            }
        }
        Expr::SumLoop(loop_data) => {
            let start = evaluate_expression(&loop_data.start, env)?.as_number(line)? as i64;
            let end = evaluate_expression(&loop_data.end, env)?.as_number(line)? as i64;
            let mut accumulator = 0.0;
            for i in start..=end {
                env.set_var(loop_data.var_name.clone(), EidolonValue::Number(i as f64));
                let result = evaluate_expression(&loop_data.body, env)?;
                accumulator += result.as_number(line)?;
            }
            env.variables.remove(&loop_data.var_name);
            Ok(EidolonValue::Number(accumulator))
        }
        Expr::BinaryOp(op) => {
            let left = evaluate_expression(&op.left, env)?;
            let right = evaluate_expression(&op.right, env)?;
            match (op.op, left, right) {
                (BinaryOperator::Add, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Number(l + r)),
                (BinaryOperator::Subtract, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Number(l - r)),
                (BinaryOperator::Multiply, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Number(l * r)),
                (BinaryOperator::Divide, EidolonValue::Number(l), EidolonValue::Number(r)) => {
                    if r == 0.0 { return Err(RuntimeError::new("Division by zero", line)); }
                    Ok(EidolonValue::Number(l / r))
                }
                (BinaryOperator::Power, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Number(l.powf(r))),
                (BinaryOperator::GreaterThan, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Boolean(l > r)),
                (BinaryOperator::LessThan, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Boolean(l < r)),
                (BinaryOperator::Equal, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Boolean((l - r).abs() < f64::EPSILON)),
                (BinaryOperator::GreaterThanOrEqual, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Boolean(l >= r)),
                (BinaryOperator::LessThanOrEqual, EidolonValue::Number(l), EidolonValue::Number(r)) => Ok(EidolonValue::Boolean(l <= r)),
                (BinaryOperator::Add, EidolonValue::Vec2(v1), EidolonValue::Vec2(v2)) => Ok(EidolonValue::Vec2(Vec2 { x: v1.x + v2.x, y: v1.y + v2.y })),
                (BinaryOperator::Subtract, EidolonValue::Vec2(v1), EidolonValue::Vec2(v2)) => Ok(EidolonValue::Vec2(Vec2 { x: v1.x - v2.x, y: v1.y - v2.y })),
                (BinaryOperator::Multiply, EidolonValue::Vec2(v), EidolonValue::Number(s)) => Ok(EidolonValue::Vec2(Vec2 { x: v.x * s, y: v.y * s })),
                (BinaryOperator::Multiply, EidolonValue::Number(s), EidolonValue::Vec2(v)) => Ok(EidolonValue::Vec2(Vec2 { x: v.x * s, y: v.y * s })),
                (BinaryOperator::Divide, EidolonValue::Vec2(v), EidolonValue::Number(s)) => Ok(EidolonValue::Vec2(Vec2 { x: v.x / s, y: v.y / s })),
                (BinaryOperator::Add, EidolonValue::Vec3(v1), EidolonValue::Vec3(v2)) => Ok(EidolonValue::Vec3(Vec3 { x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z })),
                (BinaryOperator::Subtract, EidolonValue::Vec3(v1), EidolonValue::Vec3(v2)) => Ok(EidolonValue::Vec3(Vec3 { x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z })),
                (BinaryOperator::Multiply, EidolonValue::Vec3(v), EidolonValue::Number(s)) => Ok(EidolonValue::Vec3(Vec3 { x: v.x * s, y: v.y * s, z: v.z * s })),
                (BinaryOperator::Multiply, EidolonValue::Number(s), EidolonValue::Vec3(v)) => Ok(EidolonValue::Vec3(Vec3 { x: v.x * s, y: v.y * s, z: v.z * s })),
                (BinaryOperator::Divide, EidolonValue::Vec3(v), EidolonValue::Number(s)) => Ok(EidolonValue::Vec3(Vec3 { x: v.x / s, y: v.y / s, z: v.z / s })),
                _ => Err(RuntimeError::new("Type mismatch in binary operation", line)),
            }
        }
        Expr::IfElse(if_else) => {
            let condition = evaluate_expression(&if_else.condition, env)?;
            match condition {
                EidolonValue::Boolean(b) => {
                    if b {
                        evaluate_expression(&if_else.then_branch, env)
                    } else {
                        evaluate_expression(&if_else.else_branch, env)
                    }
                }
                _ => Err(RuntimeError::new("If condition must be a boolean", line)),
            }
        }
        Expr::FunctionCall(call) => {
            if let Some(native_fn) = env.native_functions.clone().get(&call.name) {
                let mut evaluated_args = Vec::new();
                for arg_expr in &call.args {
                    evaluated_args.push(evaluate_expression(arg_expr, env)?);
                }
                return native_fn(evaluated_args, line);
            }

            if let Some(func_def) = env.functions.get(&call.name).cloned() {
                if call.args.len() != func_def.params.len() {
                    return Err(RuntimeError::new(
                        format!(
                            "Function '{}' expects {} arguments, but got {}",
                            call.name,
                            func_def.params.len(),
                            call.args.len()
                        ),
                        line,
                    ));
                }

                let mut function_scope_env = Environment::new(env.globals);
                function_scope_env.functions = env.functions.clone();
                function_scope_env.native_functions = env.native_functions.clone();
                function_scope_env.constants = env.constants.clone();
                function_scope_env.variables = env.variables.clone();

                let mut evaluated_args = Vec::new();
                for arg_expr in &call.args {
                    evaluated_args.push(evaluate_expression(arg_expr, env)?);
                }

                for (param_name, arg_value) in func_def.params.iter().zip(evaluated_args.into_iter()) {
                    function_scope_env.set_var(param_name.clone(), arg_value);
                }

                for stmt in &func_def.body.statements {
                    match stmt {
                        Statement::LetBinding(let_binding) => {
                            let value = evaluate_expression(&let_binding.value, &mut function_scope_env)?;
                            function_scope_env.set_var(let_binding.name.clone(), value);
                        },
                        _ => { return Err(RuntimeError::new("Only 'let' statements are allowed inside functions for now", line)); }
                    }
                }
                return evaluate_expression(&func_def.body.final_expr, &mut function_scope_env);
            }
            Err(RuntimeError::new(format!("Function '{}' not found", call.name), line))
        }
    }
}