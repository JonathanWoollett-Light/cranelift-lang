#[allow(clippy::wildcard_imports)]
use crate::frontend::*;
use std::collections::HashMap;

pub type ValueMap = HashMap<ExprIdent, usize>;
pub type FunctionMap<'a> = HashMap<ExprIdent, &'a ExprFn>;

#[derive(Debug)]
pub enum ControlFlow {
    None,
    Break,
    Return(Value),
}

/// Evaluates function calls.
///
/// # Panics
///
/// When coming across a `break` statement at the base of a function.
pub fn evaluate_call(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    ExprCall { ident, args }: &ExprCall,
) -> Option<usize> {
    let fn_args = &functions.get(ident).unwrap().args;

    let mut fn_values = fn_args
        .iter()
        .zip(args.iter())
        .filter_map(|(a, b)| {
            let eval = evaluate_value(values, functions, b);
            eval.map(|x| (a.clone(), x))
        })
        .collect();

    let ExprFn {
        ident: _,
        args: _,
        statements,
    } = functions.get(ident).unwrap();
    for statement in statements.iter() {
        match evaluate_statement(&mut fn_values, functions, statement) {
            Some(ControlFlow::None) => continue,
            Some(ControlFlow::Break) => panic!("Cannot call break outside `loop`."),
            Some(ControlFlow::Return(value)) => {
                return evaluate_value(&mut fn_values, functions, &value)
            }
            None => return None,
        }
    }
    Some(0)
}

pub fn evaluate_function<'a>(
    _: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    function: &'a ExprFn,
) {
    functions.insert(function.ident.clone(), function);
}

pub fn evaluate_loop<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    ExprLoop(statements): &'a ExprLoop,
) -> Option<ControlFlow> {
    // TODO Store the values of element in the loop in each iteration.
    loop {
        for statement in statements.iter() {
            match evaluate_statement(values, functions, statement) {
                Some(ControlFlow::None) => continue,
                // This loop breaks and consumes the break.
                Some(ControlFlow::Break) => return Some(ControlFlow::None),
                Some(ControlFlow::Return(v)) => return Some(ControlFlow::Return(v)),
                None => return None,
            }
        }
    }
}

pub fn evaluate_statement<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    statement: &'a Statement,
) -> Option<ControlFlow> {
    match statement {
        Statement::Assign(assign) => {
            evaluate_assign(values, functions, assign);
            Some(ControlFlow::None)
        }
        Statement::Loop(expr_loop) => evaluate_loop(values, functions, expr_loop),
        Statement::If(expr_if) => evaluate_if(values, functions, expr_if),
        Statement::Call(expr_call) => {
            evaluate_call(values, functions, expr_call);
            Some(ControlFlow::None)
        }
        Statement::Break(_) => Some(ControlFlow::Break),
        Statement::Function(expr_fn) => {
            evaluate_function(values, functions, expr_fn);
            Some(ControlFlow::None)
        }
        Statement::Return(ExprReturn(v)) => Some(ControlFlow::Return(v.clone())),
    }
}

pub fn evaluate_if<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    ExprIf { cond, then, val }: &'a ExprIf,
) -> Option<ControlFlow> {
    let eval = evaluate_cond(values, functions, cond).map(|x| x == 1);
    val.borrow_mut().push(eval);

    match eval {
        Some(true) => {
            for statement in then.iter() {
                match evaluate_statement(values, functions, statement) {
                    Some(ControlFlow::None) => continue,
                    Some(ControlFlow::Break) => return Some(ControlFlow::Break),
                    Some(ControlFlow::Return(v)) => return Some(ControlFlow::Return(v)),
                    // If in any statement we cannot know if control flow is evaluated then we cannot
                    // know if control flow is evaluated for this if.
                    None => return None,
                }
            }
            Some(ControlFlow::None)
        }
        Some(false) => Some(ControlFlow::None),
        None => None,
    }
}

pub fn evaluate_cond(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    ExprCond { lhs, cmp, rhs }: &ExprCond,
) -> Option<usize> {
    let (rhs_opt, lhs_opt) = (
        evaluate_value(values, functions, lhs),
        evaluate_value(values, functions, rhs),
    );
    match (rhs_opt, lhs_opt) {
        (Some(rhs), Some(lhs)) => Some(match cmp {
            Cmp::Eq => usize::from(rhs == lhs),
            Cmp::Gt => usize::from(rhs > lhs),
            Cmp::Lt => usize::from(rhs < lhs),
        }),
        _ => None,
    }
}

pub fn evaluate_assign(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    ExprAssign { ident, assign, val }: &ExprAssign,
) {
    let x = match assign {
        Expr::Unary(value) => evaluate_value(values, functions, value),
        Expr::Binary(binary) => evaluate_binary(values, functions, binary),
    };
    val.borrow_mut().push(x);
    if let Some(y) = x {
        values.insert(ident.clone(), y);
    }
}

/// Evaluate binary expression.
pub fn evaluate_binary(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    (a, op, b): &ExprBinary,
) -> Option<usize> {
    let (rhs_opt, lhs_opt) = (
        evaluate_value(values, functions, a),
        evaluate_value(values, functions, b),
    );
    match (rhs_opt, lhs_opt) {
        (Some(rhs), Some(lhs)) => Some(match op {
            Op::Add => rhs + lhs,
            Op::Div => rhs / lhs,
            Op::Mul => rhs * lhs,
            Op::Sub => rhs - lhs,
        }),
        _ => None,
    }
}

/// Given a value returns a `usize` if it can be determined at compile time.
///
/// # Panics
///
/// When attempting to parse literal as `usize`.
pub fn evaluate_value(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    value: &Value,
) -> Option<usize> {
    match value {
        Value::Literal(ExprLit(literal)) => {
            Some(literal.parse::<usize>().expect("Failed to parse literal."))
        }
        Value::Identifier(ident) => values.get(ident).copied(),
        Value::Call(call) => evaluate_call(values, functions, call),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn checker() {
        let string = std::fs::read_to_string("./example-input").unwrap();
        let statements = parser::statements(&string, 0).unwrap();
        println!("statements: {statements:?}");

        let mut values = ValueMap::new();
        let mut functions = FunctionMap::new();
        evaluate_statement(&mut values, &mut functions, &statements[0]);
        println!("\nstatements[0]: {:?}", statements[0]);

        evaluate_statement(&mut values, &mut functions, &statements[1]);
        println!("\nstatements[1]: {:?}", statements[1]);

        evaluate_statement(&mut values, &mut functions, &statements[2]);
        println!("\nstatements[2]: {:?}", statements[2]);

        evaluate_statement(&mut values, &mut functions, &statements[3]);
        println!("\nstatements[3]: {:?}", statements[3]);

        evaluate_statement(&mut values, &mut functions, &statements[4]);
        println!("\nstatements[4]: {:?}", statements[4]);

        evaluate_statement(&mut values, &mut functions, &statements[5]);
        println!("\nstatements[5]: {:?}", statements[5]);

        println!("\nvalues: {values:?}");

        evaluate_statement(&mut values, &mut functions, &statements[6]);
        println!("\nstatements[6]: {:?}", statements[6]);

        println!("\nfunctions: {functions:?}");

        evaluate_statement(&mut values, &mut functions, &statements[7]);
        println!("\nstatements[7]: {:?}", statements[7]);

        evaluate_statement(&mut values, &mut functions, &statements[8]);
        println!("\nstatements[8]: {:?}", statements[8]);

        evaluate_statement(&mut values, &mut functions, &statements[9]);
        println!("\nstatements[9]: {:?}", statements[9]);

        println!(
            "\n\n\n{}\n\n\n",
            statements
                .iter()
                .map(|s| format!("{}\n", s.print(0)))
                .collect::<String>()
        );
    }
}
