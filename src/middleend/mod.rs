#[allow(clippy::wildcard_imports)]
use crate::frontend::*;
use std::collections::HashMap;

pub mod call;
pub mod value;
pub mod binary;

use std::cmp::{max, min};
#[derive(Debug)]
pub struct CheckingValue {
    current: usize,
    known_range: std::ops::Range<usize>,
}

pub type ValueMap = HashMap<ExprIdent, CheckingValue>;
pub type FunctionMap<'a> = HashMap<ExprIdent, &'a ExprFn>;

#[derive(Debug)]
pub enum ControlFlow {
    None,
    Break,
    Return(Option<usize>),
}

#[derive(Debug)]
pub enum ConstStatement {
    If(ConstExprIf),
    Assign(ConstAssign),
    Loop(ConstLoop),
    Call(ConstCall),
    Break,
    Function,
    Return(Box<ConstReturn>),
}

pub fn evaluate_function<'a>(
    _: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    const_program: &mut Vec<Statement>,
    function: &'a ExprFn,
) {
    functions.insert(function.ident.clone(), function);
}

const LOOP_LIMIT: usize = 100;

pub fn evaluate_loop<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    const_program: &mut Vec<Statement>,
    ExprLoop(statements): &'a ExprLoop,
) -> (Option<ControlFlow>, ConstLoop) {
    // TODO Store the values of element in the loop in each iteration.
    let mut i = 0;
    let const_block = Vec::new();
    loop {
        for statement in statements.iter() {
            if i > LOOP_LIMIT {
                return (None, ConstLoop(const_block));
            }
            i += 1;
            match evaluate_statement(values, functions, const_program, statement) {
                (Some(ControlFlow::None), _) => continue,
                // This loop breaks and consumes the break.
                (Some(ControlFlow::Break), _) => {
                    return (Some(ControlFlow::None), ConstLoop(const_block))
                }
                (Some(ControlFlow::Return(v)), _) => {
                    return (Some(ControlFlow::Return(v)), ConstLoop(const_block))
                }
                (None, _) => return (None, ConstLoop(const_block)),
            }
        }
    }
}

pub fn evaluate_return<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    const_program: &mut Vec<Statement>,
    ExprReturn(statement): &'a ExprReturn,
) -> (Option<usize>, ConstReturn) {
    let (control_flow, const_value) = evaluate_value(values, functions, const_program, statement);
    (control_flow, ConstReturn(const_value))
}

pub fn evaluate_statement<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    const_program: &mut Vec<Statement>,
    statement: &'a Statement,
) -> (Option<ControlFlow>, ConstStatement) {
    match statement {
        Statement::Assign(assign) => {
            let const_assign = evaluate_assign(values, functions, const_program, assign);
            (
                Some(ControlFlow::None),
                ConstStatement::Assign(const_assign),
            )
        }
        Statement::Loop(expr_loop) => {
            let (control_flow, const_loop) =
                evaluate_loop(values, functions, const_program, expr_loop);
            (control_flow, ConstStatement::Loop(const_loop))
        }
        Statement::If(expr_if) => {
            let (control_flow, const_if) = evaluate_if(values, functions, const_program, expr_if);
            (control_flow, ConstStatement::If(const_if))
        }
        Statement::Call(expr_call) => {
            let (_, const_call) = call::evaluate_call(values, functions, const_program, expr_call);
            (Some(ControlFlow::None), ConstStatement::Call(const_call))
        }
        Statement::Break(_) => (Some(ControlFlow::Break), ConstStatement::Break),
        Statement::Function(expr_fn) => {
            evaluate_function(values, functions, const_program, expr_fn);
            (Some(ControlFlow::None), ConstStatement::Function)
        }
        Statement::Return(expr_return) => {
            let (v, const_return) = evaluate_return(values, functions, const_program, expr_return);

            (
                Some(ControlFlow::Return(v.clone())),
                ConstStatement::Return(Box::new(const_return)),
            )
        }
    }
}

pub struct ConstReturn(ConstValue);

pub struct ConstLoop(Vec<ConstStatement>);

pub enum ConstExprIf {
    True(Vec<ConstStatement>),
    False,
    Unknown(ExprCond, Vec<ConstStatement>),
}
pub enum ConstAssign {
    // An unknown binary expression
    Binary(Box<ConstValue>, Op, Box<ConstValue>),
    // An unknown value
    Unary(Box<ConstValue>),
    // A known value.
    Known,
}

pub fn evaluate_if<'a>(
    values: &mut ValueMap,
    functions: &mut FunctionMap<'a>,
    const_program: &mut Vec<Statement>,
    ExprIf { cond, then, val }: &'a ExprIf,
) -> (Option<ControlFlow>, ConstExprIf) {
    let eval = evaluate_cond(values, functions, const_program, cond).map(|x| x == 1);
    val.borrow_mut().push(eval);

    match eval {
        Some(true) => {
            let mut const_block = Vec::new();
            for statement in then.iter() {
                match evaluate_statement(values, functions, const_program, statement) {
                    (Some(ControlFlow::None), _) => continue,
                    (Some(ControlFlow::Break), _) => {
                        return (Some(ControlFlow::Break), ConstExprIf::True(const_block))
                    }
                    (Some(ControlFlow::Return(v)), _) => {
                        return (Some(ControlFlow::Return(v)), ConstExprIf::True(const_block))
                    }
                    // If in any statement we cannot know if control flow is evaluated then we cannot
                    // know if control flow is evaluated for this if.
                    (None, _) => return (None, ConstExprIf::True(const_block)),
                }
            }
            (Some(ControlFlow::None), ConstExprIf::True(const_block))
        }
        Some(false) => (Some(ControlFlow::None), ConstExprIf::False),
        None => (None, ConstExprIf::Unknown(cond.clone(), then.clone())),
    }
}

pub fn evaluate_cond(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    const_program: &mut Vec<Statement>,
    ExprCond { lhs, cmp, rhs }: &ExprCond,
) -> Option<usize> {
    let (rhs_opt, lhs_opt) = (
        evaluate_value(values, functions, const_program, lhs),
        evaluate_value(values, functions, const_program, rhs),
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

/// Evaluates an assignment expression.
pub fn evaluate_assign(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    const_program: &mut Vec<Statement>,
    ExprAssign { ident, assign, val }: &ExprAssign,
) -> Option<ConstAssign> {
    let x: (Option<usize>, ConstAssign) = match assign {
        Expr::Unary(value) => {
            let (x, y) = evaluate_value(values, functions, const_program, value);
            (x, ConstAssign::Unary(Box::new(y)))
        }
        Expr::Binary(binary) => {
            let (x, y) = evaluate_binary(values, functions, const_program, binary);

            (
                x,
                match y {
                    Some((lhs, op, rhs)) => ConstAssign::Binary(Box::new(lhs), op, Box::new(rhs)),
                    None => None,
                },
            )
        }
    };
    // Push this value onto the evaluates values for this expression.
    val.borrow_mut().push(x);

    // If the assignment can be evaluated at compile time.
    if let Some(y) = x {
        if let Some(z) = values.get_mut(ident) {
            z.current = y;
            z.known_range = min(z.known_range.start, y)..max(z.known_range.end, y);
        } else {
            values.insert(
                ident.clone(),
                CheckingValue {
                    current: y,
                    known_range: y..y,
                },
            );
        }
        None
    } else {
        Some(ConstAssign::Unknown(ident.0.clone()))
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
        let mut const_program = Vec::new();
        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[0],
        );
        println!("\nstatements[0]: {:?}", statements[0]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[1],
        );
        println!("\nstatements[1]: {:?}", statements[1]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[2],
        );
        println!("\nstatements[2]: {:?}", statements[2]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[3],
        );
        println!("\nstatements[3]: {:?}", statements[3]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[4],
        );
        println!("\nstatements[4]: {:?}", statements[4]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[5],
        );
        println!("\nstatements[5]: {:?}", statements[5]);

        println!("\nvalues: {values:?}");

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[6],
        );
        println!("\nstatements[6]: {:?}", statements[6]);

        println!("\nfunctions: {functions:?}");

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[7],
        );
        println!("\nstatements[7]: {:?}", statements[7]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[8],
        );
        println!("\nstatements[8]: {:?}", statements[8]);

        evaluate_statement(
            &mut values,
            &mut functions,
            &mut const_program,
            &statements[9],
        );
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
