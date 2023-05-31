use crate::frontend::Op;

pub enum ConstCallValue {
    Call(Vec<ConstCallStatement>),
    Identifier(String),
    Unknown
}

pub enum ConstCallAssign {
    Both(ConstCallValue,Op,ConstCallValue),
    Lhs(ConstCallValue,Op),
    Rhs(Op,ConstCallValue)
}

#[derive(Debug)]
pub enum ConstCallStatement {
    If(Box<ConstCallIf>),
    Assign(ConstCallAssign),
    // ...
}

// -----------------------------------------------------------------------------

pub enum ConstCallLoopReturn {
    Identifier(String)
}

pub enum ConstCallIf {
    True(),
    Unknown()
}

pub enum ConstCallLoop {
    Assign(String, ConstCallAssign),
    Loop(ConstCallLoop),
    If(ConstCallIf),
    Break,
    Return
}

pub enum ConstCallAssign {
    Call(ConstCallStatement),
    Identifier(String),
    Unknown
}

#[derive(Debug)]
pub enum ConstCallStatement {
    Assign(String, ConstCallAssign),
    Loop(ConstCallLoop),
    If(ConstCallIf),
}

#[derive(Debug)]
pub enum ConstCall {
    Known(usize),
    Statement(ConstCallStatement),
}

impl From<(&mut ValueMap, &mut FunctionMap, &ExprCall)> for ConstCall {
    fn from((values,functions,ExprCall { ident, args }):(&mut ValueMap, &mut FunctionMap, &ExprCall)) -> Self {
        let fn_args = &functions.get(ident).unwrap().args;

        // Functions have a different scope and thus a different value map, the initial values are drawn
        // from the values passed, the range is set to the initial value
        let mut fn_values = fn_args
            .iter()
            .zip(args.iter())
            .filter_map(|(a, b)| {
                let eval = evaluate_value(values, functions, const_program, b);
                eval.map(|x| {
                    (
                        a.clone(),
                        CheckingValue {
                            current: x,
                            known_range: x..x,
                        },
                    )
                })
            })
            .collect();

        let ExprFn {
            ident: _,
            args: _,
            statements,
        } = functions.get(ident).unwrap();
        for statement in statements.iter() {
            match evaluate_statement(&mut fn_values, functions, const_program, statement) {
                Some(ControlFlow::None) => continue,
                Some(ControlFlow::Break) => panic!("Cannot call break outside `loop`."),
                Some(ControlFlow::Return(value)) => {
                    return evaluate_value(&mut fn_values, functions, const_program, &value)
                }
                None => return None,
            }
        }
        Some(0)
    }
}

/// Evaluates function calls.
///
/// # Panics
///
/// When coming across a `break` statement at the base of a function.
pub fn evaluate_call(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    const_program: &mut Vec<Statement>,
    ExprCall { ident, args }: &ExprCall,
) -> ConstCall {
    let fn_args = &functions.get(ident).unwrap().args;

    // Functions have a different scope and thus a different value map, the initial values are drawn
    // from the values passed, the range is set to the initial value
    let mut fn_values = fn_args
        .iter()
        .zip(args.iter())
        .filter_map(|(a, b)| {
            let eval = evaluate_value(values, functions, const_program, b);
            eval.map(|x| {
                (
                    a.clone(),
                    CheckingValue {
                        current: x,
                        known_range: x..x,
                    },
                )
            })
        })
        .collect();

    let ExprFn {
        ident: _,
        args: _,
        statements,
    } = functions.get(ident).unwrap();
    for statement in statements.iter() {
        match evaluate_statement(&mut fn_values, functions, const_program, statement) {
            Some(ControlFlow::None) => continue,
            Some(ControlFlow::Break) => panic!("Cannot call break outside `loop`."),
            Some(ControlFlow::Return(value)) => {
                return evaluate_value(&mut fn_values, functions, const_program, &value)
            }
            None => return None,
        }
    }
    Some(0)
}