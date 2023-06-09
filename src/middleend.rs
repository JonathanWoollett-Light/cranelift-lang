use crate::frontend::*;
use std::{
    collections::{HashMap, LinkedList},
    time::{Duration, Instant},
};

use tracing::info;

#[derive(Debug, Default)]
pub struct Context {
    /// The values of variables.
    variables: HashMap<String, Expression>,
    /// The definitions of functions.
    functions: HashMap<String, Function>,
    /// When inline variables from a function, we use a prefix prevent identifier collisions.
    /// Specifically we use `_` as the prefix as this character is not accepted by the parser, so
    /// the user cannot use it to define variables.
    prefix: usize,
}
impl Context {
    fn prefix(&self, x: &str) -> Option<String> {
        Self::prefix_free(self.prefix, x)
    }
    fn prefix_free(n: usize, x: &str) -> Option<String> {
        let num = x.chars().take_while(|c| *c == '_').count();
        n.checked_sub(num).map(|d| format!("{}{x}", "_".repeat(d)))
    }
}
use std::collections::linked_list::CursorMut;

#[derive(Debug, Eq, PartialEq)]
pub enum Control {
    Break,
    Return(Value),
}

const MAX_FUNCTION_INLINING_TIME: Duration = Duration::from_secs(1);

#[allow(clippy::match_same_arms, clippy::too_many_lines)]
// #[instrument]
pub fn evaluate(mut cursor: CursorMut<Statement>, context: &mut Context) -> Option<Control> {
    #[cfg(debug_assertions)]
    let mut outer_guard = 0;

    // TODO Make this less awkward.
    loop {
        info!("{:?}", cursor.current());
        info!("{:?}", context.variables);

        if cursor.current().is_none() {
            break;
        }

        debug_assert!(cursor.current().is_some());

        // info!("current: {:?}", cursor.current());
        // info!("next: {:?}", cursor.peek_next());
        // info!("prev: {:?}", cursor.peek_prev());

        match cursor.current().unwrap().0.clone() {
            // TODO Properly handle loops
            StatementType::Loop(l) => {
                #[cfg(debug_assertions)]
                let mut guard = 0;

                let mut inline = LinkedList::new();
                let start = Instant::now();
                let inlined = loop {
                    // We spend up to 1 second evaluating if code can be inlined.
                    if start.elapsed() > MAX_FUNCTION_INLINING_TIME {
                        break false;
                    }
                    #[cfg(debug_assertions)]
                    {
                        guard += 1;
                        debug_assert!(guard < 3);
                    }
                    let mut list = l.0.clone();
                    let control = evaluate(list.cursor_front_mut(), context);
                    info!("control: {control:?}");
                    inline.append(&mut list);
                    match control {
                        None => continue,
                        Some(Control::Break) => {
                            cursor.splice_before(inline);
                            cursor.remove_current();
                            break true;
                        }
                        Some(Control::Return(x)) => {
                            cursor.splice_before(inline);
                            cursor.remove_current();
                            return Some(Control::Return(x));
                        }
                    }
                };

                // TODO Is this behavior correct?
                // If the loop was not inlined, move past it.
                if !inlined {
                    cursor.move_next();
                }
            }
            StatementType::If(i) => {
                match &i.cond {
                    Value::Literal(literal) => {
                        // if false
                        if literal.0 == 0 {
                            cursor.remove_current();
                        }
                        // if true
                        else {
                            cursor.splice_after(i.inner);
                            cursor.remove_current();
                        }
                    }
                    Value::Ident(_) => {
                        tracing::warn!("skipping statement");
                        cursor.move_next();
                    }
                }
            }
            StatementType::Return(Return(value)) => match value {
                Value::Ident(ident) => {
                    // Apply context prefix.
                    let ident = context.prefix(&ident.0).unwrap();
                    let inner_value = match context.variables.get(&ident) {
                        Some(Expression::Binary(Binary {
                            lhs: Value::Literal(_),
                            op: _,
                            rhs: Value::Literal(_),
                        })) => unreachable!(),
                        Some(Expression::Unary(Unary(
                            v @ Value::Ident(_) | v @ Value::Literal(_),
                        ))) => v.clone(),
                        _ => Value::Ident(Ident(ident)),
                    };
                    cursor.current().unwrap().0.return_mut().unwrap().0 = inner_value.clone();

                    info!("return");
                    cursor.split_after();
                    info!("{:?}", cursor.current());
                    cursor.move_next();
                    info!("{:?}", cursor.current());
                    return Some(Control::Return(inner_value));
                    // TODO Remove all statements before this in scope which do not change the identifier.
                }
                Value::Literal(_) => {
                    cursor.current().unwrap().0.return_mut().unwrap().0 = value.clone();
                    return Some(Control::Return(value));
                    // TODO Remove all statements before this in scope as the return will always be a known literal.
                }
            },
            StatementType::Break(_) => {
                info!("break");
                cursor.split_after();
                // TODO There are plenty circumstances where loops are not inlined and breaks cannot
                // be removed. For these (which represent most loops, this is incorrect and will
                // need to be fixed moving forward).
                cursor.remove_current();
                return Some(Control::Break);
            }
            StatementType::Assign(assign) => {
                // Apply context prefix.
                // In argument assignments (when inlining a function, we need to assign to the
                // functions arguments) the values being assigned to will have a prefix greater
                // than the current scope by 1, thus they will not match, in this case we simply
                // pass-through the identifiers.
                let assign_ident = Ident(
                    context
                        .prefix(&assign.ident.0)
                        .unwrap_or(assign.ident.0.clone()),
                );
                cursor.current().unwrap().0.assign_mut().unwrap().ident.0 = assign_ident.0.clone();

                // In the cases where we don't remove the current statement, when we evaluate
                // previous statements to return to the next statement we need to take another step
                // forward.
                let mut step_forward = false;

                match &assign.expr {
                    Expression::Unknown(_) => {
                        // The value cannot be evaluated at compile-time thus we remove
                        // the variable from the context.
                        context.variables.remove(&assign_ident.0);
                        step_forward = true;
                    }
                    Expression::Unary(unary) => match &unary.0 {
                        Value::Literal(_) => {
                            // Any assignment where the assigned value is known can be removed and the value
                            // inlined.
                            cursor.remove_current();

                            context
                                .variables
                                .insert(assign_ident.0.clone(), assign.expr.clone());
                        }
                        Value::Ident(ident) => {
                            // Apply context prefix.
                            // In return assignments (when inlining a function, we need to assign to
                            // return value) the value being assigned will have a prefix greater
                            // than the current scope by 1, thus they will not match, in this case
                            // we simply pass-through the identifiers.
                            let ident = Ident(context.prefix(&ident.0).unwrap_or(ident.0.clone()));

                            cursor.remove_current();

                            // Insert the value of `ident` into the context under `assign_ident`, if
                            // no value is found, insert the value as the identifier `ident`.
                            context.variables.insert(
                                assign_ident.0.clone(),
                                context
                                    .variables
                                    .get(&ident.0)
                                    .cloned()
                                    .unwrap_or(assign.expr.clone()),
                            );
                        }
                    },
                    Expression::Binary(binary) => match (&binary.lhs, &binary.rhs) {
                        (Value::Ident(a), Value::Ident(b)) => {
                            // Apply context prefix.
                            let a = Ident(context.prefix(&a.0).unwrap());
                            cursor
                                .current()
                                .unwrap()
                                .0
                                .assign_mut()
                                .unwrap()
                                .expr
                                .binary_mut()
                                .unwrap()
                                .lhs
                                .ident_mut()
                                .unwrap()
                                .0 = a.0.clone();
                            // Apply context prefix.
                            let b = Ident(context.prefix(&b.0).unwrap());
                            cursor
                                .current()
                                .unwrap()
                                .0
                                .assign_mut()
                                .unwrap()
                                .expr
                                .binary_mut()
                                .unwrap()
                                .rhs
                                .ident_mut()
                                .unwrap()
                                .0 = b.0.clone();

                            match (context.variables.get(&a.0), context.variables.get(&b.0)) {
                                (Some(x), Some(y)) => match (x, y) {
                                    (Expression::Unary(Unary(x)), Expression::Unary(Unary(y))) => {
                                        match (x, y) {
                                            // If the lhs identifier has a known literal value and the rhs identifier has a known literal value.
                                            (
                                                Value::Literal(Literal(a)),
                                                Value::Literal(Literal(b)),
                                            ) => {
                                                let c = binary.op.run(*a, *b);
                                                context.variables.insert(
                                                    assign_ident.0.clone(),
                                                    Expression::Unary(Unary(Value::Literal(
                                                        Literal(c),
                                                    ))),
                                                );

                                                // Remove current + Step
                                                cursor.remove_current();
                                            }
                                            (a, b) => {
                                                cursor
                                                    .current()
                                                    .unwrap()
                                                    .0
                                                    .assign_mut()
                                                    .unwrap()
                                                    .expr
                                                    .binary_mut()
                                                    .unwrap()
                                                    .lhs = a.clone();
                                                cursor
                                                    .current()
                                                    .unwrap()
                                                    .0
                                                    .assign_mut()
                                                    .unwrap()
                                                    .expr
                                                    .binary_mut()
                                                    .unwrap()
                                                    .rhs = b.clone();
                                                // The value cannot be evaluated at compile-time thus we remove
                                                // the variable from the context.
                                                context.variables.remove(&assign_ident.0);
                                                step_forward = true;
                                            }
                                        }
                                    }
                                    _ => todo!(),
                                },
                                (Some(x), None) => match x {
                                    Expression::Unary(Unary(u)) => {
                                        // The lhs identifier, can be an alias, unknown or a known literal.
                                        let new_value = match u {
                                            // Alias or unwrap for unknown.
                                            Value::Ident(y) => context
                                                .variables
                                                .get(&y.0)
                                                .cloned()
                                                .unwrap_or(x.clone()),
                                            // Known literal.
                                            Value::Literal(_) => x.clone(),
                                        };

                                        cursor.current().unwrap().0.assign_mut().unwrap().expr =
                                            new_value;

                                        // The value cannot be evaluated at compile-time thus we remove
                                        // the variable from the context.
                                        context.variables.remove(&assign_ident.0);
                                        step_forward = true;
                                    }
                                    _ => todo!(),
                                },
                                (None, Some(y)) => match y {
                                    Expression::Unary(Unary(u)) => {
                                        // The rhs identifier, can be an alias, unknown or a known literal.
                                        let new_value = match u {
                                            // Alias or unwrap for unknown.
                                            Value::Ident(x) => context
                                                .variables
                                                .get(&x.0)
                                                .cloned()
                                                .unwrap_or(y.clone()),
                                            // Known literal.
                                            Value::Literal(_) => y.clone(),
                                        };

                                        cursor.current().unwrap().0.assign_mut().unwrap().expr =
                                            new_value;

                                        // The value cannot be evaluated at compile-time thus we remove
                                        // the variable from the context.
                                        context.variables.remove(&assign_ident.0);
                                        step_forward = true;
                                    }
                                    _ => todo!(),
                                },
                                (None, None) => {
                                    // The value cannot be evaluated at compile-time thus we remove
                                    // the variable from the context.
                                    context.variables.remove(&assign_ident.0);
                                    step_forward = true;
                                }
                                _ => todo!(),
                            }
                        }
                        // TODO Handle the other binary cases
                        _ => todo!(),
                    },
                    Expression::Call(call) => {
                        let control = inline_call(&mut cursor, context, call);
                        let Some(Control::Return(ret)) = control else {
                            panic!("Functions called for assignments need to always return.");
                        };

                        cursor.current().unwrap().0.assign_mut().unwrap().expr =
                            Expression::Unary(Unary(ret));
                        // We do not step here as we want this return assignment to be evaluated in
                        // the next loop.
                    }
                }

                // When assigning to an identifier, we can remove all previous assigns
                // where the value wasn't used.
                //
                // From
                // ```
                // x = ?
                // x = 4
                // ```
                // we can remove `x = ?`
                //
                // As such here, we iterate through the previous statement, until we find a
                // statement that uses the identifier, if this statement is an assignment we
                // remove it, otherwise we break.

                let mut i = 0;
                #[cfg(debug_assertions)]
                let mut guard: i32 = 0;
                loop {
                    #[cfg(debug_assertions)]
                    {
                        guard += 1;
                        debug_assert!(guard < 100);
                    }

                    cursor.move_prev();
                    i += 1;
                    if let Some(prev) = cursor.current() {
                        #[allow(clippy::single_match_else)]
                        match &prev.0 {
                            StatementType::Assign(a) => {
                                // We can only remove the assignment if it is assigning to
                                // the same ident and is not using the ident (if the
                                // ident was previously assigned an unknown value e.g.
                                // `x = ?` it won't be inlined and this next assignment
                                // `x = x + 5` cannot be removed).
                                if a.ident == assign_ident {
                                    match &a.expr {
                                        Expression::Unknown(_) => {
                                            cursor.remove_current();
                                            i -= 1;
                                            continue;
                                        }
                                        Expression::Binary(Binary {
                                            lhs,
                                            op: _,
                                            rhs,
                                        }) => {
                                            if !(matches!(lhs,Value::Ident(x) if *x == assign_ident)
                                                || matches!(rhs,Value::Ident(x) if *x == assign_ident))
                                            {
                                                cursor.remove_current();
                                                i -= 1;
                                                continue;
                                            }
                                        }
                                        Expression::Unary(Unary(value)) => {
                                            if !matches!(value,Value::Ident(x) if *x == assign_ident)
                                            {
                                                cursor.remove_current();
                                                i -= 1;
                                                continue;
                                            }
                                        },
                                        Expression::Call(Call { ident: _, args }) => {
                                            if !args.iter().any(|arg|matches!(arg,Value::Ident(x) if *x == assign_ident)) {
                                                cursor.remove_current();
                                                i -= 1;
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                            // TODO We should only break when the identifier is used, not when
                            // any statement other than an assign is used, fix this.
                            _ => tracing::warn!("skipping statement"),
                        }
                    }
                    break;
                }
                // Reset cursor to current position.
                for _ in 0..i {
                    cursor.move_next();
                }
                if step_forward {
                    cursor.move_next();
                }
            }
            StatementType::Call(call) => {
                let _ret = inline_call(&mut cursor, context, &call);
            }
            // TODO Handle context prefixes for functions.
            StatementType::Function(f) => {
                context.functions.insert(f.ident.0.clone(), f);
                cursor.remove_current();
            }
        }

        #[cfg(debug_assertions)]
        {
            outer_guard += 1;
            assert!(outer_guard < 100);
        }
    }
    None
}

// fn outer_evaluate(cursor: &mut LinkedList<Statement>) {
//     let mut context = Context::default();
//     let cursor = statements.cursor_front_mut();
//     let control = evaluate(cursor, &mut context);
//     let Some(Control::Return(ret)) = control else {
//         panic!("Functions called for assignments need to always return.");
//     };

//     cursor.current().unwrap().0.assign_mut().unwrap().expr =
//         Expression::Unary(Unary(ret));
// }

fn inline_call(
    cursor: &mut CursorMut<Statement>,
    context: &mut Context,
    call: &Call,
) -> Option<Control> {
    info!("call");
    let function = context
        .functions
        .get(&call.ident.0)
        .expect("Undefined function.");
    assert_eq!(
        call.args.len(),
        function.args.len(),
        "Incorrect number of arguments."
    );

    // Construct context for the function
    let function_variables = call
        .args
        .iter()
        .zip(function.args.iter())
        .filter_map(|(from, to)| {
            // let inline_ident = Context::prefix_free(context.prefix + 1, &to.0).unwrap();
            match from {
                Value::Literal(_) => Some((to.0.clone(), Expression::Unary(Unary(from.clone())))),
                Value::Ident(Ident(ident)) => context
                    .variables
                    .get(ident)
                    .map(|value| (to.0.clone(), value.clone())),
            }
        })
        .collect::<HashMap<_, _>>();

    // Functions are automatically passed.
    let mut function_context = Context {
        variables: function_variables,
        functions: context.functions.clone(),
        prefix: context.prefix,
    };
    // Get the statements that passthrough the values.
    let mut function_statements = call
        .args
        .iter()
        .zip(function.args.iter())
        .map(|(from, to)| {
            let inline_ident = Context::prefix_free(context.prefix + 1, &to.0).unwrap();
            Statement(StatementType::Assign(Assign {
                ident: Ident(inline_ident),
                expr: Expression::Unary(Unary(from.clone())),
            }))
        })
        .collect::<LinkedList<_>>();

    // Evaluate argument assignments.
    let control = evaluate(
        function_statements.cursor_front_mut(),
        &mut function_context,
    );
    debug_assert_eq!(control, None);

    // With a higher prefix, evaluate the function statements (starting from the 1st statement in
    // the function but considering the argument assignment statements).
    function_context.prefix += 1;
    let mut list = function.inner.clone();
    let mut inner_cursor = list.cursor_front_mut();
    inner_cursor.splice_before(function_statements);
    let control = evaluate(inner_cursor, &mut function_context);

    // TODO Is this correct?
    context.variables.extend(function_context.variables);

    cursor.splice_before(list);
    cursor.move_prev();
    cursor.remove_current();

    control
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn middle() {
        tracing_subscriber::fmt::fmt()
            .with_file(true)
            .with_line_number(true)
            .init();

        let string = std::fs::read_to_string("./example-input.txt").unwrap();
        let mut list = crate::frontend::parser::statements(&string, 0).unwrap();

        print!("---------------------------------------");
        println!("{}", print(&list, 0));
        println!("---------------------------------------");

        let mut context = Context::default();

        let cursor = list.cursor_front_mut();
        evaluate(cursor, &mut context);

        print!("---------------------------------------");
        println!("{}", print(&list, 0));
        println!("---------------------------------------");
    }
}
