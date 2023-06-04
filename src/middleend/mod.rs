mod ast;

use crate::frontend::*;
use std::{
    collections::{HashMap, LinkedList},
    time::{Duration, Instant},
};

use tracing::info;

#[derive(Debug, Default)]
pub struct Context {
    /// The values of variables.
    variables: HashMap<String, LiteralValue>,
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
        if num <= n {
            Some(format!("{}{x}", "_".repeat(n)))
        } else {
            None
        }
    }
}
use std::collections::linked_list::CursorMut;

#[derive(Debug, Eq, PartialEq)]
pub enum Control {
    Break,
    Return(Value),
}

const MAX_FUNCTION_INLINING_TIME: Duration = Duration::from_secs(1);

#[allow(clippy::match_same_arms)]
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
                    _ => cursor.move_next(),
                }
            }
            StatementType::Return(_) => {
                // Apply context prefix.
                if let Some(ident) = cursor
                    .current()
                    .unwrap()
                    .0
                    .return_mut()
                    .unwrap()
                    .0
                    .ident_mut()
                {
                    ident.0 = context.prefix(&ident.0).unwrap();
                }
                let value = cursor.current().unwrap().0.return_mut().unwrap().0.clone();

                info!("return");
                cursor.split_after();
                info!("{:?}", cursor.current());
                cursor.move_next();
                info!("{:?}", cursor.current());
                return Some(Control::Return(value));
            }
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
                let assign_ident = Ident(context.prefix(&assign.ident.0).unwrap());
                cursor.current().unwrap().0.assign_mut().unwrap().ident.0 = assign_ident.0.clone();

                match &assign.expr {
                    Expression::Unary(unary) => match &unary.0 {
                        Value::Literal(literal) => {
                            // Any assignment where the assigned value is known can be removed and the value
                            // inlined.
                            cursor.remove_current();

                            // When assigning a literal to an identifier, we can remove all previous assigns
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
                                    match &prev.0 {
                                        StatementType::Assign(a) => {
                                            // We can only remove the assignment if it is assigning to
                                            // the same ident and is not using the ident (if the
                                            // ident was previously assigned an unknown value e.g.
                                            // `x = ?` it won't be inlined and this next assignment
                                            // `x = x + 5` cannot be removed).
                                            if a.ident == assign_ident {
                                                match &a.expr {
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
                                        _ => {}
                                    }
                                }
                                break;
                            }
                            // Reset cursor to current position.
                            for _ in 0..i {
                                cursor.move_next();
                            }

                            context.variables.insert(assign_ident.0.clone(), literal.0);
                        }
                        Value::Unknown => cursor.move_next(),
                        Value::Ident(ident) => {
                            // Apply context prefix.
                            // In this case, we may be assigning an identifier from an inlined
                            // scope, this could mean it would already have a greater prefix, in
                            // this case this is okay.
                            let ident: Ident = if let Some(new) = context.prefix(&ident.0) {
                                Ident(new)
                            } else {
                                Ident(ident.0.clone())
                            };
                            cursor
                                .current()
                                .unwrap()
                                .0
                                .assign_mut()
                                .unwrap()
                                .expr
                                .unary_mut()
                                .unwrap()
                                .0
                                .ident_mut()
                                .unwrap()
                                .0 = ident.0.clone();

                            if let Some(x) = context.variables.get(&ident.0) {
                                context.variables.insert(assign_ident.0.clone(), *x);
                                cursor.remove_current();
                            } else {
                                cursor.move_next();
                            }
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
                                (Some(x), Some(y)) => {
                                    let z = binary.op.run(*x, *y);
                                    context.variables.insert(assign_ident.0.clone(), z);

                                    // Remove current + Step
                                    cursor.remove_current();
                                }
                                (Some(x), None) => {
                                    cursor
                                        .current()
                                        .unwrap()
                                        .0
                                        .assign_mut()
                                        .unwrap()
                                        .expr
                                        .binary_mut()
                                        .unwrap()
                                        .lhs = Value::Literal(Literal(*x));
                                    // The value of the identifier is now unknown thus we remove it
                                    // from the variable context.
                                    context.variables.remove(&assign_ident.0);

                                    // Step
                                    cursor.move_next();
                                }
                                (None, Some(y)) => {
                                    cursor
                                        .current()
                                        .unwrap()
                                        .0
                                        .assign_mut()
                                        .unwrap()
                                        .expr
                                        .binary_mut()
                                        .unwrap()
                                        .rhs = Value::Literal(Literal(*y));
                                    // The value of the identifier is now unknown thus we remove it
                                    // from the variable context.
                                    context.variables.remove(&assign_ident.0);

                                    // Step
                                    cursor.move_next();
                                }
                                (None, None) => {
                                    // The value of the identifier is now unknown thus we remove it
                                    // from the variable context.
                                    context.variables.remove(&assign_ident.0);

                                    // Step
                                    cursor.move_next();
                                },
                            }
                        }
                        (Value::Unknown, Value::Ident(b)) => {
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
                            if let Some(y) = context.variables.get(&b.0) {
                                cursor
                                    .current()
                                    .unwrap()
                                    .0
                                    .assign_mut()
                                    .unwrap()
                                    .expr
                                    .binary_mut()
                                    .unwrap()
                                    .rhs = Value::Literal(Literal(*y));
                            }
                            cursor.move_next();
                        }
                        (Value::Ident(a), Value::Unknown) => {
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
                            if let Some(x) = context.variables.get(&a.0) {
                                cursor
                                    .current()
                                    .unwrap()
                                    .0
                                    .assign_mut()
                                    .unwrap()
                                    .expr
                                    .binary_mut()
                                    .unwrap()
                                    .lhs = Value::Literal(Literal(*x));
                            }
                            cursor.move_next();
                        }
                        // TODO Handle the other binary cases
                        _ => cursor.move_next(),
                    },
                    // TODO Reduce code duplicate between here and `Statement::Call`
                    Expression::Call(call) => {
                        let control = inline_call(&mut cursor, context, call);
                        let Some(Control::Return(ret)) = control else {
                            panic!("Functions called for assignments need to always return.");
                        };

                        cursor.current().unwrap().0.assign_mut().unwrap().expr =
                            Expression::Unary(Unary(ret));
                    }
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

fn inline_call(cursor: &mut CursorMut<Statement>, context: &mut Context, call: &Call) -> Option<Control> {
    info!("call");
    // we don't get stuck).
    let function = context
        .functions
        .get(&call.ident.0)
        .expect("Undefined function.");
    assert_eq!(
        call.args.len(),
        function.args.len(),
        "Incorrect number of arguments."
    );
    let (inline_variable_assignment, inline_variable_values) = call
        .args
        .iter()
        .zip(function.args.iter())
        .map(|(from, to)| {
            let inline_ident = Context::prefix_free(context.prefix + 1, &to.0).unwrap();
            match from {
                Value::Literal(Literal(x)) => (
                    None,
                    Some((inline_ident,*x,))
                ),
                Value::Ident(Ident(ident)) => {
                    if let Some(x) = context.variables.get(ident) {
                        (None,Some((inline_ident,*x)))
                    }
                    else {
                        (Some(Statement(StatementType::Assign(Assign { ident: Ident(inline_ident.clone()), expr: Expression::Unary(Unary(Value::Ident(Ident(ident.clone())))) }))),None)
                    }
                },
                Value::Unknown => (
                    Some(Statement(StatementType::Assign(Assign { ident: Ident(inline_ident), expr: Expression::Unary(Unary(Value::Unknown)) }))),
                    None
                ),
            }
            
        })
        .unzip::<_,_,Vec<_>,Vec<_>>();
    let function_assignments = inline_variable_assignment.into_iter().filter_map(|s|s).collect::<LinkedList<_>>();
    let function_variables = inline_variable_values.into_iter().filter_map(|s|s).collect::<HashMap<_,_>>();
    // Functions are automatically passed.
    let mut function_context = Context {
        variables: function_variables,
        functions: context.functions.clone(),
        prefix: context.prefix + 1,
    };
    let mut list = function.inner.clone();
    let control = evaluate(list.cursor_front_mut(), &mut function_context);
    context.variables.extend(function_context.variables);

    cursor.splice_before(function_assignments);
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
