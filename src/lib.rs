use frontend::*;
use linked_syntax_tree::{Cursor, CursorMut, SyntaxTree};
use tracing::{info, instrument};
/// The frontend convert from the text to the abstract syntax tree, this is a singly-linked graph
pub mod frontend;

#[instrument(ret, skip_all, level = "trace")]
pub fn evaluate_tree(tree: &mut SyntaxTree<Statement>) {
    let mut cursor = tree.cursor_mut();

    if cursor.current().is_none() {
        return;
    }

    // Inline elements
    // ---------------------------------------------------------------------------------------------
    #[cfg(debug_assertions)]
    let mut guard = 0;
    loop {
        #[cfg(debug_assertions)]
        {
            guard += 1;
            assert!(guard < 100);
        }

        info!("current: {:?}", cursor.current());

        // Evaluate element
        // ---------------------------------------------------------------------
        // Split to get a cursor to the preceding statement so we can explore preceding statements
        // while maintaining a mutable cursor at the current node we can use to update the
        // statement.
        // let (mut after, mut before) = cursor.split();
        evaluate_statement(&mut cursor);

        // Move to next element
        // ---------------------------------------------------------------------
        if !cursor.move_successor() {
            break;
        }
    }

    // Remove statements
    // ---------------------------------------------------------------------------------------------
    let mut cursor = tree.cursor_mut();
    #[cfg(debug_assertions)]
    let mut guard = 0;
    // TODO Remove the `cloned` here.
    while let Some(current) = cursor.current().cloned() {
        #[cfg(debug_assertions)]
        {
            guard += 1;
            assert!(guard < 100);
        }

        match &current.0 {
            StatementType::Assign(assign) => match &assign.expr {
                // If statement assigns an identifier a literal value it can be removed.
                Expression::Unary(Unary(Value::Literal(_))) => cursor.remove_current(),
                // If a statement assigns an identifier to an identifier which has an unknown value,
                // this statement can be removed.
                Expression::Unary(Unary(Value::Ident(ident))) => {
                    let (_, mut before) = cursor.split();
                    while let Some(previous) = before.current() {
                        match &previous.0 {
                            StatementType::Assign(inner_assign) => {
                                if inner_assign.ident == *ident {
                                    match &inner_assign.expr {
                                        Expression::Unknown(_) => {
                                            cursor.remove_current();
                                        }
                                        _ => todo!(),
                                    }
                                    break;
                                }
                            }
                            _ => todo!(),
                        }
                        // TODO We also need to explore statements within `if` statements as these may assign to the identifier.
                        before.move_preceding();
                    }
                }
                _ => {
                    // Move to next element, if attempting to move to the next element failed,
                    // break.
                    if !cursor.move_successor() {
                        break;
                    }
                }
            },
            _ => {
                // Move to next element, if attempting to move to the next element failed, break.
                if !cursor.move_successor() {
                    break;
                }
            }
        }
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_statement(cursor: &mut CursorMut<Statement>) {
    let (mut after, mut before) = cursor.split();
    if let Some(statement) = after.current_mut() {
        match &mut statement.0 {
            StatementType::Assign(assign) => evaluate_assign(assign, &mut before),
            StatementType::Return(_) => evaluate_return(&mut after, &mut before),
            StatementType::If(_) => evaluate_if(&mut after, &mut before),
            _ => {}
        }
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_if(cursor: &mut CursorMut<Statement>, _before: &mut Cursor<Statement>) {
    match cursor.current().unwrap().0.if_ref().unwrap().cond {
        Value::Literal(Literal(n)) => {
            if n > 0 {
                cursor.flatten();
            }
            cursor.remove_current();
        }
        Value::Ident(_) => {}
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_return(cursor: &mut CursorMut<Statement>, before: &mut Cursor<Statement>) {
    let return_statement = cursor.current_mut().unwrap().0.return_mut().unwrap();

    // Evaluate return statement value.
    // -------------------------------------------------------------------------
    if let Value::Ident(ident) = &mut return_statement.0 {
        while let Some(previous) = before.current() {
            match &previous.0 {
                StatementType::Assign(inner_assign) => {
                    if inner_assign.ident == *ident {
                        match &inner_assign.expr {
                            Expression::Unary(inner_unary) => match inner_unary {
                                Unary(value @ Value::Literal(_)) => {
                                    return_statement.0 = value.clone();
                                    break;
                                }
                                _ => todo!(),
                            },
                            _ => todo!(),
                        }
                    }
                }
                _ => todo!(),
            }
            before.move_preceding();
        }
    }

    // Remove all next statements after a return statement.
    // -------------------------------------------------------------------------
    debug_assert_eq!(cursor.peek_child(), None);
    cursor.split_next();

    // Remove statements before the return statement.
    // -------------------------------------------------------------------------

    // TODO Avoid doing this again (see that we already do it above).
    let return_statement = cursor.current().unwrap().0.return_ref().unwrap();

    match return_statement.0 {
        // If the return statements returns a literal value, we can remove all statements before it
        // which do not have side affects.
        Value::Literal(_) => {
            let (_, mut before) = cursor.split_restricted();
            while let Some(current) = before.current() {
                match &current.0 {
                    StatementType::Assign(Assign { ident: _, expr }) => match expr {
                        Expression::Unary(_) | Expression::Binary(_) => before.remove_current(),
                        Expression::Unknown(_) => break,
                        Expression::Call(_) => todo!(),
                    },
                    _ => break,
                }
                before.move_preceding();
            }
        }
        Value::Ident(_) => todo!(),
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_assign(current: &mut Assign, before: &mut Cursor<Statement>) {
    match &mut current.expr {
        Expression::Binary(binary) => match binary {
            Binary {
                lhs: Value::Literal(a),
                op,
                rhs: Value::Literal(b),
            } => {
                current.expr = Expression::Unary(Unary(Value::Literal(Literal(op.run(a.0, b.0)))));
            }
            // TODO This is messy, clean this up.
            Binary {
                lhs: Value::Ident(a),
                op,
                rhs: Value::Ident(b),
            } => {
                let mut a_hit = false;
                let mut b_hit = false;
                let mut a_val = None;
                let mut b_val = None;

                while let Some(previous) = before.current() {
                    #[allow(clippy::single_match)]
                    match &previous.0 {
                        StatementType::Assign(inner_assign) => {
                            if inner_assign.ident == *a {
                                a_hit = true;
                                #[allow(clippy::single_match)]
                                match &inner_assign.expr {
                                    Expression::Unary(inner_unary) => match inner_unary {
                                        Unary(Value::Literal(v)) => {
                                            a_val = Some(v.clone());
                                        }
                                        _ => todo!(),
                                    },
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }
                            } else if inner_assign.ident == *b {
                                b_hit = true;
                                #[allow(clippy::single_match)]
                                match &inner_assign.expr {
                                    Expression::Unary(inner_unary) => match inner_unary {
                                        Unary(Value::Literal(v)) => {
                                            b_val = Some(v.clone());
                                        }
                                        _ => todo!(),
                                    },
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }
                            }
                            if a_hit && b_hit {
                                break;
                            }
                        }
                        // This is a todo, but for now we currently don't explore this.
                        _ => {}
                    }
                    // TODO This should be some function `move_predecessor_if` where the `if` is
                    // some closure that evaluates if to enter the children of a `if` statement.
                    before.move_preceding();
                }

                match (a_val, b_val) {
                    (Some(x), Some(y)) => {
                        current.expr =
                            Expression::Unary(Unary(Value::Literal(Literal(op.run(x.0, y.0)))));
                    }
                    (Some(x), None) => {
                        binary.lhs = Value::Literal(x);
                    }
                    (None, Some(y)) => {
                        binary.rhs = Value::Literal(y);
                    }
                    (None, None) => {}
                }
            }
            _ => todo!(),
        },
        Expression::Unary(unary) => {
            if let Unary(Value::Ident(ident)) = unary {
                while let Some(previous) = before.current() {
                    match &previous.0 {
                        StatementType::Assign(inner_assign) => {
                            if inner_assign.ident == *ident {
                                #[allow(clippy::single_match)]
                                match &inner_assign.expr {
                                    Expression::Unary(inner_unary) => match inner_unary {
                                        Unary(Value::Literal(_)) => {
                                            *unary = inner_unary.clone();
                                            break;
                                        }
                                        _ => todo!(),
                                    },
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }
                                break;
                            }
                        }
                        _ => todo!(),
                    }
                    // TODO This should be some function `move_predecessor_if` where the `if` is
                    // some closure that evaluates if to enter the children of a `if` statement.
                    before.move_preceding();
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use linked_syntax_tree::Element;

    #[test]
    fn one() {
        let text = r#"x = ?
y = 2 + 3
x = y
if 1
    c = y + x
    x = c
return x
"#;
        let mut tree = statements(text.as_bytes());
        evaluate_tree(&mut tree);
        let mut iter = tree.iter();
        // Assert `text` is evaluated to
        // ```text
        // x = ?
        // return 10
        // ```
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("x")),
                    expr: Expression::Unknown(Unknown)
                })),
                depth: 0
            })
        );
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Return(Return(Value::Literal(Literal(10))))),
                depth: 0
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn two() {
        let text = r#"x = ?
y = 2
y = x
a = 2
b = 3
c = a + b
d = ?
if 1
    c = d + c
    d = 4
a = d
return a
return a
"#;
        let mut tree = statements(text.as_bytes());
        evaluate_tree(&mut tree);
        let mut iter = tree.iter();
        // Assert `text` is evaluated to
        // ```text
        // x = ?
        // d = ?
        // return 4
        // ```
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("x")),
                    expr: Expression::Unknown(Unknown)
                })),
                depth: 0
            })
        );
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("d")),
                    expr: Expression::Unknown(Unknown)
                })),
                depth: 0
            })
        );
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Return(Return(Value::Literal(Literal(4))))),
                depth: 0
            })
        );
        assert_eq!(iter.next(), None);
    }

    fn middle() {
        tracing_subscriber::fmt::init();

        let file = std::fs::File::open("./example-input.txt").unwrap();
        let mut tree = statements(file);
        evaluate_tree(&mut tree);
        println!("{tree}");
    }
}
