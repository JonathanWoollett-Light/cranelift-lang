#![warn(clippy::pedantic)]

use frontend::*;
use linked_syntax_tree::{Cursor, CursorMut, OptionalNode};
use tracing::{info, instrument};
/// The frontend convert from the text to the abstract syntax tree, this is a singly-linked graph
pub mod frontend;

#[instrument(ret, skip_all, level = "trace")]
pub fn evaluate_tree(tree: &mut OptionalNode<Statement>) {
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

    // Remove unused assignments
    // ---------------------------------------------------------------------------------------------
    let mut cursor = tree.cursor_mut();
    #[cfg(debug_assertions)]
    let mut guard = 0;
    while let Some(current) = cursor.current() {
        #[cfg(debug_assertions)]
        {
            guard += 1;
            assert!(guard < 100);
        }

        match &current.0 {
            StatementType::Assign(assign) => match assign.expr {
                Expression::Unary(Unary(Value::Literal(_))) => cursor.remove_current(),
                _ => {
                    // Move to next element, if attempting to move to the next element failed, break.
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
            StatementType::Return(ret) => evaluate_return(ret, &mut before),
            StatementType::If(_) => evaluate_if(&mut after, &mut before),
            _ => {}
        }
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_if(cursor: &mut CursorMut<Statement>, before: &mut Cursor<Statement>) {
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
fn evaluate_return(current: &mut Return, before: &mut Cursor<Statement>) {
    if let Value::Ident(ident) = &mut current.0 {
        while let Some(previous) = before.current() {
            match &previous.0 {
                StatementType::Assign(inner_assign) => {
                    if inner_assign.ident == *ident {
                        match &inner_assign.expr {
                            Expression::Unary(inner_unary) => match inner_unary {
                                Unary(value @ Value::Literal(_)) => {
                                    current.0 = value.clone();
                                    break;
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                        break;
                    }
                }
                _ => {}
            }
            before.move_preceding();
        }
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
                    match &previous.0 {
                        StatementType::Assign(inner_assign) => {
                            if inner_assign.ident == *a {
                                a_hit = true;
                                match &inner_assign.expr {
                                    Expression::Unary(inner_unary) => match inner_unary {
                                        Unary(Value::Literal(v)) => {
                                            a_val = Some(v.clone());
                                        }
                                        _ => {}
                                    },
                                    _ => {}
                                }
                            } else if inner_assign.ident == *b {
                                b_hit = true;
                                match &inner_assign.expr {
                                    Expression::Unary(inner_unary) => match inner_unary {
                                        Unary(Value::Literal(v)) => {
                                            b_val = Some(v.clone());
                                        }
                                        _ => {}
                                    },
                                    _ => {}
                                }
                            }
                            if a_hit && b_hit {
                                break;
                            }
                        }
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
            _ => {}
        },
        Expression::Unary(unary) => match unary {
            Unary(Value::Ident(ident)) => {
                while let Some(previous) = before.current() {
                    match &previous.0 {
                        StatementType::Assign(inner_assign) => {
                            if inner_assign.ident == *ident {
                                match &inner_assign.expr {
                                    Expression::Unary(inner_unary) => match inner_unary {
                                        Unary(Value::Literal(_)) => {
                                            *unary = inner_unary.clone();
                                            break;
                                        }
                                        _ => {}
                                    },
                                    _ => {}
                                }
                                break;
                            }
                        }
                        _ => {}
                    }
                    // TODO This should be some function `move_predecessor_if` where the `if` is
                    // some closure that evaluates if to enter the children of a `if` statement.
                    before.move_preceding();
                }
            }
            _ => {}
        },
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use linked_syntax_tree::Element;

    #[test]
    fn basic() {
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
        println!("{tree}");
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
    fn middle() {
        tracing_subscriber::fmt::init();

        let file = std::fs::File::open("./example-input.txt").unwrap();
        let mut tree = statements(file);
        evaluate_tree(&mut tree);
        println!("{tree}");
    }
}
