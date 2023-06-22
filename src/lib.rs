#![warn(clippy::pedantic)]

use frontend::*;
use linked_syntax_tree::{Cursor, CursorMut, Preceding, SyntaxTree};
/// The frontend convert from the text to the abstract syntax tree, this is a singly-linked graph
pub mod frontend;

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

        

        // Split the tree to get a cursor at the previous node we can use to explore previous
        // statements while maintaining a mutable cursor at the current node we can use to update
        // the statement.
        
        
        // println!("current: {:?}",cursor.current());
        // println!("-------------------------\n{}",unsafe { cursor.tree.as_ref().clone() });
        // println!("cursor: {cursor:?}");

        let mut before = cursor.split();

        // println!("cursor: {cursor:?}");
        // println!("current: {:?}",cursor.current());
        // println!("before: {before:?}");
        // println!("-------------------------\n{before}");


        // let before_current = cursor.get_preceding().map(Preceding::unwrap);
        // let before_preceding = before_current.and_then(|c| unsafe { c.as_ref().preceding });
        // let mut before_cursor = unsafe { Cursor::new(before_preceding, before_current, &before) };

        // Evaluate element
        // ---------------------------------------------------------------------
        evaluate_statement(&mut cursor, &mut before);

        // Re-join tree.
        cursor.try_join(before).unwrap();

        // println!("cursor: {cursor:?}");
        // println!("current: {:?}",cursor.current());
        // println!("-------------------------\n{}",unsafe { cursor.tree.as_ref().clone() });
        // println!("\n\n\n");

        // Move to next element
        // ---------------------------------------------------------------------
        if !cursor.move_successor() {
            break;
        }
    }
    // return;

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
            }
            _ => {
                // Move to next element, if attempting to move to the next element failed, break.
                if !cursor.move_successor() {
                    break;
                }
            }
        }
        
    }
}

fn evaluate_statement(current: &mut CursorMut<Statement>, before: &mut Cursor<Statement>) {
    if let Some(current) = current.current_mut() {
        match &mut current.0 {
            StatementType::Assign(assign) => evaluate_assign(assign, before),
            StatementType::Return(ret) => evaluate_return(ret,before),
            StatementType::If(i) => evaluate_if(current,before),
            _ => {}
        }
    }
}

fn evaluate_if(current: &mut CursorMut<Statement>, before: &mut Cursor<Statement>) {
    let i = current.current()
}

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
    #[test]
    fn middle() {
        let file = std::fs::File::open("./example-input.txt").unwrap();
        let mut tree = statements(file);
        evaluate_tree(&mut tree);
        println!("{tree}");
    }
}
