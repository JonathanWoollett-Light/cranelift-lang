use crate::frontend::*;
use linked_syntax_tree::Preceding;
use linked_syntax_tree::{CursorMut, SyntaxTree};
use tracing::{info, instrument};

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

        // println!("subtree:\n{}",cursor.subtree());

        // Move to next element
        // ---------------------------------------------------------------------
        let moved = cursor.move_successor();
        if !moved {
            break;
        }
    }
    // return;
    // Remove statements
    // ---------------------------------------------------------------------------------------------
    info!("removing statements");

    #[cfg(debug_assertions)]
    let mut guard = 0;
    let mut used = std::collections::HashSet::new();
    loop {
        #[cfg(debug_assertions)]
        {
            guard += 1;
            assert!(guard < 100);
        }

        // Currently when evaluating assignments, we search preceding statements for an
        // assign to the same identifier that isn't used to remove it. The problem is if
        // this statement is immediately above the current statement we can't remove it due
        // to the way `RestrictedCursor` assures safety. So to cover this case we do this
        // here.
        //
        // TODO This is hacky, don't do this. Do something better.
        //
        // If the `before_expr` has no side-affects, and the `before_ident == ident` we can remove the preceding statement.
        if let Some(Statement(StatementType::Assign(Assign { ident, .. }))) = cursor.current()
            && let Some(Preceding::Parent(preceding) | Preceding::Previous(preceding)) = cursor.peek_preceding()
            && let Statement(StatementType::Assign(Assign { ident: before_ident, expr: before_expr })) = preceding
            && ident == before_ident
        {
            if let Expression::Literal(_) | Expression::Ident(_) = before_expr {
                cursor.move_preceding();
                cursor.remove_current();
            }
        }

        if let Some(Statement(current)) = cursor.current() {
            // info!("current: {current}");

            match current {
                StatementType::Assign(Assign { ident, expr }) => match expr {
                    // If statement assigns an identifier a literal value it can be removed.
                    Expression::Literal(Literal(_)) => {
                        if ident.0 != "out" {
                            cursor.remove_current()
                        }
                    }
                    // If a statement assigns an identifier to an identifier which has an unknown value,
                    // this statement can be removed.
                    Expression::Ident(rhs_ident) => {
                        used.insert(rhs_ident.0.clone());
                        if ident.0 != "out" {
                            cursor.remove_current();
                        }
                    }
                    Expression::Call(Call {
                        rt,
                        ident: rhs_ident,
                        input,
                    }) if rhs_ident.0 == "add" => {
                        let check = !used.contains(&rhs_ident.0);
                        for i in input.idents() {
                            used.insert(i.0.clone());
                        }
                        if check {
                            cursor.remove_current();
                        }
                    }
                    Expression::Unknown(_) => {}
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }

        let moved = cursor.move_predecessor();
        if !moved {
            break;
        }
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_statement(cursor: &mut CursorMut<Statement>) {
    // let (mut after, mut before) = cursor.slit();
    if let Some(statement) = cursor.current_mut() {
        match &mut statement.0 {
            StatementType::Assign(_) => evaluate_assign(cursor),
            // StatementType::Call(_) => evaluate_return(&mut after, &mut before),
            StatementType::If(_) => evaluate_if(cursor),
            _ => {}
        }
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_if(cursor: &mut CursorMut<Statement>) {
    match cursor.current().unwrap().0.if_ref().unwrap().cond {
        Expression::Literal(Literal(n)) => {
            if n > 0 {
                cursor.flatten();
            }
            cursor.remove_current();
            // Since the outer loop always moves to the successor element, we need this function to
            // finish on the already covered element to prevent skipping an element.
            // TODO This is inefficient, avoid this back and forth.
            cursor.move_preceding();
        }
        _ => todo!(),
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_assign(cursor: &mut CursorMut<Statement>) {
    let (mut cursor, mut before) = cursor.split_restricted();

    let current = cursor.current_mut().unwrap().0.assign_mut().unwrap();
    // info!("assign: {current}");
    match &mut current.expr {
        Expression::Call(call) => match call {
            Call { rt, ident, input: Some(input) } if ident.0 == "add" => {
                if let Expression::Array(Array(array)) = &mut **input && let Some([lhs,rhs]) = array.get_mut(..) {
                    match (&lhs,&rhs) {
                        (Expression::Literal(Literal(a)), Expression::Literal(Literal(b))) => {
                            // Update the expression
                            current.expr = Expression::Literal(Literal(*a+*b));

                            // Remove previous assignments that got overwritten
                            if let Some(mut preceding) = before.current() {
                                loop {
                                    if let StatementType::Assign(Assign { ident: before_ident, expr: before_expr }) = &preceding.0 {
                                        if current.ident == *before_ident {
                                            // TODO Evaluate the call for side affects.
                                            // A call may have side affects so we can't remove it.
                                            if let Expression::Call(_) = before_expr {

                                            }
                                            else {
                                                before.remove_current();
                                            }
                                        }
                                    }

                                    let Some(x) = before.peek_move_preceding() else { break; };
                                    preceding = x;
                                }
                            }
                        },
                        // TODO This is messy, clean this up.
                        (Expression::Ident(a),Expression::Ident(b)) => {
                            let mut hit = false;
                            let mut a_val = None;
                            let mut b_val = None;

                            // Search for the identifier values and remove previous assignments that got overwritten.
                            let mut used = false;

                            if let Some(mut preceding) = before.current() {
                                loop {
                                    #[allow(clippy::single_match)]
                                    match &preceding.0 {
                                        StatementType::Assign(Assign { ident: before_ident, expr: before_expr }) => {
                                            if current.ident == *before_ident  {
                                                // If assigning to a variable that has been used before its next assignment.
                                                if used {
                                                    // The variable can be used in its own re-assignment.
                                                    used = before_expr.contains(ident);
                                                }
                                                // If assigning to a variable that is not used until its next assignment.
                                                else {
                                                    // An unknown or call may have side affect so we cannot easily remove it.
                                                    match before_expr {
                                                        Expression::Call(_) | Expression::Unknown(_) => {},
                                                        _ => {
                                                            before.remove_current();
                                                        }
                                                    }
                                                }
                                            }
                                            else if before_ident == a {
                                                #[allow(clippy::single_match)]
                                                match before_expr {
                                                    Expression::Literal(Literal(inner_literal)) => {
                                                        a_val = Some(*inner_literal);
                                                    },
                                                    // This is a todo, but for now we currently don't explore this.
                                                    _ => {}
                                                }
                                                if hit {
                                                    break;
                                                }
                                                else {
                                                    hit = true;
                                                }
                                            }
                                            else if before_ident == b {
                                                #[allow(clippy::single_match)]
                                                match before_expr {
                                                    Expression::Literal(Literal(inner_literal)) => {
                                                        b_val = Some(*inner_literal);
                                                    },
                                                    // This is a todo, but for now we currently don't explore this.
                                                    _ => {}
                                                }
                                                if hit {
                                                    break;
                                                }
                                                else {
                                                    hit = true;
                                                }
                                            }
                                        }
                                        // This is a todo, but for now we currently don't explore this.
                                        _ => {}
                                    }

                                    let Some(x) = before.peek_move_preceding() else { break; };
                                    preceding = x;
                                }
                            }

                            match (a_val, b_val) {
                                (Some(x), Some(y)) => {
                                    current.expr =
                                        Expression::Literal(Literal(x+y));
                                }
                                (Some(x), None) => {
                                    *lhs = Expression::Literal(Literal(x));
                                }
                                (None, Some(y)) => {
                                    *rhs = Expression::Literal(Literal(y));
                                }
                                (None, None) => {}
                            }

                        }
                        _ => todo!()
                    }
                }
                else {
                    panic!("Invalid add");
                }
            }
            _ => todo!(),
        },
        Expression::Ident(ident) => {
            info!("ident: {ident}");

            let mut used = current.ident == *ident;

            // TODO This shit is crazy reduce duplication.
            // Iterate through predecessors, possibly removing an unused assignment, and searching
            // for the value of `ident`.
            if let Some(mut preceding) = before.current() {
                loop {
                    info!("preceding: {preceding}");

                    if let StatementType::Assign(Assign {
                        ident: before_ident,
                        expr: before_expr,
                    }) = &preceding.0
                    {
                        // If this statement is an assignment to the variable our current statement
                        // is assigning to.
                        //
                        // Since each assigns checks for and removes an unused assignment to the same
                        // identifier before it, there can only be at most 1 unused assignment to this
                        // identifier before this assignment (and in the case the last assignment before
                        // this is used, all other assignment before this will also be used).
                        if current.ident == *before_ident {
                            info!("here");

                            // If the identifier has not been used since its last assignment.
                            info!("used: {used}");
                            if !used {
                                info!("before_expr: {before_expr}");
                                // An unknown or call may have side affect so we cannot easily remove it.
                                match before_expr {
                                    Expression::Call(_) | Expression::Unknown(_) => {}
                                    _ => {
                                        info!("hit?");
                                        info!("preceding 2: {:?}", before.guarded_nodes.last());
                                        info!("preceding 2: {:?}", before.current);
                                        before.remove_current();
                                    }
                                }
                            }

                            // Continue loop, now only searching for the identifier value.
                            while let Some(Statement(preceding)) = before.peek_move_preceding() {
                                if let StatementType::Assign(Assign {
                                    ident: before_ident,
                                    expr: before_expr,
                                }) = preceding
                                {
                                    info!("preceding 2: {preceding}");
                                    // If this statement is an assignment to the variable used in
                                    // the current assignment.
                                    if before_ident == ident {
                                        #[allow(clippy::single_match)]
                                        match before_expr {
                                            Expression::Literal(literal) => {
                                                current.expr = Expression::Literal(literal.clone());
                                                break;
                                            }
                                            // - In the case we find the ident we are looking for is
                                            //   assigned to another ident, we know this ident could
                                            //   not be previously evaluated.
                                            // - In the unknown case, we cannot do anything.
                                            Expression::Ident(_) | Expression::Unknown(_) => break,
                                            _ => todo!(),
                                        }
                                    }
                                }
                            }
                            break;
                        }
                        // If this statement is an assignment to the variable used in the current
                        // assignment.
                        else if before_ident == ident {
                            info!("no here");
                            match before_expr {
                                Expression::Literal(literal) => {
                                    current.expr = Expression::Literal(literal.clone());
                                }
                                // - In the case we find the ident we are looking for is
                                //   assigned to another ident, we know this ident could
                                //   not be previously evaluated.
                                // - In the unknown case, we cannot do anything.
                                Expression::Ident(_) | Expression::Unknown(_) => {}
                                _ => todo!(),
                            }

                            // Continue loop, now only searching for an unused assignment.
                            let ident_clone = before_ident.clone();
                            while let Some(Statement(preceding)) = before.peek_move_preceding() {
                                if let StatementType::Assign(Assign {
                                    ident: _,
                                    expr: before_expr,
                                }) = preceding
                                {
                                    // If this statement is an assignment to the variable our
                                    // current statement is assigning to.
                                    if current.ident == ident_clone {
                                        if !used {
                                            match before_expr {
                                                // An unknown or call may have side affect so we cannot easily remove it.
                                                Expression::Call(_) | Expression::Unknown(_) => {}
                                                _ => before.remove_current(),
                                            }
                                        }
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                    }

                    if preceding.uses(&current.ident) {
                        used = true;
                    }

                    // TODO We also need to explore statements within `if` statements as these may assign to the identifier.
                    let Some(x) = before.peek_move_preceding() else { break; };
                    preceding = x;
                }
            }

            // info!("check 1: {:?}",before.current());
            // info!("check 2: {:?}",before.peek_preceding().unwrap());
        }
        _ => {}
    }
    cursor.move_preceding();
}

#[cfg(test)]
mod tests {
    use super::*;
    use linked_syntax_tree::Element;

    #[test]
    fn middle_one() {
        let text = r#"x = ?
y = add {2,3,}
x = y
if 1
    c = add {y,x,}
    x = c
out = x
"#;
        let mut tree = statements(text.as_bytes());
        evaluate_tree(&mut tree);

        println!("tree: {tree}");

        let mut iter = tree.iter();
        // Assert `text` is evaluated to
        // ```text
        // x = ?
        // out = 10
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
                    ident: Ident(String::from("out")),
                    expr: Expression::Literal(Literal(10))
                })),
                depth: 0
            })
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn middle_two() {
        let text = r#"x = ?
y = 2
y = x
a = 2
b = 3
c = add {a,b,}
d = ?
if 1
    c = add {d,c,}
    d = 4
a = d
out = a
out = a
"#;
        let mut tree = statements(text.as_bytes());
        evaluate_tree(&mut tree);

        println!("{tree}");

        let mut iter = tree.iter();
        // Assert `text` is evaluated to
        // ```text
        // x = ?
        // d = ?
        // out = 4
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
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("out")),
                    expr: Expression::Literal(Literal(4))
                })),
                depth: 0
            })
        );
        assert_eq!(iter.next(), None);
    }

    // fn middle() {
    //     tracing_subscriber::fmt::init();

    //     let file = std::fs::File::open("./example-input.txt").unwrap();
    //     let mut tree = statements(file);
    //     evaluate_tree(&mut tree);
    //     println!("{tree}");
    // }
}
