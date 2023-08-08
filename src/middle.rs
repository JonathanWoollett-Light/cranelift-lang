use crate::frontend::*;
use linked_syntax_tree::Preceding;
use linked_syntax_tree::{CursorMut, SyntaxTree};
use std::mem::size_of;
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
            assert!(guard < 20);
        }

        info!("current: {:?}", cursor.current());

        // Evaluate element
        // ---------------------------------------------------------------------
        // Split to get a cursor to the preceding statement so we can explore preceding statements
        // while maintaining a mutable cursor at the current node we can use to update the
        // statement.
        // let (mut after, mut before) = cursor.split();
        evaluate_statement(&mut cursor);

        // info!("subtree:\n{}", cursor.subtree());

        // Move to next element
        // ---------------------------------------------------------------------
        let moved = cursor.move_successor();
        info!("moved: {moved}");
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
            assert!(guard < 20);
        }

        info!("current: {:?}", cursor.current());

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
                        if ident.0 != "write" {
                            cursor.remove_current()
                        }
                    }
                    // If a statement assigns an identifier to an identifier which has an unknown value,
                    // this statement can be removed.
                    Expression::Ident(rhs_ident) => {
                        used.insert(rhs_ident.0.clone());
                        if ident.0 != "write" {
                            cursor.remove_current();
                        }
                    }
                    Expression::Call(Call {
                        rt: _,
                        ident: rhs_ident,
                        input,
                    }) => match rhs_ident.0.as_str() {
                        "add" => {
                            let check = !used.contains(&rhs_ident.0);
                            if let Some(input) = input {
                                for i in input.idents() {
                                    used.insert(i.0.clone());
                                }
                            }

                            if check {
                                cursor.remove_current();
                            }
                        }
                        "write" => {
                            // let check = !used.contains(&rhs_ident.0);
                            if let Some(input) = input {
                                for i in input.idents() {
                                    used.insert(i.0.clone());
                                }
                            }
                            // TODO Should we change the statement from an assignment to a function
                            // call when the assigned value isn't used?
                            // if check {
                            //     *current = Expression::Call()
                            // }
                        }
                        "read" => {
                            if let Some(input) = input {
                                for i in input.idents() {
                                    used.insert(i.0.clone());
                                }
                            }
                        }
                        _ => todo!(),
                    },
                    Expression::Unknown(_) => {}
                    _ => todo!(),
                },
                StatementType::Call(Call {
                    rt: _,
                    ident: Ident(inner_ident),
                    input: Some(_),
                }) if inner_ident == "exit" => {}
                StatementType::Call(Call {
                    rt,
                    ident: _,
                    input: _,
                }) => match rt {
                    true => {}
                    false => todo!(),
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
            StatementType::Call(_) => evaluate_call(cursor),
            _ => {}
        }
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_call(cursor: &mut CursorMut<Statement>) {
    let (mut cursor, mut before) = cursor.split_restricted();

    let current = cursor.current_mut().unwrap().0.call_mut().unwrap();
    match current {
        Call {
            rt: _,
            ident: Ident(inner_ident),
            input: Some(box Expression::Ident(exit_code_ident)),
        } if inner_ident == "exit" => {
            // Search for the identifier value
            if let Some(mut preceding) = before.current() {
                loop {
                    match &preceding.0 {
                        StatementType::Assign(Assign {
                            ident: before_ident,
                            expr: before_expr,
                        }) if exit_code_ident == before_ident => {
                            match before_expr {
                                Expression::Literal(Literal(inner_literal)) => {
                                    // ident_val = Some(*inner_literal);
                                    current.input = Some(Box::new(Expression::Literal(Literal(
                                        *inner_literal,
                                    ))));
                                }
                                // This is a todo, but for now we currently don't explore this.
                                _ => {}
                            }
                            break;
                        }
                        _ => {}
                    }
                    let Some(x) = before.peek_move_preceding() else {
                        break; // TODO Will this only trigger if a variable is not defined? Should this then panic?
                    };
                    preceding = x;
                }
            }
            cursor.split_next();
        }
        Call {
            rt: _,
            ident: Ident(inner_ident),
            input: Some(box Expression::Literal(_)),
        } if inner_ident == "exit" => {
            cursor.split_next();
        }
        _ => todo!(),
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_if(cursor: &mut CursorMut<Statement>) {
    let (mut cursor, mut before) = cursor.split_restricted();
    match &cursor.current().unwrap().0.if_ref().unwrap().cond {
        Expression::Literal(Literal(n)) => {
            if *n > 0 {
                cursor.flatten();
            }
            cursor.remove_current();
            // Since the outer loop always moves to the successor element, we need this function to
            // finish on the already covered element to prevent skipping an element.
            // TODO This is inefficient, avoid this back and forth.
            cursor.move_preceding();
        },
        Expression::Call(Call { rt: _, ident: Ident(inner_ident), input: Some(box Expression::Array(Array(arr))) }) if inner_ident == "eq" => {
            match arr.get(..) {
                Some([Expression::Literal(Literal(a)), Expression::Literal(Literal(b))]) => {
                    let res = (a == b) as u8 as i32;
                    cursor.current_mut().unwrap().0.if_mut().unwrap().cond = Expression::Literal(Literal(res));
                },
                Some([Expression::Ident(Ident(a)),Expression::Ident(Ident(b))]) => {
                    // Evaluate identifiers
                    let mut a_val = None;
                    let mut b_val = None;
                    if let Some(mut preceding) = before.current() {
                        loop {
                            match &preceding.0 {
                                StatementType::Assign(Assign {
                                    ident: Ident(before_ident),
                                    expr: before_expr
                                }) => {
                                    if before_ident == a {
                                        match before_expr {
                                            Expression::Literal(Literal(inner_literal)) => {
                                                a_val = Some(*inner_literal);
                                            }
                                            _ => {} // This is a todo, but for now we currently don't explore this.
                                        }
                                    }
                                    else if before_ident == b {
                                        match before_expr {
                                            Expression::Literal(Literal(inner_literal)) => {
                                                b_val = Some(*inner_literal);
                                            }
                                            _ => {} // This is a todo, but for now we currently don't explore this.
                                        }
                                    }
                                },
                                _ => todo!()
                            }
                            if a_val.is_some() && b_val.is_some() {
                                break;
                            }
                            let Some(x) = before.peek_move_preceding() else {
                                break; // TODO Will this only trigger if a variable is not defined? Should this then panic?
                            };
                            preceding = x;
                        }
                        match (a_val,b_val) {
                            (Some(x),Some(y)) => todo!(),
                            (Some(x),None) => todo!(),
                            (None, Some(x)) => todo!(),
                            (None,None) => todo!()
                        }
                    }
                }
                _ => todo!()
            }
        }
        _ => todo!("{}", cursor.current().unwrap()),
    }
}

#[instrument(ret, skip_all, level = "trace")]
fn evaluate_assign(cursor: &mut CursorMut<Statement>) {
    let (mut cursor, mut before) = cursor.split_restricted();

    let current = cursor.current_mut().unwrap().0.assign_mut().unwrap();
    // info!("assign: {current}");
    match &mut current.expr {
        Expression::Call(call) => match call {
            Call {
                rt: false,
                ident: Ident(inner_ident),
                input: Some(box Expression::Array(Array(arr))),
            } if inner_ident == "read" => {
                let Some([Expression::Literal(Literal(fd)), Expression::Literal(Literal(n))]) =
                    arr.get_mut(..)
                else {
                    todo!()
                };
                let mut buf = (0..*n).map(|_| u8::default()).collect::<Vec<_>>();
                info!("Awaiting input of {n} bytes");

                let res = unsafe { libc::read(*fd, buf.as_mut_ptr().cast(), *n as usize) };
                assert_eq!(res, *n as isize); // TODO A read can fail at compile time, we should then return the error code.

                let x = LiteralValue::from_ne_bytes(<[u8; 4]>::try_from(buf).unwrap());
                info!("Read value of {x}");

                current.expr = Expression::Literal(Literal(x));
            }
            Call {
                rt: true,
                ident: Ident(inner_ident),
                input: Some(box Expression::Array(Array(arr))),
            } if inner_ident == "read" => {
                match arr.get(..) {
                    Some([Expression::Literal(Literal(_)) | Expression::Ident(Ident(_)), Expression::Literal(Literal(_)) | Expression::Ident(Ident(_))]) => {},
                    _ => todo!()
                }
            }
            Call {
                rt: true,
                ident: Ident(inner_ident),
                input: Some(box Expression::Array(Array(arr))),
            } if inner_ident == "write" => {
                let Some([fd, y]) = arr.get_mut(..) else {
                    todo!()
                };
                match (fd, &y) {
                    (Expression::Literal(Literal(_)), Expression::Ident(ident)) => {
                        // Search for the identifier value
                        let mut ident_val = None;
                        if let Some(mut preceding) = before.current() {
                            loop {
                                match &preceding.0 {
                                    StatementType::Assign(Assign {
                                        ident: before_ident,
                                        expr: before_expr,
                                    }) => {
                                        if ident == before_ident {
                                            match before_expr {
                                                Expression::Literal(Literal(inner_literal)) => {
                                                    ident_val = Some(*inner_literal);
                                                }
                                                // This is a todo, but for now we currently don't explore this.
                                                _ => {}
                                            }
                                            break;
                                        }
                                    }
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }
                                let Some(x) = before.peek_move_preceding() else {
                                    break; // TODO Will this only trigger if a variable is not defined? Should this then panic?
                                };
                                preceding = x;
                            }
                        }
                        if let Some(x) = ident_val {
                            *y = Expression::Literal(Literal(x))
                        }
                    }
                    _ => todo!(),
                }
            }
            Call {
                rt: false,
                ident: Ident(inner_ident),
                input: Some(box Expression::Array(Array(arr))),
            } if inner_ident == "write" => {
                let Some([fd, x]) = arr.get_mut(..) else {
                    todo!()
                };
                match (fd, x) {
                    (Expression::Literal(Literal(fd)), Expression::Literal(Literal(x))) => {
                        let res = unsafe {
                            libc::write(
                                *fd,
                                x.to_ne_bytes().as_mut_ptr().cast(),
                                size_of::<LiteralValue>(),
                            )
                        };

                        current.expr = Expression::Literal(Literal(res as i32));
                    }
                    (Expression::Literal(Literal(fd)), Expression::Ident(ident)) => {
                        // Search for the identifier value
                        let mut ident_val = None;
                        if let Some(mut preceding) = before.current() {
                            loop {
                                match &preceding.0 {
                                    StatementType::Assign(Assign {
                                        ident: before_ident,
                                        expr: before_expr,
                                    }) => {
                                        if ident == before_ident {
                                            match before_expr {
                                                Expression::Literal(Literal(inner_literal)) => {
                                                    ident_val = Some(*inner_literal);
                                                }
                                                // This is a todo, but for now we currently don't explore this.
                                                _ => {}
                                            }
                                            break;
                                        }
                                    }
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }
                                let Some(x) = before.peek_move_preceding() else {
                                    break; // TODO Will this only trigger if a variable is not defined? Should this then panic?
                                };
                                preceding = x;
                            }
                        }
                        if let Some(x) = ident_val {
                            let mut arr = x.to_ne_bytes();
                            let res = unsafe {
                                libc::write(*fd, arr.as_mut_ptr().cast(), size_of::<LiteralValue>())
                            };
                            info!("res: {res}");
                            current.expr = Expression::Literal(Literal(res as i32));
                        }
                    }
                    _ => todo!(),
                }
            }
            Call {
                rt: _,
                ident: Ident(inner_ident),
                input: Some(box Expression::Array(Array(arr))),
            } if inner_ident == "add" => {
                let Some([lhs, rhs]) = arr.get_mut(..) else {
                    todo!()
                };
                match (&lhs, &rhs) {
                    (Expression::Literal(Literal(a)), Expression::Literal(Literal(b))) => {
                        // Update the expression
                        current.expr = Expression::Literal(Literal(*a + *b));

                        // Remove previous assignments that got overwritten
                        if let Some(mut preceding) = before.current() {
                            loop {
                                if let StatementType::Assign(Assign {
                                    ident: before_ident,
                                    expr: before_expr,
                                }) = &preceding.0
                                {
                                    if current.ident == *before_ident {
                                        // TODO Evaluate the call for side affects.
                                        // A call may have side affects so we can't remove it.
                                        if let Expression::Call(_) = before_expr {
                                        } else {
                                            before.remove_current();
                                        }
                                    }
                                }

                                let Some(x) = before.peek_move_preceding() else {
                                    break;
                                };
                                preceding = x;
                            }
                        }
                    }
                    // TODO This is messy, clean this up.
                    (Expression::Ident(a), Expression::Ident(b)) => {
                        let mut hit = false;
                        let mut a_val = None;
                        let mut b_val = None;

                        // Search for the identifier values and remove previous assignments that got overwritten.
                        let mut used = false;

                        if let Some(mut preceding) = before.current() {
                            loop {
                                #[allow(clippy::single_match)]
                                match &preceding.0 {
                                    StatementType::Assign(Assign {
                                        ident: before_ident,
                                        expr: before_expr,
                                    }) => {
                                        if current.ident == *before_ident {
                                            // If assigning to a variable that has been used before its next assignment.
                                            if used {
                                                // The variable can be used in its own re-assignment.
                                                used = before_expr.contains(&current.ident);
                                            }
                                            // If assigning to a variable that is not used until its next assignment.
                                            else {
                                                // An unknown or call may have side affect so we cannot easily remove it.
                                                match before_expr {
                                                    Expression::Call(_)
                                                    | Expression::Unknown(_) => {}
                                                    _ => {
                                                        before.remove_current();
                                                    }
                                                }
                                            }
                                        } else if before_ident == a {
                                            #[allow(clippy::single_match)]
                                            match before_expr {
                                                Expression::Literal(Literal(inner_literal)) => {
                                                    a_val = Some(*inner_literal);
                                                }
                                                // This is a todo, but for now we currently don't explore this.
                                                _ => {}
                                            }
                                            if hit {
                                                break;
                                            } else {
                                                hit = true;
                                            }
                                        } else if before_ident == b {
                                            #[allow(clippy::single_match)]
                                            match before_expr {
                                                Expression::Literal(Literal(inner_literal)) => {
                                                    b_val = Some(*inner_literal);
                                                }
                                                // This is a todo, but for now we currently don't explore this.
                                                _ => {}
                                            }
                                            if hit {
                                                break;
                                            } else {
                                                hit = true;
                                            }
                                        }
                                    }
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }

                                let Some(x) = before.peek_move_preceding() else {
                                    break; // TODO Will this only trigger if a variable is not defined? Should this then panic?
                                };
                                preceding = x;
                            }
                        }

                        match (a_val, b_val) {
                            (Some(x), Some(y)) => {
                                current.expr = Expression::Literal(Literal(x + y));
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
                    (Expression::Ident(expr_ident), Expression::Literal(Literal(expr_literal)))
                    | (Expression::Literal(Literal(expr_literal)), Expression::Ident(expr_ident)) =>
                    {
                        // Search for the identifier value and if present remove a previous
                        // assignments that is overwritten.
                        let mut used = false;
                        let mut ident_val = None;
                        if let Some(mut preceding) = before.current() {
                            loop {
                                match &preceding.0 {
                                    StatementType::Assign(Assign {
                                        ident: before_ident,
                                        expr: before_expr,
                                    }) if current.ident == *before_ident => {
                                        // If assigning to a variable that is used before its next assignment.
                                        if used {
                                            // The variable can be used in its own re-assignment.
                                            used = before_expr.contains(&current.ident);
                                        }
                                        // If assigning to a variable that is not used before its next assignment.
                                        else {
                                            // An unknown or call may have side affect so we cannot easily remove it.
                                            match before_expr {
                                                Expression::Call(_) | Expression::Unknown(_) => {}
                                                _ => {
                                                    before.remove_current();
                                                }
                                            }
                                        }
                                    }
                                    StatementType::Assign(Assign {
                                        ident: before_ident,
                                        expr: before_expr,
                                    }) if expr_ident == before_ident => {
                                        match before_expr {
                                            Expression::Literal(Literal(inner_literal)) => {
                                                ident_val = Some(*inner_literal);
                                            }
                                            // This is a todo, but for now we currently don't explore this.
                                            _ => {}
                                        }
                                        break;
                                    }
                                    // This is a todo, but for now we currently don't explore this.
                                    _ => {}
                                }
                                let Some(x) = before.peek_move_preceding() else {
                                    break; // TODO Will this only trigger if a variable is not defined? Should this then panic?
                                };
                                preceding = x;
                            }
                        }

                        if let Some(x) = ident_val {
                            current.expr = Expression::Literal(Literal(x + expr_literal));
                        }
                    }
                    _ => todo!(),
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
                                            _ => break,
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
                    let Some(x) = before.peek_move_preceding() else {
                        break;
                    };
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
    use std::mem::size_of_val;
    

    // Two-sum <i8, u8>
    #[test]
    fn middle_2sumi8u8() {
        let text = r#"target = rt read:{0,1,}
length = rt read:{0,1,}
input = rt read:{0,length,}
set = ?
i = 0
loop
    if eq:{i,length,}
        break
    difference = sub:{target,index:{set,i,},}
    if index:{set,difference,}
        exit:0
    i = add:{i,1,}
exit:1
"#;
        let mut tree = statements(text.as_bytes());

        let result = tree.to_string();
        println!("{result}");

        evaluate_tree(&mut tree);
        println!("{result}");
    }

    #[test]
    fn middle_one() {
        // tracing_subscriber::fmt::fmt()
        //     .with_file(true)
        //     .with_line_number(true)
        //     .init();

        const INPUT: i32 = 3;
        // A pipe our code can read from.
        let r0 = {
            let mut arr = [libc::c_int::default(), libc::c_int::default()];
            let res = unsafe { libc::pipe(arr.as_mut_ptr().cast()) };
            assert_eq!(res, 0);
            let [r, w] = arr;

            let mut temp = INPUT.to_ne_bytes();
            let res = unsafe { libc::write(w, temp.as_mut_ptr().cast(), size_of_val(&temp)) };
            assert_eq!(res, size_of_val(&temp) as isize);
            unsafe { libc::close(w) };
            r
        };
        // A pipe our code can write to.
        let (r1, w1) = {
            let mut arr = [libc::c_int::default(), libc::c_int::default()];
            let res = unsafe { libc::pipe(arr.as_mut_ptr().cast()) };
            assert_eq!(res, 0);
            let [r, w] = arr;
            (r, w)
        };

        let text = format!(
            "x = read:{{{r0},4,}}
a = write:{{{w1},x,}}
y = add:{{2,x,}}
x = y
if 1
    c = add:{{y,x,}}
    x = c
b = rt write:{{0,x,}}
"
        );
        let mut tree = statements(text.as_bytes());
        evaluate_tree(&mut tree);

        println!("tree: {tree}");

        let mut iter = tree.iter();
        // Assert `text` is evaluated to
        // ```text
        // rt write:{0,10}
        // ```
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("b")),
                    expr: Expression::Call(Call {
                        rt: true,
                        ident: Ident(String::from("write")),
                        input: Some(Box::new(Expression::Array(Array(vec![
                            Expression::Literal(Literal(0)),
                            Expression::Literal(Literal(10))
                        ]))))
                    })
                })),
                depth: 0
            })
        );
        assert_eq!(iter.next(), None);

        let mut temp = [u8::default(); std::mem::size_of::<i32>()];
        let res = unsafe { libc::read(r1, temp.as_mut_ptr().cast(), size_of::<LiteralValue>()) };
        assert_eq!(res, size_of::<LiteralValue>() as isize);
        assert_eq!(i32::from_ne_bytes(temp), INPUT);
        unsafe { libc::close(w1) };
        unsafe { libc::close(r1) };

        unsafe { libc::close(r0) };
    }

    #[test]
    fn middle_two() {
        let text = r#"x = rt read:{0,4,}
y = 2
y = x
a = 2
b = 3
c = add:{a,b,}
d = rt read:{0,4,}
if 1
    c = add:{d,c,}
    d = 4
a = d
exit:a
exit:a
"#;
        let mut tree = statements(text.as_bytes());
        evaluate_tree(&mut tree);

        println!("{tree}");

        let mut iter = tree.iter();
        // Assert `text` is evaluated to
        // ```text
        // x = rt read:{0,4,}
        // d = rt read:{0,4,}
        // exit:4
        // ```
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("x")),
                    expr: Expression::Call(Call {
                        rt: true,
                        ident: Ident(String::from("read")),
                        input: Some(Box::new(Expression::Array(Array(vec![
                            Expression::Literal(Literal(0)),
                            Expression::Literal(Literal(4))
                        ]))))
                    })
                })),
                depth: 0
            })
        );
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Assign(Assign {
                    ident: Ident(String::from("d")),
                    expr: Expression::Call(Call {
                        rt: true,
                        ident: Ident(String::from("read")),
                        input: Some(Box::new(Expression::Array(Array(vec![
                            Expression::Literal(Literal(0)),
                            Expression::Literal(Literal(4))
                        ]))))
                    })
                })),
                depth: 0
            })
        );
        assert_eq!(
            iter.next(),
            Some(Element {
                item: &Statement(StatementType::Call(Call {
                    rt: false,
                    ident: Ident(String::from("exit")),
                    input: Some(Box::new(Expression::Literal(Literal(4))))
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
