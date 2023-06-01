use crate::frontend::*;
use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Default)]
pub struct Context {
    /// The values of variables.
    variables: HashMap<String, LiteralValue>,
    /// The definitions of functions.
    functions: HashMap<String, Function>,
}

#[allow(clippy::match_same_arms)]
pub fn evaluate(statement: &Statement, context: &mut Context) -> Option<Box<Statement>> {
    match &statement.block {
        StatementType::Loop(l) => Some(Box::new(Statement {
            block: StatementType::Loop(Loop(l.0.as_ref().and_then(|s| evaluate(s, context)))),
            next: statement.next.clone(),
        })),
        StatementType::If(i) => {
            match &i.cond {
                Value::Literal(literal) => {
                    if literal.0 == 0 {
                        Some(Box::new(statement.clone()))
                    } else {
                        let mut inline = i.inner.as_ref().and_then(|s| evaluate(s, context));

                        // Find the last element in the linked list, and append the next element.
                        let mut last = &mut inline;
                        while let Some(next) = last {
                            last = &mut next.next;
                        }
                        debug_assert_eq!(*last, None);
                        *last = statement.next.as_ref().and_then(|s| evaluate(s, context));
                        inline
                    }
                }
                _ => Some(Box::new(statement.clone())),
            }
        }
        StatementType::Return(_) => Some(Box::new(statement.clone())),
        StatementType::Break(_) => Some(Box::new(statement.clone())),
        StatementType::Assign(assign) => match &assign.expr {
            Expression::Unary(unary) => match &unary.0 {
                // Value::Literal(_) => if let Some(s) = &statement.next {
                //     Some(*s.clone())
                // }
                // else {
                //     None
                // },
                Value::Literal(literal) => {
                    context.variables.insert(assign.ident.0.clone(), literal.0);
                    statement.next.as_ref().and_then(|s| evaluate(s, context))
                }
                Value::Unknown => Some(Box::new(Statement {
                    block: statement.block.clone(),
                    next: statement.next.as_ref().and_then(|s| evaluate(s, context)),
                })),
                _ => Some(Box::new(statement.clone())),
            },
            Expression::Binary(binary) => match (&binary.lhs, &binary.rhs) {
                (Value::Ident(a), Value::Ident(b)) => {
                    match (context.variables.get(&a.0), context.variables.get(&b.0)) {
                        (Some(x), Some(y)) => {
                            let z = binary.op.run(*x, *y);
                            context.variables.insert(assign.ident.0.clone(), z);
                            statement.next.as_ref().and_then(|s| evaluate(s, context))
                        }
                        (Some(x), None) => Some(Box::new(Statement {
                            block: StatementType::Assign(Assign {
                                ident: assign.ident.clone(),
                                expr: Expression::Binary(Binary {
                                    lhs: Value::Literal(Literal(*x)),
                                    op: binary.op,
                                    rhs: binary.rhs.clone(),
                                }),
                            }),
                            next: statement.next.as_ref().and_then(|s| evaluate(s, context)),
                        })),
                        (None, Some(y)) => Some(Box::new(Statement {
                            block: StatementType::Assign(Assign {
                                ident: assign.ident.clone(),
                                expr: Expression::Binary(Binary {
                                    lhs: binary.lhs.clone(),
                                    op: binary.op,
                                    rhs: Value::Literal(Literal(*y)),
                                }),
                            }),
                            next: statement.next.as_ref().and_then(|s| evaluate(s, context)),
                        })),
                        _ => Some(Box::new(statement.clone())),
                    }
                }
                _ => Some(Box::new(statement.clone())),
            },
        },
        StatementType::Call(_) => Some(Box::new(statement.clone())),
        StatementType::Function(_) => Some(Box::new(statement.clone())),
    }
}
// impl Return {
//     fn evaluate(&self, context: &mut Context) -> Vec<Statement> {
//         self.0.evaluate(ValueContainers::Return(self.clone()), context)
//     }
// }
// impl Value {
//     fn evaluate(&self, container: ValueContainers, context: &mut Context) -> Vec<Statement> {
//         match (self,container) {
//             (Self::Call(c),ValueContainers::Return(r)) => c.evaluate(ValueContainers::Return(r.clone()), context),
//             _ => todo!()
//         }
//     }
// }
// impl Call {
//     fn evaluate(&self, container: ValueContainers, context: &mut Context) -> Vec<Statement> {
//         vec![self.clone()]
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn middle() {
        let string = std::fs::read_to_string("./example-input.txt").unwrap();
        let statement = parser::statements(&string, 0).unwrap().unwrap();
        println!("{}", statement.print(0));

        let mut context = Context::default();
        let evaluated = evaluate(&statement, &mut context).unwrap();
        println!("{}", evaluated.print(0));
    }
}
