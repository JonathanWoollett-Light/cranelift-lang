use super::value::ConstValue;

pub enum ConstBinaryValue {
    Call(ConstCall),
    Identifier(String),
    Unknown,
}

pub enum ConstBinary {
    Unknown(ConstBinaryValue, Op, ConstBinaryValue),
    KnownLhs(usize, Op, ConstBinaryValue),
    KnownRhs(ConstBinaryValue, Op, usize),
    Known(usize),
}

impl From<(ConstValue, Op, ConstValue)> for ConstBinary {
    fn from((a, op, b): (ConstValue, Op, ConstValue)) -> Self {
        match (a, b) {
            (ConstValue::Literal(a), ConstValue::Literal(b)) => ConstBinary::Known(op.run(a, b)),
            (ConstValue::Literal(a), ConstValue::Identifier(b)) => {
                ConstBinary::KnownLhs(a, op, ConstBinaryValue::Identifier(b))
            }
            (ConstValue::Literal(a), op, ConstValue::Call(b)) => {
                ConstBinary::KnownLhs(a, op, ConstBinaryValue::Call(b))
            }
            (ConstValue::Literal(a), ConstValue::Unknown) => {
                ConstBinary::KnownLhs(a, op, ConstBinaryValue::Unknown)
            }
            (ConstValue::Identifier(a), ConstValue::Literal(b)) => {
                ConstBinary::KnownRhs(ConstBinaryValue::Identifier(a), op, b)
            }
            (ConstValue::Identifier(a), ConstValue::Call(b)) => {
                ConstBinary::Unknown(ConstBinaryValue::Identifier(a), op, ConstBinaryValue::Call(b))
            }
            (ConstValue::Identifier(a), ConstValue::Unknown(b)) => {
                ConstBinary::Unknown(ConstBinaryValue::Identifier(a), op, ConstBinaryValue::Unknown)
            }
            (ConstValue::Call(a), ConstValue::Literal(b)) => {
                ConstBinary::KnownRhs(ConstBinaryValue::Call(a), op, b)
            }
            (ConstValue::Call(a), ConstValue::Identifier(b)) => {
                ConstBinary::Unknown(ConstBinaryValue::Call(a), op, ConstBinaryValue::Identifier(b))
            }
            (ConstValue::Call(a), ConstValue::Call(b)) => {
                ConstBinary::Unknown(ConstBinaryValue::Call(a), op, ConstBinaryValue::Call(b))
            }
            (ConstValue::Call(a), ConstValue::Unknown) => {
                ConstBinary::Unknown(ConstBinaryValue::Call(a), op, ConstBinaryValue::Unknown)
            }
            (ConstValue::Unknown, ConstValue::Literal(b)) => {
                ConstBinary::KnownRhs(ConstBinaryValue::Unknown, op, b)
            }
            (ConstValue::Unknown, ConstValue::Identifier(b)) => {
                ConstBinary::Unknown(ConstBinaryValue::Unknown, op, ConstValue::Identifier(b))
            }
            (ConstValue::Unknown, ConstValue::Call(b)) => {
                ConstBinary::Unknown(ConstBinaryValue::Unknown, op, ConstValue::Call(b))
            }
            (ConstValue::Unknown, ConstValue::Unknown) => {
                ConstBinary::Unknown(ConstBinaryValue::Unknown, op, ConstValue::Unknown)
            }
        }
    }
}

/// Evaluate binary expression.
pub fn evaluate_binary(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    const_program: &mut Vec<Statement>,
    (a, op, b): &ExprBinary,
) -> (Option<usize>, ConstBinary) {
    let ((rhs_opt, rhs_val), (lhs_opt, lhs_val)) = (
        evaluate_value(values, functions, const_program, a),
        evaluate_value(values, functions, const_program, b),
    );

    let res = ConstBinary::from((lhs_val,op,rhs_val));

    if let ConstBinary::Known(x) = res {
        (Some(x),res)
    }
    else {
        (None,res)
    }
}