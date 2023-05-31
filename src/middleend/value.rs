use super::call::ConstCall;

pub enum ConstValue {
    Known(usize),
    /// An identifier for an unknown value.
    Identifier(String),
    /// A function call that returns an unknown value.
    Call(Vec<super::call::ConstCallStatement>),
    /// An unknown special character.
    Unknown,
}
impl From<ConstCall> for ConstValue {
    fn from(const_call: ConstCall) -> Self {
        match const_call {
            ConstCall::Known(x) => ConstValue::Known(x),
            ConstCall::Unknown(x) => ConstValue::Call(x)
        }
    }
}

/// Given a value returns a `usize` if it can be determined at compile time.
///
/// # Panics
///
/// When attempting to parse literal as `usize`.
pub fn evaluate_value(
    values: &mut ValueMap,
    functions: &mut FunctionMap,
    const_program: &mut Vec<Statement>,
    value: &Value,
) -> ConstValue {
    match value {
        Value::Literal(ExprLit(literal)) => {
            let x = literal.parse::<usize>().expect("Failed to parse literal.");
            ConstValue::Known(x)
        }
        Value::Identifier(ident) => {
            let x = values.get(ident);
            if let Some(CheckingValue { current, .. }) = x {
                ConstValue::Known(*current)
            } else {
                ConstValue::Identifier(ident.0)
            }
        }
        Value::Call(call) => {
            let const_call = super::call::evaluate_call(values, functions, const_program, call);
            ConstValue::from(const_call)
        }
        Value::Unknown => ConstValue::Unknown,
    }
}