use colored::Colorize;
use std::fmt;
use std::string::ToString;

pub type LiteralValue = i32;

/// Keyword enum to ensure consistency
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum KeyWord {
    Def,
    If,
    Loop,
    Break,
}
impl fmt::Display for KeyWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                KeyWord::Def => "def",
                KeyWord::If => "if",
                KeyWord::Loop => "loop",
                KeyWord::Break => "break",
            }
            .purple()
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Break;
impl fmt::Display for Break {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", KeyWord::Break)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Call {
    pub ident: Ident,
    pub input: Value,
}
impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.ident.to_string().yellow(), self.input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Loop;
impl fmt::Display for Loop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", KeyWord::Loop)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function(pub Ident);
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", KeyWord::Def, self.0.to_string().yellow(),)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Assign {
    pub ident: Ident,
    pub expr: Expression,
}
impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.ident, self.expr)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct If {
    pub cond: Value,
}
impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", KeyWord::If, self.cond)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Literal(Literal),
    Ident(Ident),
}
impl Value {
    pub fn ident_mut(&mut self) -> Option<&mut Ident> {
        match self {
            Self::Ident(x) => Some(x),
            Self::Literal(_) => None,
        }
    }
    pub fn literal_mut(&mut self) -> Option<&mut Literal> {
        match self {
            Self::Literal(x) => Some(x),
            Self::Ident(_) => None,
        }
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Ident(ident) => write!(f, "{ident}"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal(pub LiteralValue);
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.to_string().blue())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident(pub String);
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.as_str() {
            "in" | "out" => write!(f, "{}", self.0.truecolor(255, 165, 0)),
            _ => write!(f, "{}", self.0),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unknown;
impl fmt::Display for Unknown {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "?".truecolor(255, 165, 0))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Unary(Unary),
    Binary(Binary),
    Call(Call),
    Unknown(Unknown),
    Array(Array),
    Index(Index),
}
impl Expression {
    pub fn unary_mut(&mut self) -> Option<&mut Unary> {
        match self {
            Self::Unary(x) => Some(x),
            _ => None,
        }
    }
    pub fn binary_mut(&mut self) -> Option<&mut Binary> {
        match self {
            Self::Binary(x) => Some(x),
            _ => None,
        }
    }
    pub fn call_mut(&mut self) -> Option<&mut Call> {
        match self {
            Self::Call(x) => Some(x),
            _ => None,
        }
    }
    pub fn unknown_mut(&mut self) -> Option<&mut Unknown> {
        match self {
            Self::Unknown(x) => Some(x),
            _ => None,
        }
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unary(x) => write!(f, "{x}"),
            Self::Binary(x) => write!(f, "{x}"),
            Self::Call(x) => write!(f, "{x}"),
            Self::Unknown(x) => write!(f, "{x}"),
            Self::Array(x) => write!(f, "{x}"),
            Self::Index(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Array(pub Vec<Value>);
impl Array {
    pub const LHS: &str = "{";
    pub const RHS: &str = "}";
}
impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            Self::LHS,
            self.0.iter().map(|v| format!("{v},")).collect::<String>(),
            Self::RHS
        )
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Index {
    pub ident: Ident,
    pub index: Value,
}
impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.ident, self.index)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Statement(pub StatementType);
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementType {
    Loop(Loop),
    If(If),
    Break(Break),
    Assign(Assign),
    Call(Call),
    Function(Function),
}
impl StatementType {
    pub fn loop_mut(&mut self) -> Option<&mut Loop> {
        match self {
            Self::Loop(x) => Some(x),
            _ => None,
        }
    }
    pub fn if_mut(&mut self) -> Option<&mut If> {
        match self {
            Self::If(x) => Some(x),
            _ => None,
        }
    }
    pub fn if_ref(&self) -> Option<&If> {
        match self {
            Self::If(x) => Some(x),
            _ => None,
        }
    }
    pub fn break_mut(&mut self) -> Option<&mut Break> {
        match self {
            Self::Break(x) => Some(x),
            _ => None,
        }
    }
    pub fn assign(&self) -> Option<&Assign> {
        match self {
            Self::Assign(x) => Some(x),
            _ => None,
        }
    }
    pub fn assign_mut(&mut self) -> Option<&mut Assign> {
        match self {
            Self::Assign(x) => Some(x),
            _ => None,
        }
    }
    pub fn call_mut(&mut self) -> Option<&mut Call> {
        match self {
            Self::Call(x) => Some(x),
            _ => None,
        }
    }
    pub fn function_mut(&mut self) -> Option<&mut Function> {
        match self {
            Self::Function(x) => Some(x),
            _ => None,
        }
    }
}
impl fmt::Display for StatementType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign(x) => write!(f, "{x}"),
            Self::Loop(x) => write!(f, "{x}"),
            Self::If(x) => write!(f, "{x}"),
            Self::Call(x) => write!(f, "{x}"),
            Self::Break(x) => write!(f, "{x}"),
            Self::Function(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unary(pub Value);
impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binary {
    pub lhs: Value,
    pub op: Op,
    pub rhs: Value,
}
impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}
impl Op {
    #[must_use]
    pub fn run(&self, a: LiteralValue, b: LiteralValue) -> LiteralValue {
        match self {
            Self::Add => a + b,
            Self::Sub => a - b,
            Self::Mul => a * b,
            Self::Div => a / b,
        }
    }
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}
