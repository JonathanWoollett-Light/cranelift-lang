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
    pub input: Box<Expression>,
}
impl Call {
    /// Returns all variable identifiers referenced.
    pub fn idents(&self) -> Vec<&Ident> {
        self.input.idents()
    }
    pub fn uses(&self, ident: &Ident) -> bool {
        self.input.contains(ident)
    }
}
impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.ident.to_string().yellow(), self.input)
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
        write!(f, "{} {}", KeyWord::Def, self.0.to_string().yellow())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Assign {
    pub ident: Ident,
    pub expr: Expression,
}
impl Assign {
    pub fn uses(&self, ident: &Ident) -> bool {
        self.expr.contains(ident)
    }
}
impl fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.ident, self.expr)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct If {
    pub cond: Expression,
}
impl If {
    pub fn uses(&self, ident: &Ident) -> bool {
        self.cond.contains(ident)
    }
}
impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", KeyWord::If, self.cond)
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
            // Function inputs/outputs
            "in" | "out" => write!(f, "{}", self.0.truecolor(255, 165, 0)),
            // Intrinsics
            "add" | "sub" | "mul" | "div" => write!(f, "{}", self.0.green()),
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
    Literal(Literal),
    Ident(Ident),
    Call(Call),
    Unknown(Unknown),
    Array(Array),
    Index(Index),
}
impl Expression {
    /// Returns all variable identifiers referenced.
    pub fn idents(&self) -> Vec<&Ident> {
        match self {
            Self::Literal(_) => Vec::new(),
            Self::Ident(ident) => vec![ident],
            Self::Call(call) => call.idents(),
            Self::Unknown(_) => Vec::new(),
            Self::Array(array) => array.idents(),
            Self::Index(index) => index.idents(),
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
    pub fn contains(&self, ident: &Ident) -> bool {
        match self {
            Self::Literal(_) => false,
            Self::Ident(inner_ident) if inner_ident == ident => true,
            Self::Call(Call {
                ident: _,
                input: inner_input,
            }) if inner_input.contains(ident) => true,
            Self::Unknown(_) => false,
            Self::Array(Array(array))
                if array
                    .iter()
                    .any(|a| matches!(a, Expression::Ident(i) if i == ident)) =>
            {
                true
            }
            Self::Index(Index {
                ident: inner_ident,
                index: inner_index,
            }) if inner_ident == ident || inner_index.contains(ident) => true,
            _ => false,
        }
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(x) => write!(f, "{x}"),
            Self::Ident(x) => write!(f, "{x}"),
            Self::Call(x) => write!(f, "{x}"),
            Self::Unknown(x) => write!(f, "{x}"),
            Self::Array(x) => write!(f, "{x}"),
            Self::Index(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Array(pub Vec<Expression>);
impl Array {
    pub const LHS: &str = "{";
    pub const RHS: &str = "}";

    /// Returns all variable identifiers referenced.
    pub fn idents(&self) -> Vec<&Ident> {
        self.0.iter().flat_map(|e| e.idents()).collect()
    }
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
    pub index: Box<Expression>,
}
impl Index {
    /// Returns all variable identifiers referenced.
    pub fn idents(&self) -> Vec<&Ident> {
        let mut temp = vec![&self.ident];
        temp.extend(self.index.idents());
        temp
    }
}
impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.ident, self.index)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Statement(pub StatementType);
impl Statement {
    pub fn uses(&self, ident: &Ident) -> bool {
        self.0.uses(ident)
    }
}
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
    pub fn uses(&self, ident: &Ident) -> bool {
        match self {
            Self::Loop(_) => false,
            Self::If(i) => i.uses(ident),
            Self::Break(_) => false,
            Self::Assign(a) => a.uses(ident),
            Self::Call(c) => c.uses(ident),
            Self::Function(_) => false,
        }
    }

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
