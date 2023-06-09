use colored::Colorize;
use std::collections::LinkedList;
use std::fmt;
use std::string::ToString;
/// Keyword enum to ensure consistency
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum KeyWord {
    Def,
    If,
    Loop,
    Break,
    Return,
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
                KeyWord::Return => "return",
            }
            .purple()
        )
    }
}

pub type LiteralValue = i32;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Break;
impl Break {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        KeyWord::Break.to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Call {
    pub ident: Ident,
    pub args: Vec<Value>,
}
impl Call {
    pub fn main() -> Self {
        Self {
            ident: Ident(String::new()),
            args: Vec::new(),
        }
    }
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{}({})",
            self.ident.print(n).yellow(),
            self.args
                .iter()
                .map(|a| format!("{},", a.print(n)))
                .collect::<String>()
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Loop(pub LinkedList<Statement>);
impl Loop {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!("{}{}", KeyWord::Loop, print(&self.0, n + 1))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub ident: Ident,
    pub args: Vec<Ident>,
    pub inner: LinkedList<Statement>,
}
impl Function {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {}({}){}",
            KeyWord::Def,
            self.ident.print(n).yellow(),
            self.args
                .iter()
                .map(|x| format!("{},", x.print(n)))
                .collect::<String>(),
            print(&self.inner, n + 1)
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Return(pub Value);
impl Return {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!("{} {}", KeyWord::Return, self.0.print(n))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Assign {
    pub ident: Ident,
    pub expr: Expression,
}
impl Assign {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!("{} = {}", self.ident.print(n), self.expr.print(n),)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct If {
    pub cond: Value,
    pub inner: LinkedList<Statement>,
}
impl If {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {}{}",
            KeyWord::If,
            self.cond.print(n),
            print(&self.inner, n + 1)
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Literal(Literal),
    Ident(Ident),
}
impl Value {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Literal(l) => l.print(n),
            Self::Ident(i) => i.print(n),
        }
    }
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal(pub LiteralValue);
impl Literal {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        self.0.to_string().blue().to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident(pub String);
impl Ident {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unknown;
impl Unknown {
    fn print(&self, _n: usize) -> String {
        "?".truecolor(255, 165, 0).to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Unary(Unary),
    Binary(Binary),
    Call(Call),
    Unknown(Unknown),
}
impl Expression {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Unary(x) => x.print(n),
            Self::Binary(x) => x.print(n),
            Self::Call(c) => c.print(n),
            Self::Unknown(c) => c.print(n),
        }
    }
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Statement(pub StatementType);
impl Statement {
    pub const INDENT: &str = "    ";
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!("\n{}{}", Self::INDENT.repeat(n), self.0.print(n),)
    }
}

#[must_use]
pub fn print(list: &LinkedList<Statement>, n: usize) -> String {
    list.iter().map(|statement| statement.print(n)).collect()
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementType {
    Loop(Loop),
    If(If),
    Return(Return),
    Break(Break),
    Assign(Assign),
    Call(Call),
    Function(Function),
}
impl StatementType {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Assign(a) => a.print(n),
            Self::Loop(l) => l.print(n),
            Self::If(i) => i.print(n),
            Self::Call(c) => c.print(n),
            Self::Break(b) => b.print(n),
            Self::Function(f) => f.print(n),
            Self::Return(r) => r.print(n),
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
    pub fn return_mut(&mut self) -> Option<&mut Return> {
        match self {
            Self::Return(x) => Some(x),
            _ => None,
        }
    }
    pub fn break_mut(&mut self) -> Option<&mut Break> {
        match self {
            Self::Break(x) => Some(x),
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unary(pub Value);
impl Unary {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        self.0.print(n)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binary {
    pub lhs: Value,
    pub op: Op,
    pub rhs: Value,
}
impl Binary {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {} {}",
            self.lhs.print(n),
            self.op.print(n),
            self.rhs.print(n)
        )
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
    pub fn print(&self, _n: usize) -> String {
        String::from(match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
        })
    }
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
