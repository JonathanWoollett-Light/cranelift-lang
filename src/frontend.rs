use colored::Colorize;
use std::fmt;
use std::string::ToString;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ExprLit(pub String);
impl ExprLit {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        format!("{}", self.0.blue())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ExprIdent(pub String);
impl ExprIdent {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        self.0.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct ExprCond {
    pub lhs: Value,
    pub cmp: Cmp,
    pub rhs: Value,
}
impl ExprCond {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {} {}",
            self.lhs.print(n),
            self.cmp.print(n),
            self.rhs.print(n)
        )
    }
}

#[derive(Debug, Clone)]
pub struct ExprBreak;
impl ExprBreak {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        KeyWord::Break.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub ident: ExprIdent,
    pub args: Vec<Value>,
}

impl ExprCall {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{}({})",
            self.ident.print(n).yellow(),
            self.args
                .iter()
                .map(|a| a.print(n))
                .intersperse(String::from(","))
                .collect::<String>()
        )
    }
}

#[derive(Debug)]
pub struct ExprLoop(pub Vec<Statement>);
impl ExprLoop {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{}\n{}",
            KeyWord::Loop,
            self.0
                .iter()
                .map(|x| format!("{}{}\n", "\t".repeat(n + 1), x.print(n + 1)))
                .collect::<String>()
        )
    }
}

pub type ExprBinary = (Value, Op, Value);

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

#[derive(Debug)]
pub struct ExprFn {
    pub ident: ExprIdent,
    pub args: Vec<ExprIdent>,
    pub statements: Vec<Statement>,
}
impl ExprFn {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {}({})\n{}",
            KeyWord::Def,
            self.ident.print(n).yellow(),
            self.args
                .iter()
                .map(|x| x.print(n))
                .intersperse(String::from(","))
                .collect::<String>(),
            self.statements
                .iter()
                .map(|x| format!("{}{}\n", "\t".repeat(n + 1), x.print(n + 1)))
                .collect::<String>()
        )
    }
}

#[derive(Debug)]
pub struct ExprReturn(pub Value);
impl ExprReturn {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!("{} {}", KeyWord::Return, self.0.print(n))
    }
}

use std::cell::RefCell;

#[derive(Debug)]
pub struct ExprAssign {
    pub ident: ExprIdent,
    pub assign: Expr,
    pub val: RefCell<Vec<Option<usize>>>,
}
impl ExprAssign {
    fn new(ident: ExprIdent, assign: Expr) -> Self {
        Self {
            ident,
            assign,
            val: RefCell::new(Vec::new()),
        }
    }
}

impl ExprAssign {
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} = {} {}",
            self.ident.print(n),
            self.assign.print(n),
            format!(
                "[{}]",
                self.val
                    .borrow()
                    .iter()
                    .map(|x| x.map(|x| x.to_string()).unwrap_or(String::from("?")))
                    .intersperse(String::from(","))
                    .collect::<String>()
            )
            .black()
        )
    }
}

#[derive(Debug)]
pub struct ExprIf {
    pub cond: ExprCond,
    pub then: Vec<Statement>,
    pub val: RefCell<Vec<Option<bool>>>,
}
impl ExprIf {
    fn new(cond: ExprCond, then: Vec<Statement>) -> Self {
        Self {
            cond,
            then,
            val: RefCell::new(Vec::new()),
        }
    }

    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {} {}\n{}",
            KeyWord::If,
            self.cond.print(n),
            format!(
                "[{}]",
                self.val
                    .borrow()
                    .iter()
                    .map(|x| x.map(|y| y.to_string()).unwrap_or(String::from("?")))
                    .intersperse(String::from(","))
                    .collect::<String>()
            )
            .black(),
            self.then
                .iter()
                .map(|x| format!("{}{}", "\t".repeat(n + 1), x.print(n + 1)))
                .collect::<String>(),
        )
    }
}

#[derive(Debug, Clone)]
pub enum Cmp {
    Gt,
    Lt,
    Eq,
}
impl Cmp {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        match self {
            Self::Gt => ">",
            Self::Lt => "<",
            Self::Eq => "==",
        }
        .to_string()
    }
}

#[derive(Debug)]
pub enum Statement {
    Assign(ExprAssign),
    Loop(ExprLoop),
    If(ExprIf),
    Call(ExprCall),
    Break(ExprBreak),
    Function(ExprFn),
    Return(ExprReturn),
}

impl Statement {
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Assign(l) => l.print(n),
            Self::Loop(l) => l.print(n),
            Self::If(i) => i.print(n),
            Self::Call(c) => c.print(n),
            Self::Break(b) => b.print(n),
            Self::Function(fi) => fi.print(n),
            Self::Return(r) => r.print(n),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Literal(ExprLit),
    Identifier(ExprIdent),
    Call(ExprCall),
}

impl Value {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Literal(l) => l.print(n),
            Self::Identifier(i) => i.print(n),
            Self::Call(c) => c.print(n),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unary(Value),
    Binary(ExprBinary),
}

impl Expr {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Unary(v) => v.print(n),
            Self::Binary((a, op, b)) => format!("{} {} {}", a.print(n), op.print(n), b.print(n)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
        }
        .to_string()
    }
}

peg::parser!(pub grammar parser() for str {
    pub rule statements(n:usize) -> Vec<Statement>
        = n:((indent(n) s:statement(n) "\n"* {s})*) { n }

    pub rule statement(n:usize) -> Statement
        = i:if_rule(n) { Statement::If(i) }
            / l:simple_loop(n) { Statement::Loop(l) }
            / a:assignment(n) { Statement::Assign(a) }
            / c:call() { Statement::Call(c) }
            / f:function(n) { Statement::Function(f) }
            / "break" { Statement::Break(ExprBreak) }
            / "return" _ v:value() { Statement::Return(ExprReturn(v)) }

    pub rule if_rule(n:usize) -> ExprIf
        = "if" _ cond:cmp(n) "\n" then:statements(n+1)
        { ExprIf::new(cond,then) }

    rule simple_loop(n:usize) -> ExprLoop
        = "loop" "\n" s:statements(n+1) { ExprLoop(s) }

    rule assignment(n:usize) -> ExprAssign
        = i:identifier() _ "=" _ rhs:assignment_rhs() {ExprAssign::new(i, rhs)}

    rule assignment_rhs() -> Expr
        = a:value() _ op:op() _ b:value() { Expr::Binary((a,op,b)) }
            / v:value() { Expr::Unary(v) }

    rule value() -> Value
        = c:call() { Value::Call(c) }
            / i:identifier() { Value::Identifier(i) }
            / l:literal() { Value::Literal(l) }

    rule call() -> ExprCall
        = ident:identifier() "(" args:((v:value() "," {v})*) ")" { ExprCall { ident, args } }

    rule function(n:usize) -> ExprFn
        = "def" _ ident:identifier() "(" args:((arg:identifier() "," {arg})*) ")" "\n"
        statements:statements(n+1) { ExprFn { ident, args, statements} }

    rule cmp(n:usize) -> ExprCond
        = lhs:value() _ cmp:cond() _ rhs:value() { ExprCond { lhs, cmp, rhs } }
    rule cond() -> Cmp
        = "==" { Cmp::Eq }
        / "<" { Cmp::Lt }
        / ">" { Cmp::Gt }

    rule op() -> Op
        = "+" { Op::Add }
        / "-" { Op::Sub }
        / "*" { Op::Mul }
        / "/" { Op::Div }

    rule identifier() -> ExprIdent
        =  n:$(['a'..='z' | 'A'..='Z' | '_']+) { ExprIdent(String::from(n)) }
        / expected!("identifier")

    rule literal() -> ExprLit
        = n:$(['0'..='9']+) { ExprLit(String::from(n)) }

    rule indent(n:usize) = quiet!{"    "*<{n}>}

    rule _() =  quiet!{" "}
});

#[cfg(test)]
mod tests {
    // use std::collections::HashMap;

    // use super::*;
    // #[test]
    // fn simple_loop_test() {
    //     let out = parser::statement(r#"c = 2"#).unwrap();
    //     println!("statement: {out:?}");
    //     let out = parser::statements(r#"c = 2 b = 2"#).unwrap();
    //     println!("statements: {out:?}");
    //     let out = parser::statements(r#"
    //         c = 2
    //         b = 2
    //     "#).unwrap();
    //     println!("statements: {out:?}");
    //     let out = parser::statements(r#"
    //         a = 2
    //         if (2==3){
    //             c= 2
    //             b = 3
    //         } else {
    //         }
    //     "#).unwrap();
    //     println!("statements: {out:?}");
    // }
    // #[test]
    // fn read_file() {
    //     // let file = std::fs::File::open("../example-input").unwrap();
    //     let string = std::fs::read_to_string("./example-input").unwrap();
    //     let statements = parser::statements(&string, 0).unwrap();
    //     println!("statements: {statements:?}");
    // }
    // #[test]
    // fn function_test() {
    //     let out = parser::function(r#"
    //         fn foo(a, b) {
    //             c = if a {
    //                 if b {
    //                     30
    //                 } else {
    //                     40
    //                 }
    //             } else {
    //                 50
    //             }
    //             c = c + 2
    //         }
    //     "#).unwrap();
    //     println!("function_test: {out:?}");
    // }
}
