use colored::Colorize;
use std::fmt;
use std::string::ToString;

/// Keyword enum to ensure consistency
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Break;
impl Break {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        KeyWord::Break.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub ident: Ident,
    pub args: Vec<Value>,
}
impl Call {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{}({})",
            self.ident.print(n).yellow(),
            self.args
                .iter()
                .map(|a| format!("{},",a.print(n)))
                .collect::<String>()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Loop(Option<Box<Statement>>);
impl Loop {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!(
            "{}{}",
            KeyWord::Loop,
            if let Some(s) = &self.0 {
                s.print(n + 1)
            } else {
                String::new()
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub ident: Ident,
    pub args: Vec<Ident>,
    pub inner: Option<Box<Statement>>,
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
            if let Some(s) = &self.inner {
                s.print(n + 1)
            } else {
                String::new()
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Return(pub Value);
impl Return {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        format!("{} {}", KeyWord::Return, self.0.print(n))
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub ident: Ident,
    pub expr: Expression,
}
impl Assign {
    pub fn print(&self, n: usize) -> String {
        format!("{} = {}", self.ident.print(n), self.expr.print(n),)
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Value,
    pub inner: Option<Box<Statement>>,
}
impl If {
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {}{}",
            KeyWord::If,
            self.cond.print(n),
            if let Some(s) = &self.inner {
                s.print(n + 1)
            } else {
                String::new()
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Unknown,
    Literal(Literal),
    Ident(Ident),
    Call(Call),
}
impl Value {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Unknown => "?".truecolor(255,165,0).to_string(),
            Self::Literal(l) => l.print(n),
            Self::Ident(i) => i.print(n),
            Self::Call(c) => c.print(n),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Literal(i32);
impl Literal {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        self.0.to_string().blue().to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Ident(String);
impl Ident {
    #[must_use]
    pub fn print(&self, _n: usize) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Unary(Unary),
    Binary(Binary),
}
impl Expression {
    #[must_use]
    pub fn print(&self, n: usize) -> String {
        match self {
            Self::Unary(x) => x.print(n),
            Self::Binary(x) => x.print(n),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    block: StatementType,
    next: Option<Box<Statement>>,
}
impl Statement {
    pub const INDENT: &str = "    ";
    pub fn print(&self, n: usize) -> String {
        format!(
            "\n{}{}{}",
            Self::INDENT.repeat(n),
            self.block.print(n),
            if let Some(s) = &self.next {
                s.print(n)
            } else {
                String::new()
            }
        )
    }
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub struct Unary(Value);
impl Unary {
    pub fn print(&self, n: usize) -> String {
        self.0.print(n)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    lhs: Value,
    op: Op,
    rhs: Value,
}
impl Binary {
    pub fn print(&self, n: usize) -> String {
        format!(
            "{} {} {}",
            self.lhs.print(n),
            self.op.print(n),
            self.rhs.print(n)
        )
    }
}

#[derive(Debug, Clone, Copy)]
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
    pub fn run(&self, a: usize, b: usize) -> usize {
        match self {
            Self::Add => a + b,
            Self::Sub => a - b,
            Self::Mul => a * b,
            Self::Div => a / b,
        }
    }
}

peg::parser!(pub grammar parser() for str {
    pub rule statements(n:usize) -> Option<Statement>
        = indent(n) a:statement_type(n) "\n"* b:statements(n) {
            Some(Statement {
                block: a,
                next: b.map(|x|Box::new(x))
            })
        }
            / { None }

    pub rule statement_type(n:usize) -> StatementType
        = i:if_rule(n) { StatementType::If(i) }
            / l:simple_loop(n) { StatementType::Loop(l) }
            / a:assignment(n) { StatementType::Assign(a) }
            / c:call() { StatementType::Call(c) }
            / f:function(n) { StatementType::Function(f) }
            / "break" { StatementType::Break(Break) }
            / "return" _ v:value() { StatementType::Return(Return(v)) }

    pub rule if_rule(n:usize) -> If
        = "if" _ cond:value() "\n" then:statements(n+1) {
            If {
                cond,
                inner: then.map(|x|Box::new(x))
            }
        }

    rule simple_loop(n:usize) -> Loop
        = "loop" "\n" s:statements(n+1) { Loop(s.map(|x|Box::new(x))) }

    rule assignment(n:usize) -> Assign
        = ident:identifier() _ "=" _ expr:expression() { Assign { ident, expr } }

    rule expression() -> Expression
        = lhs:value() _ op:op() _ rhs:value() {
            Expression::Binary(Binary{
                lhs, op, rhs
            })
        }
            / v:value() { Expression::Unary(Unary(v)) }

    rule value() -> Value
        = c:call() { Value::Call(c) }
            / i:identifier() { Value::Ident(i) }
            / l:literal() { Value::Literal(l) }
            / "?" { Value::Unknown }

    rule call() -> Call
        = ident:identifier() "(" args:((v:value() "," {v})*) ")" { Call { ident, args } }

    rule function(n:usize) -> Function
        = "def" _ ident:identifier() "(" args:((arg:identifier() "," {arg})*) ")" "\n"
        s:statements(n+1) { Function { ident, args, inner: s.map(|x|Box::new(x))} }

    rule op() -> Op
        = "+" { Op::Add }
        / "-" { Op::Sub }
        / "*" { Op::Mul }
        / "/" { Op::Div }

    rule identifier() -> Ident
        =  n:$(['a'..='z' | 'A'..='Z' | '_']+) { Ident(String::from(n)) }
        / expected!("identifier")

    rule literal() -> Literal
        = n:$(['0'..='9']+) { Literal(n.parse().unwrap()) }

    rule indent(n:usize) = quiet!{"    "*<{n}>}

    rule _() =  quiet!{" "}
});

#[cfg(test)]
mod tests {
    // use std::collections::HashMap;

    use super::*;

    #[test]
    fn simple() {
        let string = std::fs::read_to_string("./example-input.txt").unwrap();
        let statement = parser::statements(&string, 0).unwrap().unwrap();
        // println!("statements: {statements:?}");
        println!("{}", statement.print(0));
    }

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
