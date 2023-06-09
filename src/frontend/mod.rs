mod ast;
use std::collections::LinkedList;

pub use ast::*;

peg::parser!(pub grammar parser() for str {
    pub rule statements(n:usize) -> LinkedList<Statement>
        = indent(n) a:statement_type(n) "\n"* b:statements(n) {
            let mut list = LinkedList::from([Statement(a)]);
            list.extend(b);
            list
        }
            / { LinkedList::new() }

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
                inner: then
            }
        }

    rule simple_loop(n:usize) -> Loop
        = "loop" "\n" s:statements(n+1) { Loop(s) }

    rule assignment(n:usize) -> Assign
        = ident:identifier() _ "=" _ expr:expression() { Assign { ident, expr } }

    rule expression() -> Expression
        = lhs:value() _ op:op() _ rhs:value() {
            Expression::Binary(Binary{
                lhs, op, rhs
            })
        }
            / c:call() { Expression::Call(c) }
            / v:value() { Expression::Unary(Unary(v)) }
            / "?" { Expression::Unknown(Unknown) }

    rule value() -> Value
        = i:identifier() { Value::Ident(i) }
            / l:literal() { Value::Literal(l) }

    rule call() -> Call
        = ident:identifier() "(" args:((v:value() "," {v})*) ")" { Call { ident, args } }

    rule function(n:usize) -> Function
        = "def" _ ident:identifier() "(" args:((arg:identifier() "," {arg})*) ")" "\n"
        s:statements(n+1) { Function { ident, args, inner: s} }

    rule op() -> Op
        = "+" { Op::Add }
        / "-" { Op::Sub }
        / "*" { Op::Mul }
        / "/" { Op::Div }

    // TODO Prevent users using '_' to start variables.
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
    use super::*;

    #[test]
    fn front() {
        let string = std::fs::read_to_string("./example-input.txt").unwrap();
        let statement = parser::statements(&string, 0).unwrap();
        println!("{}", print(&statement, 0));
    }
}
