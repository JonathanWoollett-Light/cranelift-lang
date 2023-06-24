mod ast;
use linked_syntax_tree::OptionalNode;

pub use ast::*;
use std::cmp::Ordering;
use std::io::{BufRead, Read};

pub fn get_indent(bytes: &[u8]) -> usize {
    let mut indent = 0;
    while let Some(b' ') = bytes.get(indent) {
        indent += 1;
    }
    indent
}

pub fn statements(text: impl Read) -> OptionalNode<Statement> {
    let mut lines = std::io::BufReader::new(text).lines();
    let mut tree = OptionalNode::default();
    let mut cursor = tree.cursor_mut();
    let mut last_indent = 0;

    if let Some(line_result) = lines.next() {
        let line = line_result.unwrap().into_bytes();
        let indent = get_indent(&line);

        let string = std::str::from_utf8(&line[indent..]).unwrap();
        let statement = parser::statement(string).unwrap();

        cursor.insert_next(statement);

        for line_result in lines {
            let line = line_result.unwrap().into_bytes();
            let indent = get_indent(&line);

            let string = std::str::from_utf8(&line[indent..]).unwrap();
            let statement = parser::statement(string).unwrap();

            match indent.cmp(&last_indent) {
                Ordering::Greater => {
                    cursor.insert_child(statement);
                    cursor.move_child();
                }
                Ordering::Equal => {
                    cursor.insert_next(statement);
                    cursor.move_next();
                }
                Ordering::Less => {
                    cursor.move_parent();
                    cursor.insert_next(statement);
                    cursor.move_next();
                }
            }
            last_indent = indent;
        }
    }

    tree
}

peg::parser!(pub grammar parser() for str {
    pub rule statement() -> Statement = s:statement_type() { Statement(s) }

    rule statement_type() -> StatementType
        = i:if_rule() { StatementType::If(i) }
            / l:loop() { StatementType::Loop(l) }
            / a:assignment() { StatementType::Assign(a) }
            / c:call() { StatementType::Call(c) }
            / f:function() { StatementType::Function(f) }
            / b:break() { StatementType::Break(b) }
            / r:return() { StatementType::Return(r) }

    rule if_rule() -> If = "if" _ cond:value() { If { cond } }

    rule loop() -> Loop = "loop" { Loop }

    rule break() -> Break = "break" { Break }

    rule return() -> Return = "return" _ v:value() { Return(v) }

    rule assignment() -> Assign
        = ident:identifier() _ "=" _ expr:expression() { Assign { ident, expr } }

    rule expression() -> Expression
        = b:binary() { Expression::Binary(b) }
            / c:call() { Expression::Call(c) }
            / u:unknown() { Expression::Unknown(u) }
            / u:unary() { Expression::Unary(u) }

    rule unknown() -> Unknown = "?" { Unknown }
    rule unary() -> Unary = v:value() { Unary(v) }
    rule binary() -> Binary
        = lhs:value() _ op:op() _ rhs:value() { Binary { lhs, op,rhs } }
    rule call() -> Call
        = ident:identifier() "(" args:((v:value() "," {v})*) ")" { Call { ident, args } }

    rule value() -> Value
        = i:identifier() { Value::Ident(i) }
            / l:literal() { Value::Literal(l) }



    rule function() -> Function
        = "def" _ ident:identifier() "(" args:((arg:identifier() "," {arg})*) ")" { Function { ident, args } }

    rule op() -> Op
        = "+" { Op::Add }
        / "-" { Op::Sub }
        / "*" { Op::Mul }
        / "/" { Op::Div }

    // TODO Prevent users using '_' to start variables.
    rule identifier() -> Ident
        =  s:$(['a'..='z' | 'A'..='Z' | '_']+) { Ident(String::from(s)) }

    rule literal() -> Literal
        = l:$(['0'..='9']+) { Literal(l.parse().unwrap()) }

    rule _() =  quiet!{" "}
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn front() {
        let file = std::fs::File::open("./example-input.txt").unwrap();
        let tree = statements(file);
        println!("{tree}");
    }
}
