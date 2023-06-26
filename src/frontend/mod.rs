mod ast;
use linked_syntax_tree::SyntaxTree;

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

const INDENT_STEP: usize = 4;

pub fn statements(text: impl Read) -> SyntaxTree<Statement> {
    let mut lines = std::io::BufReader::new(text).lines();
    let mut tree = SyntaxTree::default();
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
                Ordering::Equal | Ordering::Less => {
                    let mut temp = indent;
                    while temp.cmp(&last_indent) == Ordering::Less {
                        cursor.move_parent();
                        temp += INDENT_STEP;
                    }
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

    rule if_rule() -> If = "if" _ cond:value() { If { cond } }

    rule loop() -> Loop = "loop" { Loop }

    rule break() -> Break = "break" { Break }

    rule assignment() -> Assign
        = ident:identifier() _ "=" _ expr:expression() { Assign { ident, expr } }

    rule expression() -> Expression
        = b:binary() { Expression::Binary(b) }
            / c:call() { Expression::Call(c) }
            / u:unknown() { Expression::Unknown(u) }
            / a:array() { Expression::Array(a) }
            / i:index() { Expression::Index(i) }
            / u:unary() { Expression::Unary(u) }

    rule unknown() -> Unknown = "?" { Unknown }
    rule unary() -> Unary = v:value() { Unary(v) }
    rule binary() -> Binary
        = lhs:value() _ op:op() _ rhs:value() { Binary { lhs, op,rhs } }
    rule call() -> Call
        = ident:identifier() "(" input:value() ")" { Call { ident, input } }
    rule array() -> Array
        = "{" args:((v:value() "," {v})*) "}" { Array(args) }
    rule index() -> Index
        = ident:identifier() "[" index:value() "]" { Index { ident, index } }

    rule value() -> Value
        = i:identifier() { Value::Ident(i) }
            / l:literal() { Value::Literal(l) }



    rule function() -> Function
        = "def" _ ident:identifier() { Function(ident) }

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
        let text = r#"x = ?
y = 2
y = x
a = 2
b = 3
c = a + b
d = ?
if 1
    c = d + c
    d = 4
a = d
loop
    a = a + c
    if 1
        break
def triple_add
    a = in[0]
    b = in[1]
    c = in[2]
    one = a + b
    two = one + c
    if 0
        three = {two,two,two,}
        two = triple_add(three)
    out = two
temp = {a,1,b,}
a = triple_add(temp)
e = c + d
e = e + a
x = ?
e = x + e
if 1
    out = e
out = 1
"#;
        let tree = statements(text.as_bytes());

        let result = tree.to_string();
        println!("{result}");

        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("out.txt")
            .unwrap();
        file.write_all(result.as_bytes()).unwrap();

        // TODO Can we make this expected string look less bad?
        let expected = r#"x = [38;2;255;165;0m?[0m
y = [34m2[0m
y = x
a = [34m2[0m
b = [34m3[0m
c = a + b
d = [38;2;255;165;0m?[0m
[35mif[0m [34m1[0m
    c = d + c
    d = [34m4[0m
a = d
[35mloop[0m
    a = a + c
    [35mif[0m [34m1[0m
        [35mbreak[0m
[35mdef[0m triple_add
    a = [38;2;255;165;0min[0m[[34m0[0m]
    b = [38;2;255;165;0min[0m[[34m1[0m]
    c = [38;2;255;165;0min[0m[[34m2[0m]
    one = a + b
    two = one + c
    [35mif[0m [34m0[0m
        three = {two,two,two,}
        two = triple_add(three)
    [38;2;255;165;0mout[0m = two
temp = {a,[34m1[0m,b,}
a = triple_add(temp)
e = c + d
e = e + a
x = [38;2;255;165;0m?[0m
e = x + e
[35mif[0m [34m1[0m
    [38;2;255;165;0mout[0m = e
[38;2;255;165;0mout[0m = [34m1[0m
"#;
        assert_eq!(result, expected);
    }
}
