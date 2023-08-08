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
    let mut lines = std::io::BufReader::new(text).lines().map(Result::unwrap).filter(|l|!l.is_empty());
    let mut tree = SyntaxTree::default();
    let mut cursor = tree.cursor_mut();
    let mut last_indent = 0;

    if let Some(line) = lines.next() {
        let bytes = line.as_bytes();
        let indent = get_indent(&bytes);

        let string = std::str::from_utf8(&bytes[indent..]).unwrap();
        let statement = parser::statement(string).expect(&format!("{line}"));

        cursor.insert_next(statement);

        for line in lines {
            let bytes = line.as_bytes();
            let indent = get_indent(&bytes);

            let string = std::str::from_utf8(&bytes[indent..]).unwrap();
            // println!("string: {string}");
            let statement = parser::statement(string).expect(&format!("{line}"));

            match indent.cmp(&last_indent) {
                Ordering::Greater => {
                    // Increases in indentation can only follow `if` `loop` or function definition statements.
                    match cursor.current() {
                        Some(Statement(
                            StatementType::If(_)
                            | StatementType::Loop(_)
                            | StatementType::Function(_),
                        )) => {
                            cursor.insert_child(statement);
                            cursor.move_child();
                        }
                        _ => panic!("bad indentation"),
                    }
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
            / c:call() { StatementType::Call(c) }
            / a:assignment() { StatementType::Assign(a) }
            / f:function() { StatementType::Function(f) }
            / b:break() { StatementType::Break(b) }

    rule if_rule() -> If = "if" _ cond:expression() { If { cond } }

    rule loop() -> Loop = "loop" { Loop }

    rule break() -> Break = "break" { Break }

    rule assignment() -> Assign
        = ident:identifier() _ "=" _ expr:expression() { Assign { ident, expr } }

    rule expression() -> Expression
            = a:array() { Expression::Array(a) }
            / i:index() { Expression::Index(i) }
            / c:call() { Expression::Call(c) }
            / u:unknown() { Expression::Unknown(u) }
            / i:identifier() { Expression::Ident(i) }
            / l:literal() { Expression::Literal(l) }

    rule unknown() -> Unknown = "?" { Unknown }
    rule call() -> Call
        = rt:("rt" _)? ident:identifier() ":" input:(expression())? { Call { rt: rt.is_some(), ident, input: input.map(Box::new) } }
    rule array() -> Array
        = "{" args:((v:expression() "," {v})*) "}" { Array(args) }
    rule index() -> Index
        = ident:identifier() "[" index:expression() "]" { Index { ident, index: Box::new(index) } }

    rule function() -> Function
        = "def" _ ident:identifier() { Function(ident) }

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

    // # Two-sum <T, N>
    // - Read a T from stdin (t)
    // - Read a N from stdin (n)
    // - Read n i8s from stdin (s)
    // If 2 i8s in s are found that sum to t exit with 0, else exit with 1.

    // Two-sum <i8, u8>
    #[test]
    fn front_2sumi8u8() {
        let text = r#"target = rt read:{0,1,}
length = rt read:{0,1,}
input = rt read:{0,length,}
set = ?
i = 0
loop
    if eq:{i,length,}
        break
    difference = sub:{target,index:{set,i,},}
    if index:{set,difference,}
        exit:0
    i = add:{i,1,}
exit:1
"#;
        let tree = statements(text.as_bytes());

        let result = tree.to_string();
        println!("{result}");
    }
    
    // Two-sum <i8, u16>
    #[test]
    fn two_sum_i8u64() {
        let text = r#"
target = read:{0,1,}
length = read:{0,1,}
fd = memfd_create:
ptr = mmap:{fd,length,}
read:{0,length,ptr,}
loop
    if 
"#;
    }
    // Two-sum <i32, u64>
    #[test]
    fn front_calculator() {
        let text = r#"
"#;

        let tree = statements(text.as_bytes());

        let result = tree.to_string();
        println!("{result}");

        // use std::io::Write;
        // let mut file = std::fs::OpenOptions::new()
        //     .create(true)
        //     .truncate(true)
        //     .write(true)
        //     .open("out.txt")
        //     .unwrap(); 
        // file.write_all(result.as_bytes()).unwrap();

        let expected = r#"lhs_ascii = [35mrt [0m[33m[32mread[0m[33m[0m:{[34m0[0m,[34m4[0m,}
lhs = [33m[32msub[0m[33m[0m:{x,{[34m48[0m,[34m48[0m,[34m48[0m,[34m48[0m,},}
op = [35mrt [0m[33m[32mread[0m[33m[0m:{[34m0[0m,[34m1[0m,}
rhs_ascii = [35mrt [0m[33m[32mread[0m[33m[0m:{[34m0[0m,[34m4[0m,}
rhs = [33m[32msub[0m[33m[0m:{x,{[34m48[0m,[34m48[0m,[34m48[0m,[34m48[0m,},}
[35mif[0m [33meq[0m:{op,[34m0[0m,}
    res = [33m[32madd[0m[33m[0m:{lhs,rhs,}
    [33m[32mwrite[0m[33m[0m:{[34m1[0m,res,}
    [33m[32mexit[0m[33m[0m:[34m0[0m
[35mif[0m [33meq[0m:{op,[34m1[0m,}
    res = [33m[32msub[0m[33m[0m:{lhs,rhs,}
    [33m[32mwrite[0m[33m[0m:{[34m1[0m,res,}
    [33m[32mexit[0m[33m[0m:[34m0[0m
[35mif[0m [33meq[0m:{op,[34m2[0m,}
    res = [33m[32mmul[0m[33m[0m:{lhs,rhs,}
    [33m[32mwrite[0m[33m[0m:{[34m1[0m,res,}
    [33m[32mexit[0m[33m[0m:[34m0[0m
[35mif[0m [33meq[0m:{op,[34m3[0m,}
    res = [33m[32mdiv[0m[33m[0m:{lhs,rhs,}
    [33m[32mwrite[0m[33m[0m:{[34m1[0m,res,}
    [33m[32mexit[0m[33m[0m:[34m0[0m
[33m[32mexit[0m[33m[0m:[34m1[0m
"#;
        assert_eq!(result, expected);

    }

    #[test]
    fn front() {
        let text = r#"x = read:{0,4,}
y = 2
y = x
a = 2
b = 3
c = add:{a,b,}
d = read:{0,4,}
if 1
    c = add:{d,c,}
    d = 4
a = d
loop
    a = add:{a,c,}
    if 1
        break
def triple_add
    one = add:{in[0],in[1],}
    two = add:{one,in[2],}
    if 0
        two = triple_add:{two,two,two,}
    out = two
a = triple_add:{a,1,b,}
e = add:{c,d,}
e = add:{e,a,}
x = read:{0,4,}
e = add:{x,e,}
if 1
    exit:e
exit:1
"#;

        let tree = statements(text.as_bytes());

        let result = tree.to_string();
        println!("{result}");

        // use std::io::Write;
        // let mut file = std::fs::OpenOptions::new()
        //     .create(true)
        //     .truncate(true)
        //     .write(true)
        //     .open("out.txt")
        //     .unwrap(); 
        // file.write_all(result.as_bytes()).unwrap();

        // TODO Can we make this expected string look less bad?
        let expected = r#"x = [33m[32mread[0m[33m[0m:{[34m0[0m,[34m4[0m,}
y = [34m2[0m
y = x
a = [34m2[0m
b = [34m3[0m
c = [33m[32madd[0m[33m[0m:{a,b,}
d = [33m[32mread[0m[33m[0m:{[34m0[0m,[34m4[0m,}
[35mif[0m [34m1[0m
    c = [33m[32madd[0m[33m[0m:{d,c,}
    d = [34m4[0m
a = d
[35mloop[0m
    a = [33m[32madd[0m[33m[0m:{a,c,}
    [35mif[0m [34m1[0m
        [35mbreak[0m
[35mdef[0m [33mtriple_add[0m
    one = [33m[32madd[0m[33m[0m:{[38;2;255;165;0min[0m[[34m0[0m],[38;2;255;165;0min[0m[[34m1[0m],}
    two = [33m[32madd[0m[33m[0m:{one,[38;2;255;165;0min[0m[[34m2[0m],}
    [35mif[0m [34m0[0m
        two = [33mtriple_add[0m:{two,two,two,}
    [38;2;255;165;0mout[0m = two
a = [33mtriple_add[0m:{a,[34m1[0m,b,}
e = [33m[32madd[0m[33m[0m:{c,d,}
e = [33m[32madd[0m[33m[0m:{e,a,}
x = [33m[32mread[0m[33m[0m:{[34m0[0m,[34m4[0m,}
e = [33m[32madd[0m[33m[0m:{x,e,}
[35mif[0m [34m1[0m
    [33m[32mexit[0m[33m[0m:e
[33m[32mexit[0m[33m[0m:[34m1[0m
"#;
        assert_eq!(result, expected);
    }
}
