# cranelift-lang

Goal: As much compile time analysis as possible.

### AST

The goal is to have a very simple language.

A visualization I use for illustrate the abstract syntax tree:

![Graph showing abstract syntax tree](./ast.png)

### Compile-time evaluation

...

### Parsing

```txt
x = ?
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
```

becomes

![parsing-output](./parsing-output.png)