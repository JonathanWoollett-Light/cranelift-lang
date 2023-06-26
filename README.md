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
c = add {a,b,}
d = ?
if 1
    c = add {d,c,}
    d = 4
a = d
loop
    a = add {a,c,}
    if 1
        break
def triple_add
    one = add {in[0],in[1],}
    two = add {one,in[2],}
    if 0
        two = triple_add {two,two,two,}
    out = two
a = triple_add {a,1,b,}
e = add {c,d,}
e = add {e,a,}
x = ?
e = add {x,e,}
if 1
    out = e
out = 1
```

becomes

![parsing-output](./parsing-output.png)