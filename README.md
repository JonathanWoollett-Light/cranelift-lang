# cranelift-lang

Goal: As much compile time analysis as possible.

E.g.

`cargo test -- --nocapture`:
```
a = 2 [2]
b = 3 [3]
c = a + b [5]
d = 3 [3]
if a == 2 [true]
        d = 4 [4]
loop
        a = a + c [7,12,17]
        if a > 15 [false,false,true]
                break

def triple_add(a,b,c)
        one = a + b [18,42]
        two = one + c [21,63]
        if two < 30 [true,false]
                two = triple_add(two,two,two) [63]
        return two

a = triple_add(a,1,b) [63]
e = c + d [9]
e = e + a [72]
```
