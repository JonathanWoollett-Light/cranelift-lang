# cranelift-lang

Goal: As much compile time analysis as possible.

### AST

The goal is to have a very simple language.

A visualization I use for illustrate the abstract syntax tree:

![Graph showing abstract syntax tree](./ast.png)

### Example

`cargo test -- --nocapture` with outputs

![Example output code with compile time highlights](./example-output.png)

See [`example-input`](./example-input.txt).
