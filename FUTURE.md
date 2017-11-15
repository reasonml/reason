## 3.0.1

- Polymorphic variants can now parse and print \`foo(()) as \`foo() (#1560)
- Remove few places remaining that accidentally print `fun` for functions (#1588)
- Variant values with annotations like `Some((x: string))` can now be `Some(x: string)` (#1576)
- Better record & object printing (#1593, #1596)
- Fewer unnecessary wrappings in type declarations (#1616)
- Parse and print attributes on object type rows (#1637)
- Better printing for multiple type equations in a module type in a function arg (#1641)
- Better printing for unary -. in labelled argument (#1642)
