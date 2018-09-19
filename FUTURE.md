## 3.3.4 (unreleased)

Fixes:

- Fast pipe braces (#2133, 2148).
- Better rtop `use` directives (#2146, 2147).
- `foo(~Foo.a)` becoming `foo(~Fooa=Foo.a)` (#2136).
- Parse `<div> ...c</div>` correctly (#2137).
- Invalid formatting of first-class module with type constraint (#2151).
- Precedence printing of fast pipe with underscore sugar as JSX child (#2159).
- Correct location for JSX name & extension expression (#2166, #2162).
- Lack of space after `module type of` (#2175).
- Outcome printer (editor & interface generator) function signature (#2185).

Improvements:

- Preserve empty lines in records and bs objects (#2152).
