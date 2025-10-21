# Issue Test Fix Progress

## Summary
- **Original failing tests**: 41
- **Current failing tests**: 38  
- **Tests fixed**: 3
- **Tests broken**: 0

## Fixed Tests

### issue-1728: Negative numbers in tuple
- **Problem**: Trailing semicolon was being added to simple tuple expressions
- **Fix**: Modified `structure` method to use `Layout.Sep` instead of `Layout.SepFinal` for single-item structures containing simple value expressions
- **Location**: `src/reason-parser/reason_pprint_ast.ml:9196-9226`

### issue-1809: make(module I1)
- **Problem**: Trailing semicolon was being added to simple function calls
- **Fix**: Same as issue-1728 - simple function applications now don't get trailing semicolons

### issue-1835: JSX closing tag
- **Problem**: Trailing semicolon was being added to JSX elements
- **Fix**: Same as issue-1728 - JSX elements (detected via JSX attribute) now don't get trailing semicolons

## Remaining Issues (38 tests)

### Parser Errors (Need Parser Fixes, Not Just Printer)
- issue-1100: Multi-line ppx strings cause parse error
- issue-1437: Option type syntax parsing issue
- issue-52: Nested OR patterns parsing issue  
- issue-664: Reserved keyword 'class'
- issue-2251: Annotations inside function definitions

### Complex Formatting Issues

#### Comment Handling
- issue-920: Comment reordering in switch statements
- issue-773: Comment interleaving with JS object sugar
- issue-1781: Comment indentation at end of blocks
- issue-2029: Comment interleaving before JSX
- issue-2065: Comments moving inside list literals

#### Function/Expression Formatting
- issue-1619: Unnecessary breaks on multi-line functions with type annotations
- issue-1711: Too many parens for lambda expressions
- issue-1818: Bad formatting for callbacks
- issue-1721: JSX with function expressions
- issue-1987: Ternary operator formatting
- issue-2002: Function parameters wrapping with labeled/optional params

#### Type/Signature Formatting
- issue-1673: Functor type signature formatting
- issue-1696: External declaration formatting with comments
- issue-2562: Js.t obj type alignment in externals
- issue-2398: open statement formatting

#### Record/Object Formatting
- issue-2843: Record indentation and layout
- issue-2318: Comment influences line width calculation

#### String/Attribute Formatting
- issue-412: String wrapping with backslash escape (not implemented)
- issue-1792: Attribute formatting idempotency with infix expressions

#### Identifier/Keyword Issues
- issue-1751: Record field name remapping for reserved keywords (pub -> pub_ expected, \#pub produced)

#### Other Complex Issues
- issue-2334: Unhygienic expansion with underscore
- issue-2339: Pattern matching indentation
- issue-2555: Extension point indentation  
- issue-2573: Function application in JSX
- issue-2594: Include statement formatting
- issue-2653: JSX with operators (parsing issue)
- issue-2675: JSX indentation with attributes
- issue-1815: Docblock comment with parens
- issue-1923: Ocamldoc comment formatting
- issue-416: Doc comments in OCaml conversion
- issue-2445: Invalid code from OCaml interface conversion
- issue-2449: Extension point errors
- issue-2492: Underscore application with infix operators

## Key Changes Made

### File: `src/reason-parser/reason_pprint_ast.ml`

Added logic to determine when single-item structures should have trailing semicolons:

```ocaml
let sep =
  match structureItems with
  | [ { pstr_desc = Pstr_eval ({ pexp_desc; pexp_attributes; _ }, attrs); _ } ] ->
    let { Reason_attributes.jsxAttrs; _ } =
      Reason_attributes.partitionAttributes pexp_attributes
    in
    let is_simple_value =
      attrs = [] && 
      (match pexp_desc with
      (* Only very simple literal expressions *)
      | Pexp_tuple _ | Pexp_construct _ | Pexp_ident _
      | Pexp_constant _ when pexp_attributes = [] ->
        true
      (* Simple function applications - exclude operators *)
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _)
        when pexp_attributes = [] ->
        not (String.contains name '|' || String.contains name '#' 
             || String.contains name '>' || String.contains name '<'
             || String.contains name '=' || String.contains name '+'
             || String.contains name '-' || String.contains name '*'
             || String.contains name '/' || String.contains name '@')
      (* JSX elements *)
      | Pexp_apply _ when jsxAttrs != [] ->
        true
      | _ -> false)
    in
    if is_simple_value then Layout.Sep ";" else Layout.SepFinal (";", ";")
  | _ -> Layout.SepFinal (";", ";")
in
```

## Next Steps

The remaining 38 issues are significantly more complex and would require:

1. **Parser fixes** for syntax errors (5+ tests)
2. **Comment interleaving logic** rewrite (5+ tests)
3. **String wrapping implementation** (issue-412)
4. **Attribute formatting fixes** for idempotency (issue-1792)
5. **Line breaking heuristics** for various constructs (10+ tests)
6. **Identifier remapping** fixes (issue-1751)
7. **Extension point handling** improvements (multiple tests)

Each category would require several hours of investigation and implementation.

