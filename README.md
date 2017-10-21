# Reason

Simple, fast & type safe code that leverages the JavaScript & OCaml ecosystems

[![Build Status](https://travis-ci.org/facebook/reason.svg?branch=master)](https://travis-ci.org/facebook/reason) [![CircleCI](https://circleci.com/gh/facebook/reason/tree/master.svg?style=svg)](https://circleci.com/gh/facebook/reason/tree/master)

## [Getting Started](https://reasonml.github.io/guide/javascript/quickstart)

## [Community](https://reasonml.github.io/community/)

## Contributing

### Documentations

Go to https://github.com/reasonml/reasonml.github.io to contribute to the Reason documentation.

### Codebase

See the [src folder's README](https://github.com/facebook/reason/tree/master/src/README.md).

## Installation for Programmatic Usage

**If you're not using Reason programmatically**, disregard this section and see the Getting Started guide above. This is for using Reason's `refmt` as a third-party library.

### JavaScript API

We expose a `refmt.js` for you to use on the web. Again, for local development, please use the native `refmt` that comes with the installation [here](https://reasonml.github.io/guide/editor-tools/global-installation). It's an order of magnitude faster than the JS version. Don't use the JS version unless you know what you're doing. Let's keep our ecosystem fast please.

Aaaanyways, to install `refmt.js`: `npm install reason`. Here's the API:

- `parseRE(codeString)`: returns an AST + comments info that you'd use for printing
- `parseREI(codeString)`: same, but for interface
- `printRE(astAndComments)`: receives the data structure returned by `parseRE` and others
- `printREI(astAndComments)`: same, but for interface
- `parseML(codeString)`, `parseMLI(codeString)`, `printML(astAndComments)`, `printMLI(astAndComments)`: the OCaml syntax counterparts.

The `parse*` functions potentially throw an error of this shape:

```js
{
  message: string,
  location: {
    startLine: number,
    startLineStartChar: number,
    startLineStartChar: number,
    endLineEndChar: number,
  }
}
```

`location` can be `undefined`.

Example usage:

```js
const refmt = require('refmt')
// convert the ocaml syntax to reason syntax
console.log(refmt.printRE(refmt.parseML('let f a = 1')))
```

### Native API

We're spoiled with more APIs on the native side. To use Reason from OPAM as a native library, you have [these functions](https://github.com/facebook/reason/blob/5a253048e8077c4597a8935adbed7aa22bfff647/src/reason_toolchain.ml#L141-L157). So:

- `Reason_toolchain.JS.canonical_implementation_with_comments`
- `Reason_toolchain.JS.canonical_interface_with_comments`
- `Reason_toolchain.JS.print_canonical_interface_with_comments`
- `Reason_toolchain.ML.canonical_implementation_with_comments`
- etc.

`JS` means "Reason syntax". We're basically ES2050.

The `JS` parsing functions might throw [this](https://github.com/facebook/reason/blob/5a253048e8077c4597a8935adbed7aa22bfff647/src/syntax_util.ml#L301) (docs on `Location.t` [here](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Location.html)). The `ML` parsing functions might throw [`Syntaxerr.Error error`](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Syntaxerr.html).

Example usage:

```ocaml
let ast_and_comments =
  Lexing.from_string "let f a => 1"
  |> Reason_toolchain.JS.canonical_implementation_with_comments

(* Convert Reason back to OCaml syntax. That'll show these Reason users! *)
let ocaml_syntax =
  Reason_toolchain.ML.print_canonical_implementation_with_comments
    Format.str_formatter
    ast_and_comments;
  Format.flush_str_formatter ()
```

## License

See Reason license in [LICENSE.txt](LICENSE.txt).

Works that are forked from other projects are under their original licenses.

## Credit

The general structure of `refmt` repo was copied from @whitequark's m17n project, including parts of the `README` that instruct how to use this with the OPAM toolchain. Thank you OCaml!
