# Reason

Simple, fast & type safe code that leverages the JavaScript & OCaml ecosystems.

[![Build Status](https://dev.azure.com/reasonml/reason/_apis/build/status/facebook.reason?branchName=master)](https://dev.azure.com/reasonml/reason/_build/latest?definitionId=2?branchName=master) [![Build Status](https://travis-ci.org/facebook/reason.svg?branch=master)](https://travis-ci.org/facebook/reason) [![CircleCI](https://circleci.com/gh/facebook/reason/tree/master.svg?style=svg)](https://circleci.com/gh/facebook/reason/tree/master) [![Chat](https://img.shields.io/discord/235176658175262720.svg?logo=discord&colorb=blue)](https://discord.gg/reasonml)

## [Getting Started](https://reasonml.github.io/docs/en/installation)

## [Community](https://reasonml.github.io/docs/en/community.html)

## [Roadmap & Contribution](https://reasonml.github.io/docs/en/roadmap)

### Documentations

Go to https://github.com/reasonml/reasonml.github.io to contribute to the Reason documentation.

### Codebase

See the [src folder's README](https://github.com/facebook/reason/tree/master/src/README.md).

## Programmatically Using Reason Parser From JavaScript:

**If you're not using Reason programmatically**, disregard this section and see the Getting Started guide above. This is for using Reason's `refmt` as a third-party library.

### JavaScript API

We expose a `refmt.js` for you to use on the web. Again, for local development, please use the native `refmt` that comes with the installation [here](https://reasonml.github.io/docs/en/installation.html). It's an order of magnitude faster than the JS version. Don't use the JS version unless you know what you're doing. Let's keep our ecosystem fast please.

Aaaanyways, to install `refmt.js`: `npm install reason`.

Here's the API, with pseudo type annotations:

- `parseRE(code: string): astAndComments`: parse Reason code
- `parseREI(code: string): astAndComments`: parse Reason interface code
- `printRE(data: astAndComments): string`: print Reason code
- `printREI(data: astAndComments): string`: print Reason interface code
- `parseML(code)`, `parseMLI(code)`, `printML(data)`, `printMLI(data)`: same as above, but for the OCaml syntax

The type `string` is self-descriptive. The type `astAndComments` returned by the `parse*` functions is an opaque data structure; you will only use it as input to the `print*` functions. For example:

```js
const refmt = require('reason');

// convert the ocaml syntax to reason syntax
const ast = refmt.parseML('let f a = 1');
const result = refmt.printRE(ast);
console.log(result); // prints `let f = (a) => 1`
```

The `parse*` functions potentially throw an error of this shape:

```js
{
  message: string,
  // location can be undefined
  location: {
    // all 1-indexed
    startLine: number, // inclusive
    startLineStartChar: number, // inclusive
    endLine: number, // inclusive
    endLineEndChar: number, // **exclusive**
  }
}
```

**NOTE**: `refmt.js` requires the node module `fs`, which of course isn't available on the web. If using webpack, to avoid the missing module error, put `node: { fs: 'empty' }` into `webpack.config.js`. See https://webpack.js.org/configuration/node/#other-node-core-libraries for more information.

`refmt.js` is minified for you through Closure Compiler, with an accompanying `refmt.map`. The size is 2.3MB **but don't get fooled; it gzips down to just 345KB**. This way, you can carry it around in your own blog and use it to create an interactive refmt playground, without worrying about imposing bandwidth overhead to your readers. Again, keep our ecosystem fast and lean.

### Native API

We're spoiled with more APIs on the native side. To use Reason from OPAM as a native library, you have [these functions](https://github.com/facebook/reason/blob/5a253048e8077c4597a8935adbed7aa22bfff647/src/reason_toolchain.ml#L141-L157). So:

- `Reason_toolchain.RE.implementation_with_comments`
- `Reason_toolchain.RE.interface_with_comments`
- `Reason_toolchain.RE.print_interface_with_comments`
- `Reason_toolchain.ML.implementation_with_comments`
- etc.

The `ML` parsing functions might throw [`Syntaxerr.Error error`](https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Syntaxerr.html). The `RE` parsing functions might throw:

- [`Reason_syntax_util.Error`](https://github.com/facebook/reason/blob/6e99ea5aae3791359b1e356060691f7b5b596365/src/reason-parser/reason_syntax_util.ml#L456) (docs on `Location.t` [here](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Location.html))
- [`Syntaxerr.Error`](https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Syntaxerr.html).
- [`Reason_lexer.Error`](https://github.com/facebook/reason/blob/6e99ea5aae3791359b1e356060691f7b5b596365/src/reason-parser/reason_lexer.mll#L84).

Example usage:

```ocaml
let ast_and_comments =
  Lexing.from_string "let f a => 1"
  |> Reason_toolchain.RE.implementation_with_comments

(* Convert Reason back to OCaml syntax. That'll show these Reason users! *)
let ocaml_syntax =
  Reason_toolchain.ML.print_implementation_with_comments
    Format.str_formatter
    ast_and_comments;
  Format.flush_str_formatter ()
```

## License

See Reason license in [LICENSE.txt](LICENSE.txt).

Works that are forked from other projects are under their original licenses.

## Credit

The general structure of `refmt` repo was copied from [whitequark's m17n project](https://github.com/whitequark/ocaml-m17n), including parts of the `README` that instruct how to use this with the OPAM toolchain. Thank you OCaml!
