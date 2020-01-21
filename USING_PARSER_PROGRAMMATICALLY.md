## Using Reason Parser Programmatically

This document describes how to integrate the Reason parser into other custom toolchains that need to get an AST tree of Reason source code.

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
