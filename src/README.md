# Core Reason

![reason_-_bucklescript_in_ocaml](https://user-images.githubusercontent.com/1909539/31158768-0c7e9d04-a879-11e7-9cfb-19780a599231.png)

(_Click to see a larger version_)

Interested in contributing to Reason? The core of it is a parser + a printer, plus other miscellaneous utilities we expose.

**Note** that contributing to a parser or printer is, in general, not that trivial; we'd be as glad seeing you contribute to the Reason ecosystem than Reason core! That being said, there's a [very good section](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html) in Real World Ocaml on parser and printer, if you do want to check out the codebase. We're currently actively iterating on the codebase, so ping us on our [Discord channel](discord.gg/reasonml)!

## Brief Description of Files

Throughout the codebase, you might see mentions of "migrate-parsetree", `Ast_404`, etc. These refer to https://github.com/let-def/ocaml-migrate-parsetree. It's a library that allows you to convert between different versions of the OCaml AST. This way, the Reason repo can be written in OCaml 4.04's AST data structures, while being usable on OCaml 4.02's libraries (BuckleScript's on 4.02 too).

Our lexer & parser use [Menhir](http://gallium.inria.fr/~fpottier/menhir/), the parser generator. Again, more info [here](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)

### Core Files

- `src/reason_lexer.mll`: the lexer that chunks a raw string into tokens. See the file for more comments.

- `reason_parser.mly`: the parser that takes the lexer's result and turns it into a proper AST (abstract syntax tree). See the file for more comments.

- `src/reason_pprint_ast.ml`: the pretty-printer! This is the reverse of parsing: it takes in the AST (abstract syntax tree) and prints out the nicely formatted code text.

`src/reason_parser.messages`: this is the huge table of mostly generated, sometimes hand-written, syntax error messages. When the parser ends up at an invalid parsing state (aka ends up with a syntax error), it'd refer to that file's content and see if that case has a specific error message assigned to it. For an example fix, see [this PR](https://github.com/facebook/reason/pull/1018) and the [follow-up](https://github.com/facebook/reason/pull/1033). To add a syntax error message see [the wiki page](https://github.com/facebook/reason/wiki/Add-a-Menhir-error-message).

- `src/reason_oprint.ml`: the "outcome printer" used by Merlin, rtop and terminal, that prints the errors in Reason syntax. More info in the file itself.

### Miscellaneous Files

- `ocamlmerlin_reason.ml`: produces the `ocamlmerlin-reason` binary, used in conjunction with [Merlin-extend](https://github.com/let-def/merlin-extend). This is an extension to [Merlin](https://github.com/ocaml/merlin), which picks up this binary from your environment to analyze Reason files when your editor calls Merlin.

- `*.mllib`: related: see the [OCaml extensions list](https://github.com/facebook/reason/wiki/OCaml-Ecosystem-Extensions-List). These are generated file from `pkg/build.ml`, which describes the package we distribute. No need to worry about them.

- `src/reason_config.ml`: global config that says whether our parser should run in "recoverable" mode. Merlin has a neat feature which lets it continue diagnosing e.g. type errors even when the file is syntactically invalid (at the expense of the accuracy of those type error reports' quality). Searching `reason_config` in our codebase will show you how this is used.

- `reason_format_type.ml`, `reason_type_of_ocaml_type.ml`: again, see `pkg/build.ml`. These produce the `refmttype` binary, used by [BetterErrors](refmttype) to output compiler errors in Reason syntax rather than the OCaml one.

- `src/reason_parser.messages`: auto-generated from parser changes. Menhir generates parsing code that assigns each syntax error to a code, and lets us customize these errors. Syntax errors can be very precisely pinpointed and explained this way.

- `src/reason_toolchain.ml`, `refmt_impl.ml`: the entry point that calls the parsing logic.

- `reason_utop.ml`, `reason_toploop.ml`, `rtop_init.ml`: Reason's [Utop](https://github.com/diml/utop) integration. Utop's the terminal-based REPL you see when executing `utop` (in Reason's case, the wrapper `rtop`).

- `reasonbuild.ml`: our wrapper for [OCamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/), a simple build system.

- `*.sh`: some of our binaries' entries.

- `reason_util.ml`, `syntax_util.ml`: utils.

- `reactjs_jsx_ppx_v2.ml/v3.ml`: our ReactJS interop that translates [Reason JSX](https://reasonml.github.io/guide/language/jsx) into something that ReactJS understands. See the comments in the file and the description in [ReasonReact](https://reasonml.github.io/reason-react/#reason-react-jsx).

## Working With Parser

Here's a recommended workflow:

- First put your code in the current master syntax in a file
- `make build`
- `./refmt_impl.native --print ast thatFile.re`
- look closely at the ast, spot the thing you need
- Search your item in `reason_parser.mly`
- Change the logic
- `make test`

Lexer helpers doc: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html
Parser helper docs: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Parsetree.html
Menhir manual: http://gallium.inria.fr/~fpottier/menhir/manual.pdf
Small Menhir example: https://github.com/derdon/menhir-example
Random Stack Overflow answer: https://stackoverflow.com/questions/9897358/ocaml-menhir-compiling-writing
(Ok seriously, we need some more Menhir examples. But hey, nobody said it was easy... for now!)

## Working With PPX

reactjs_jsx_ppx_v2/v3 uses the ppx system. It works on the AST. It helps being able to see the AST of a particular snippet. Assuming you've written some code in a file `foo.re`, run the following incantation to output the code's AST:

```
ocamlc -dparsetree -ppx ../reactjs_jsx_ppx_v2.native -pp "../refmt_impl.native --print binary" -impl foo.re
```

That dumps the AST after accepting the ppx and the reason syntax. You can also dump the final code in Reason syntax instead:

```
ocamlc -dsource -ppx ../reactjs_jsx_ppx_v2.native -pp "../refmt_impl.native --print binary" -impl foo.re | ../refmt_impl.native --parse ml --print re --interface false
```

(Similar steps for reactjs_jsx_ppx_v3.)
