# Core Reason

Interested in contributing to Reason? The core of it is a parser + a printer, plus other miscellaneous utilities we expose.

**Note** that contributing to a parser or printer is, in general, not that trivial; we'd be as glad seeing you contribute to the Reason ecosystem than Reason core! That being said, there's a [very good section](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html) in Real World Ocaml on parser and printer, if you do want to check out the codebase. We're currently actively iterating on the codebase, so ping us on our [Discord channel](discord.gg/reasonml)!

## Brief Description of Files

(Also, see https://github.com/facebook/reason/wiki/Improve-error-message-locations)

- `ocamlmerlin_reason.ml`: produces the `ocamlmerlin-reason` binary, used in conjunction with [Merlin-extend](https://github.com/let-def/merlin-extend). This is an extension to [Merlin](https://github.com/ocaml/merlin), which picks up this binary from your environment to analyze Reason files when your editor calls Merlin.

- `*.mllib`: related: see the [OCaml extensions list](https://github.com/facebook/reason/wiki/OCaml-Ecosystem-Extensions-List). These are generated file from `pkg/build.ml`, which describes the package we distribute. No need to worry about them.

- `reason-parser/src/reason_config.ml`: global config that says whether our parser should run in "recoverable" mode. Merlin has a neat feature which lets it continue diagnosing e.g. type errors even when the file is syntactically invalid (at the expense of the accuracy of those type error reports' quality). Searching `reason_config` in our codebase will show you how this is used.

- `reason_format_type.ml`, `reason_type_of_ocaml_type.ml`: again, see `pkg/build.ml`. These produce the `refmttype` binary, used by [BetterErrors](refmttype) to output compiler errors in Reason syntax rather than the OCaml one.

- `reason-parser/src/reason_lexer.mll`, `reason_parser.mly`: the tokenizer and the parser! See the first link on Real World OCaml book section. This is used by [Menhir](http://gallium.inria.fr/~fpottier/menhir/), the parser generator.

- `reason-parser/src/reason_oprint.ml`: the "outcome printer" used by Merlin. No need to worry about it for now.

- `reason-parser/src/reason_parser.messages`: auto-generated from parser changes. Menhir generates parsing code that assigns each syntax error to a code, and lets us customize these errors. Syntax errors can be very precisely pinpointed and explained this way.

- `reason-parser/src/reason_pprint_ast.ml`: the pretty-printer! This takes in the AST (abstract syntax tree) and prints out the textual code. Theoretically for us, `print (parse myCode) == myCode`.

- `reason-parser/src/reason_toolchain.ml`, `refmt_impl.ml`: the entry point that calls the parsing logic.

- `reason_utop.ml`, `reason_toploop.ml`, `rtop_init.ml`: Reason's [Utop](https://github.com/diml/utop) integration. Utop's the terminal-based REPL you see when executing `utop` (in Reason's case, the wrapper `rtop`).

- `reasonbuild.ml`: our wrapper for [OCamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/), a simple build system.

- `*.sh`: some of our binaries' entries.

- `reason_util.ml`, `syntax_util.ml`: utils.

- `reactjs_jsx_ppx.ml`, `reactjs_jsx_ppx_2.ml`: our ReactJS interop that translates [Reason JSX](http://facebook.github.io/reason/index.html#diving-deeper-jsx) into something that ReactJS understands. See the comments in the file and the description in [ReasonReact](https://reasonml.github.io/reason-react/#reason-react-jsx).

## Working with PPX

reactjs_jsx_ppx uses the ppx system. It works on the AST. It helps being able to see the AST of a particular snippet. Assuming you've written some code in a file `foo.re`, run the following incantation to output the code's AST:

```
ocamlc -dparsetree -ppx ../reactjs_jsx_ppx.native -pp "../refmt_impl.native --print binary" -impl foo.re
```

That dumps the AST after accepting the ppx and the reason syntax. You can also dump the final code in Reason syntax instead:

```
ocamlc -dsource -ppx ../reactjs_jsx_ppx.native -pp "../refmt_impl.native --print binary" -impl foo.re | ../refmt_impl.native --parse ml --print re --interface false
```
