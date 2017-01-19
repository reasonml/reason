# Core Reason

Interested in contributing to Reason? The core of it is a parser + a printer, plus other miscellaneous utilities we expose.

**Note** that contributing to a parser or printer is, in general, not that trivial. We're as glad seeing you contribute to the Reason ecosystem than Reason core! That being said, there's a [very good section](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html) in Real World Ocaml on parser and printer, if you do want to check out the codebase. We're currently actively iterating on the codebase, so ping us on our [Discord channel](discord.gg/reasonml)!

## Brief Description of Files

- `ocamlmerlin_reason.ml`: produces the `ocamlmerlin-reason` binary, used in conjunction with [Merlin-extend](https://github.com/let-def/merlin-extend). This is an extension to [Merlin](https://github.com/ocaml/merlin), which picks up this binary from your environment to analyze Reason files when your editor calls Merlin (we have starter projects such as [ReasonProject](https://reasonml.github.io/ReasonProject) that takes care of putting the binary in your `PATH` correctly).

- `*.mllib`: related: see the [OCaml extensions list](https://github.com/facebook/reason/wiki/OCaml-Ecosystem-Extensions-List). These are generated file from `pkg/build.ml`, which describes our package. No need to worry about it.

- `reason_config.ml`: tiny file that says whether our parser should run in "recoverable" mode. Merlin has a neat feature which lets it continue diagnosing e.g. type errors even when the file is syntactically invalid (at the expense of the accuracy of those type error reports' quality). Searching `reason_config` in our codebase will show you where this is used.

- `reason_format_type.ml`, `reason_type_of_ocaml_type.ml`: again, see `pkg/build.ml`. This produces the `refmttype` binary, used by [BetterErrors](refmttype) to output the errors in Reason syntax rather than OCaml syntax.

- `reason_lexer.mll`, `reason_parser.mly`: finally, the tokenizer and the parser! See the first link on Real World OCaml book section. This is used by Menhir, the parser generator.

- `reason_oprint.ml`: the "outcome printer" used by Merlin. No need to worry about it for now.

- `reason_parser.messages`: auto-generated from parser changes. Menhir generates parsing code that assigns each syntax error to a code, and lets us customize these errors. Syntax errors can be very precisely pinpointed and explained this way.

- `reason_pprint_ast.ml`: the pretty-printer! This takes in the AST (abstract syntax tree) and prints it into the textual code. Theoretically for us, `print (parse myCode) == myCode`.

- `reason_toolchain.ml`: the entry point into the parsing logic, used by e.g. `refmt_impl`.

- `reason_utop.ml`, `reason_toploop.ml`, `rtop_init.ml`: Reason's [Utop](https://github.com/diml/utop) integration. Utop's the terminal-based REPL you see when executing `utop` (in Reason's case, the wrapper `rtop`).

- `reasonbuild.ml`: our wrapper for [OCamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/), a simple build system.

- `*.sh`: some of our binaries' entries.

- `reason_util.ml`, `syntax_util.ml`: utils.

- `reactjs_jsx_ppx.ml`: TODO.
