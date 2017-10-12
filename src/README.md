# Core Reason

![reason_-_bucklescript_in_ocaml](https://user-images.githubusercontent.com/1909539/31158768-0c7e9d04-a879-11e7-9cfb-19780a599231.png)

(_Click to see a larger version_)

Interested in contributing to Reason? The core of it is a parser + a printer, plus other miscellaneous utilities we expose.

**Note** that contributing to a parser or printer is, in general, not that trivial; we'd be as glad seeing you contribute to the Reason ecosystem than Reason core! That being said, there's a [very good section](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html) in Real World OCaml on parser and printer, if you do want to check out the codebase. We're currently actively iterating on the codebase, so ping us on our [Discord channel](discord.gg/reasonml)!

## Brief Description of Files

Throughout the codebase, you might see mentions of "migrate-parsetree", `Ast_404`, etc. These refer to https://github.com/let-def/ocaml-migrate-parsetree. It's a library that allows you to convert between different versions of the OCaml AST. This way, the Reason repo can be written in OCaml 4.04's AST data structures, while being usable on OCaml 4.02's libraries (BuckleScript's on 4.02 too).

Our lexer & parser use [Menhir](http://gallium.inria.fr/~fpottier/menhir/), a library that helps us with parsing (it's a "parser generator"). Again, more info [here](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)

### Core Files

- `src/reason_lexer.mll`: the lexer that chunks a raw string into tokens. See the file for more comments.

- `reason_parser.mly`: the parser that takes the lexer's result and turns it into a proper AST (abstract syntax tree). See the file for more comments.

- `src/reason_pprint_ast.ml`: the pretty-printer! This is the reverse of parsing: it takes in the AST (abstract syntax tree) and prints out the nicely formatted code text.

`src/reason_parser.messages`: this is the huge table of mostly generated, sometimes hand-written, syntax error messages. When the parser ends up at an invalid parsing state (aka ends up with a syntax error), it'd refer to that file's content and see if that case has a specific error message assigned to it. For an example fix, see [this PR](https://github.com/facebook/reason/pull/1018) and the [follow-up](https://github.com/facebook/reason/pull/1033). To add a syntax error message see [the wiki page](https://github.com/facebook/reason/wiki/Add-a-Menhir-error-message).

- `src/reason_oprint.ml`: the "outcome printer" used by Merlin, rtop and terminal, that prints the errors in Reason syntax. More info in the file itself.

### Miscellaneous Files

- `ocamlmerlin_reason.ml`: produces the `ocamlmerlin-reason` binary, used in conjunction with [Merlin-extend](https://github.com/let-def/merlin-extend). This is an extension to [Merlin](https://github.com/ocaml/merlin), which picks up this binary from your environment to analyze Reason files when your editor calls Merlin.

- `*.mllib`: related: see the [OCaml extensions list](https://github.com/facebook/reason/wiki/OCaml-Ecosystem-Extensions-List). These are generated file from `pkg/build.ml`, which describes the package we distribute. No need to worry about them.

- `src/reason_config.ml`: global configuration that says whether our parser should run in "recoverable" mode. Merlin has a neat feature which lets it continue diagnosing e.g. type errors even when the file is syntactically invalid (at the expense of the accuracy of those type error reports' quality). Searching `reason_config` in our codebase will show you how this is used.

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

**Want some example pull requests**? Here are a few:

- [Fix outcome printer object printing](https://github.com/facebook/reason/pull/1357)
- [Add more spacing when printing Ptyp_package](https://github.com/facebook/reason/pull/1430)
- [Implement spread for jsx3](https://github.com/facebook/reason/pull/1429)
- [Make deref be a prefix operator](https://github.com/facebook/reason/pull/1463)
- [Print MyConstructor(()) as MyConstructor()](https://github.com/facebook/reason/pull/1465)
- [Ensure valid parsing of constraint expressions after printing](https://github.com/facebook/reason/pull/1464)
- [Record punning for value & pattern for fields with module prefix](https://github.com/facebook/reason/pull/1456)
- [Rage implement everything](https://github.com/facebook/reason/pull/1448)
- [Print functions as javascript](https://github.com/facebook/reason/pull/1469)
- [Transform ocaml pervasives interfaces to reason correctly](https://github.com/facebook/reason/pull/1474)
- [Special case printing of foo(bar)##value](https://github.com/facebook/reason/pull/1481)
- [Use ~ for named args](https://github.com/facebook/reason/pull/1483/)
- [Bring back parentheses-less `switch foo`](https://github.com/facebook/reason/pull/1476)
- [Remove extra parens printed in `type a = Foo((unit => unit))`](https://github.com/facebook/reason/pull/1491)

### Debugging Grammar Conflicts

Run the main parser through Menhir with the `--explain` flag to have it print out details about the conflict. `menhir src/reason_parser.mly --explain`. The debug information can be found at `src/reason_parser.conflicts`.

### Debugging the Parser State at Runtime

If you set the environment variable as follows, the parser state will be printed out as it parses files.

```sh
export OCAMLRUNPARAM='p'
```

### Add a Menhir Error Message

To add a Menhir error message, you first need to know the error code. To find the error code, you can run the following commands from the Reason project root:

```
make
./refmt_impl.native --parse re foo.re
```

Where `foo.re` contains a syntax error. This will result in an error message like:

```
File "test2.re", line 4, characters 2-6:
Error: 2665: <UNKNOWN SYNTAX ERROR>
```

Here, the error code is 2665. We then search for this code in `src/reason_parser.messages`.

- If you find it, you can add a better error message instead of the not so descriptive `<UNKNOWN SYNTAX ERROR>`.

To test the new error message you can run the following commands again:

```
make
./refmt --parse re foo.re
```

Then submit a PR!

- If you can't find the corresponding error code, `make all_errors` generates all the possible error states and corresponding code, from which you can copy the relevant one over and modify the message. More info [here](https://github.com/facebook/reason/pull/1033#issuecomment-276445792)

### Improve Error Message Locations

In some situations, Reason might report errors in a different place than where it occurs. This is caused by the AST not having a correct location for a node. When the error reports a location that's simply at the top of the file, that means we likely didn't attach the location in the parser correctly, altogether.

Before digging into Reason parser, make sure this isn't actually caused by some PPX. Otherwise, run:

```
make
./refmt_impl.native --parse re --print ast test.re
```

Where `test.re` has the code that produces the error message at the wrong location.

In the printed AST, you can see nodes such as `Ptyp_constr "int" (test.re[1,0+15]..[1,0+18])` where the part between parentheses is the location of the node.

The error message's own location will look like `([0,0+-1]..[0,0+-1])` too.

To fix this, we need to find the AST node in `src/reason_parser.mly`. It's a big file, but if you search for the AST node, you should be able to find the location (if not, bug us on Discord). It will probably involve a `mkexp` or `mkpat` without a proper `~loc` attached to it.

As you can see from other parts in the parser, many do have a `~loc` assigned to it. For example

```
| LIDENT jsx_arguments
      {
        (* a (punning) *)
        let loc_lident = mklocation $startpos($1) $endpos($1) in
        [($1, mkexp (Pexp_ident {txt = Lident $1; loc = loc_lident}) ~loc:loc_lident)] @ $2
      }
```

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
