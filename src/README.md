# Core Reason

**See a list of easy tasks [here](https://github.com/facebook/reason/labels/GOOD%20FIRST%20TASK)**

## Contributor Setup

Thanks for considering contributing to Reason! Here's the setup you need:

### With opam

```sh
# On OSX, install opam via Homebrew:
brew update
brew install opam
# On Linux, see here (you will need opam >= 1.2.2): http://opam.ocaml.org/doc/Install.html

opam init
# Add this to your ~/.bashrc (or ~/.zshrc), then do `source ~/.bashrc`
#   eval $(opam config env)

opam update
opam switch 4.04.2
eval $(opam config env)
git clone https://github.com/facebook/reason.git
cd reason
opam pin add -y reason .
```

### With esy

The esy workflow is still experimental.
If you would like to help test, you can try it with the following commands.

```sh
npm install -g esy
git clone https://github.com/facebook/reason.git
cd reason
esy install
esy build
esy x make test
```

Build commands like `make` that would normally be executed by
themselves will need to be prefixed with `esy `.

For more, see the [esy documentation](https://github.com/esy-ocaml/esy).

### Troubleshooting

- Is the previous pinning unsuccessful? We might have updated a dependency; try `opam update` then `opam upgrade`.
- During the last `opam pin` step, make sure your local repo is clean. In particular, remove artifacts and `node_modules`. Otherwise the pinning might go stale or stall due to the big `node_modules`.

### Build

`make build`. **If this fails on your machine but master passes**, it means your setup wasn't right. Could you check if you followed the above installation steps? In particular, make sure you did `eval $(opam config env)` and sourced your shell environment (if you don't know how, just open a new shell tab and it'll be sourced usually).

The generated artifacts are in `_build`. All the binaries are in `_build/install/default/bin`.

### Test

To do some one-off tests, try `echo "let a = 1" | _build/install/default/bin/refmt`

`make test` (make sure to follow the repo pinning instructions above!). The tests will output the difference between the expected syntax formatting and the actual one, if any.

Small exception: testing your changes in `rtop` is a little complicated, but you usually don't need to test it. If you do, you might have seen that your changes of the parser or printer don't affect `rtop` when you run it. Instead, you need to do `opam pin add -y reason .` and _then_ run `rtop` to see the Reason changes reflected.

## Repo Walkthrough

![reason_-_bucklescript_in_ocaml](https://user-images.githubusercontent.com/1909539/31158768-0c7e9d04-a879-11e7-9cfb-19780a599231.png)

(_Click to see a larger version_)

We're that orange part! The core of the codebase is a parser + a printer, plus other miscellaneous utilities we expose.

Throughout the codebase, you might see mentions of "migrate-parsetree", `Ast_404`, etc. These refer to https://github.com/let-def/ocaml-migrate-parsetree. It's a library that allows you to convert between different versions of the OCaml AST. This way, the Reason repo can be written in OCaml 4.04's AST data structures, while being usable on OCaml 4.02's libraries (BuckleScript's on 4.02 too).

Our lexer & parser use [Menhir](http://gallium.inria.fr/~fpottier/menhir/), a library that helps us with parsing (it's a "parser generator"). We **highly recommend** you to read about Menhir [here](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html).

### Core Files

- `src/reason-parser/reason_lexer.mll`: the lexer that chunks a raw string into tokens. See the file for more comments.

- `src/reason-parser/reason_parser.mly`: the parser that takes the lexer's result and turns it into a proper AST (abstract syntax tree). See the file for more comments.

- `src/reason-parser/reason_pprint_ast.ml`: the pretty-printer! This is the reverse of parsing: it takes in the AST (abstract syntax tree) and prints out the nicely formatted code text.

- `src/reason-parser/reason_parser.messages.checked-in`: this is the huge table of mostly generated, sometimes hand-written, syntax error messages. When the parser ends up at an invalid parsing state (aka ends up with a syntax error), it'd refer to that file's content and see if that case has a specific error message assigned to it. For an example fix, see [this PR](https://github.com/facebook/reason/pull/1018) and the [follow-up](https://github.com/facebook/reason/pull/1033). To add a syntax error message see [the wiki page](https://github.com/facebook/reason/wiki/Add-a-Menhir-error-message).
  - When running `make build`, and a new `reason_parser.messages` file is generated, do a `mv reason_parser.messages reason_parser.messages.checked-in` to persist the updated messages.

- `src/reason-parser/reason_oprint.ml`: the "outcome printer" used by Merlin, rtop and terminal, that prints the errors in Reason syntax. More info in the file itself.

- `src/reason-parser/menhir_error_processor.ml, reason_parser_explain.ml`: two files that allows us to batch assign a better syntax error message for a category of errors, like accidentally using a reserved token. reason_parser_explain_raw.ml`. More info in the comments of these files.

### Miscellaneous Files

- `ocamlmerlin_reason.ml`: produces the `ocamlmerlin-reason` binary, used in conjunction with [Merlin-extend](https://github.com/let-def/merlin-extend). This is an extension to [Merlin](https://github.com/ocaml/merlin), which picks up this binary from your environment to analyze Reason files when your editor calls Merlin.

- `*.mllib`: related: see the [OCaml extensions list](https://github.com/facebook/reason/wiki/OCaml-Ecosystem-Extensions-List). These are generated file from `pkg/build.ml`, which describes the package we distribute. No need to worry about them.

- `src/reason-parser/reason_config.ml`: global configuration that says whether our parser should run in "recoverable" mode. Merlin has a neat feature which lets it continue diagnosing e.g. type errors even when the file is syntactically invalid (at the expense of the accuracy of those type error reports' quality). Searching `reason_config` in our codebase will show you how this is used.

- `src/refmttype/reason_format_type.ml`, `reason_type_of_ocaml_type.ml`: again, see `pkg/build.ml`. These produce the `refmttype` binary, used by [BetterErrors](refmttype) to output compiler errors in Reason syntax rather than the OCaml one.

- `src/reason-parser/reason_parser.messages`: auto-generated from parser changes. Menhir generates parsing code that assigns each syntax error to a code, and lets us customize these errors. Syntax errors can be very precisely pinpointed and explained this way.

- `src/reason-parser/reason_toolchain.ml`, `src/reason-parser/refmt_impl.ml`: the entry point that calls the parsing logic.

- `src/rtop/reason_utop.ml`, `src/rtop/reason_toploop.ml`, `src/rtop/rtop_init.ml`: Reason's [Utop](https://github.com/diml/utop) integration. Utop's the terminal-based REPL you see when executing `utop` (in Reason's case, the wrapper `rtop`).

- `src/reasonbuild/myocamlbuild.ml`: our wrapper for [OCamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/), a simple build system.

- `*.sh`: some of our binaries' entries.

- `src/rtop/reason_util.ml`, `syntax_util.ml`: utils.

- `src/reason-parser/reactjs_jsx_ppx_v2.ml/v3.ml`: our ReactJS interop that translates [Reason JSX](https://reasonml.github.io/guide/language/jsx) into something that ReactJS understands. See the comments in the file and the description in [ReasonReact](https://reasonml.github.io/reason-react/#reason-react-jsx).

- `src/reason-parser-tests/testOprint.ml`: unit tests for the outcome printer mentioned above. See the file for more info on how outcome printing is tested.

## Working With Parser

Here's a recommended workflow:

- First put your code in the current master syntax in a file `test.re`
- `make build`
- `./_build/install/default/bin/refmt --print ast test.re`
- look closely at the ast, spot the thing you need
- Search for your item in `reason_parser.mly`
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
- [Don't apply sugar to Js.t({.}) and Js.t({..})](https://github.com/facebook/reason/pull/1502)
- [Pun record destructuring with renaming](https://github.com/facebook/reason/pull/1517)
- [Add support for simple pattern direct argument with array, list & record](https://github.com/facebook/reason/pull/1528)
- [Fix outcome printer record value printing](https://github.com/facebook/reason/pull/1529)
- [Print`foo(()) as `foo() + update parser](https://github.com/facebook/reason/pull/1560)
- [Allow parsing of constraint expressions without parens inside constructor expr](https://github.com/facebook/reason/pull/1576)
- [Don't print fun in record expressions with Pexp_fun values](https://github.com/facebook/reason/pull/1588)
- [Force breaks for nested records](https://github.com/facebook/reason/pull/1593)
- [Always break object def with two or more rows](https://github.com/facebook/reason/pull/1596)
- [Make exponentiation operator print with right associativity](https://github.com/facebook/reason/pull/1678)

### Debugging Grammar Conflicts

Run the main parser through Menhir with the `--explain` flag to have it print out details about the conflict. `menhir --explain src/reason-parser/reason_parser.mly`. The debug information can be found at `src/reason-parser/reason_parser.conflicts`.

### Debugging the Parser State at Runtime

If you set the environment variable as follows, the parser state will be printed out as it parses files.

```sh
export OCAMLRUNPARAM='p'
```

### Add a Menhir Error Message

To add a Menhir error message, you first need to know the error code. To find the error code, you can run the following commands from the Reason project root:

```
make
./_build/install/default/bin/refmt --parse re foo.re
```

Where `foo.re` contains a syntax error. This will result in an error message like:

```
File "test2.re", line 4, characters 2-6:
Error: 2665: <UNKNOWN SYNTAX ERROR>
```

Here, the error code is 2665. We then search for this code in `src/reason-parser/reason_parser.messages`.

- If you find it, you can add a better error message instead of the not so descriptive `<UNKNOWN SYNTAX ERROR>`.

To test the new error message you can run the following commands again:

```
make
./refmt --parse re foo.re
```

Then submit a PR!

### Improve Error Message Locations

In some situations, Reason might report errors in a different place than where it occurs. This is caused by the AST not having a correct location for a node. When the error reports a location that's simply at the top of the file, that means we likely didn't attach the location in the parser correctly, altogether.

Before digging into Reason parser, make sure this isn't actually caused by some PPX. Otherwise, run:

```
make
./_build/install/default/bin/refmt --parse re --print ast test.re
```

Where `test.re` has the code that produces the error message at the wrong location.

In the printed AST, you can see nodes such as `Ptyp_constr "int" (test.re[1,0+15]..[1,0+18])` where the part between parentheses is the location of the node.

The error message's own location will look like `([0,0+-1]..[0,0+-1])` too.

To fix this, we need to find the AST node in `src/reason-parser/reason_parser.mly`. It's a big file, but if you search for the AST node, you should be able to find the location (if not, bug us on Discord). It will probably involve a `mkexp` or `mkpat` without a proper `~loc` attached to it.

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
ocamlc -dparsetree -ppx ./_build/install/default/bin/reactjs_jsx_ppx_v2.native -pp "./_build/install/default/bin/refmt_impl.native --print binary" -impl foo.re
```

That dumps the AST after accepting the ppx and the reason syntax. You can also dump the final code in Reason syntax instead:

```
ocamlc -dsource -ppx ./_build/install/default/bin/reactjs_jsx_ppx_v2.native -pp "./_build/install/default/bin/refmt_impl.native --print binary" -impl foo.re | ./_build/install/default/bin/refmt_impl.native --parse ml --print re --interface false
```

(Similar steps for reactjs_jsx_ppx_v3.)

## Testing Two Different Syntax Versions

If you'd like to convert from an old Reason syntax version to the one in master (whether to debug things, or if you're interested in trying out some syntax changes from master and then explaining to us why your perspective on the Reason syntax is the best, lol):

- Revert the repo to the old commit you want
- Build, through `make build`
- Move the built refmt binary `./_build/install/default/bin/refmt` somewhere else
- Revert back to master
- `make build` again to get the master binary.

Then do:

```
./_build/install/default/bin/refmt --parse my_old_syntax_file.re --print binary_reason | ./refmt_impl --parse binary_reason --print re
```

Basically, turn your old syntax into an AST (which is resilient to syntax changes), then turn it back into the new, textual code. If you're reverting to an old enough version, the old binary's flags and/or the old build instructions might be different. In that case, see `./_build/install/default/bin/refmt -help` and/or the old README.

## Cutting a release

### OPAM

Reason exists on OCaml's package manager OPAM.

- Make sure local changes are properly committed
- Update remote:

```sh
git fetch;
git reset --hard origin/master
```

- Prerelease:

```sh
env version=x.y.z make pre_release
```

- Check everything is ok locally
- Release!

```sh
env version=x.y.z make release
```

- Use [opam-publish](https://github.com/ocaml/opam-publish) to publish the latest version to opam.

### NPM

Reason's also on npm, albeit for a different purpose. The `refmt.js` file is distributed there, for use-cases where the native `refmt` doesn't suffice (e.g. using it on the web).

To publish to npm, get https://github.com/sindresorhus/np and run `np`.
