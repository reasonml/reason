# Core Reason

## Contributor Setup


### With esy

```sh
# Make sure you have the latest esy
npm install -g esy@next
git clone https://github.com/facebook/reason.git
cd reason
esy
```


#### Testing:

**Test Suite:**

```sh
esy test # Run tests
```

**One Off Tests:**

Start up the `rtop` top level with your changes:

```sh
esy x rtop
```

Pipe some text to `refmt` with your changes:

```sh
echo "let a = 1" | esy x refmt
```


> **`esy` tips:**
> - `esy x your command` will run one command `your command` in an environment
>   where the projects are built/installed. `esy x which refmt` will build the
>   packages and install them for the duration of one command - `which refmt`.
>   This will print the location of the built `refmt` binary.
> - For more, see the [esy documentation](https://github.com/esy-ocaml/esy).


> All the built binaries are in `esy echo '#{self.target_dir}/install/default/bin'`.


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
opam pin add -y rtop .
```
> **Opam Troubleshooting:**
> - Is the previous pinning unsuccessful? We might have updated a dependency;
>   try `opam update` then `opam upgrade`.
> - During the last `opam pin` step, make sure your local repo is clean. In
>   particular, remove artifacts and `node_modules`. Otherwise the pinning
>   might go stale or stall due to the big `node_modules`.


## Repo Walkthrough

![reason_-_bucklescript_in_ocaml](https://user-images.githubusercontent.com/1909539/31158768-0c7e9d04-a879-11e7-9cfb-19780a599231.png)
(_Click to see a larger version_)

Reason is the orange part. The core of the codebase is a parser + a printer, plus other miscellaneous utilities we expose.

Throughout the codebase, you might see mentions of "migrate-parsetree", `Ast_404`, etc. These refer to https://github.com/let-def/ocaml-migrate-parsetree. It's a library that allows you to convert between different versions of the OCaml AST. This way, the Reason repo can be written in OCaml 4.04's AST data structures, while being usable on OCaml 4.02's libraries (BuckleScript's on 4.02 too).

The Reason lexer & parser use [Menhir](http://gallium.inria.fr/~fpottier/menhir/), a library that generates parsers. You can read more about Menhir [here](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html).

### Core Files

- `src/reason-parser/reason_lexer.mll`: the lexer that chunks a raw string into tokens. See the file for more comments.

- `src/reason-parser/reason_parser.mly`: the parser that takes the lexer's result and turns it into a proper AST (abstract syntax tree). See the file for more comments.

- `src/reason-parser/reason_pprint_ast.ml`: the pretty-printer! This is the reverse of parsing: it takes in the AST (abstract syntax tree) and prints out the nicely formatted code text.

- `src/reason-parser/reason_parser.messages.checked-in`: this is the huge table of mostly generated, sometimes hand-written, syntax error messages. When the parser ends up at an invalid parsing state (aka ends up with a syntax error), it'd refer to that file's content and see if that case has a specific error message assigned to it. For an example fix, see [this PR](https://github.com/facebook/reason/pull/1018) and the [follow-up](https://github.com/facebook/reason/pull/1033). To add a syntax error message see the "Add a Menhir Error Message" section below.
  - When running `esy`, and a new `reason_parser.messages` file is generated, do a `mv reason_parser.messages reason_parser.messages.checked-in` to persist the updated messages.

- `src/reason-parser/reason_oprint.ml`: the "outcome printer" used by Merlin, rtop and terminal, that prints the errors in Reason syntax. More info in the file itself.

- `src/reason-parser/menhir_error_processor.ml, reason_parser_explain.ml`: two files that allows us to batch assign a better syntax error message for a category of errors, like accidentally using a reserved token. More info in the comments of these files.

### Miscellaneous Files

- `ocamlmerlin_reason.ml`: produces the `ocamlmerlin-reason` binary, used in conjunction with [Merlin-extend](https://github.com/let-def/merlin-extend). This is an extension to [Merlin](https://github.com/ocaml/merlin), which picks up this binary from your environment to analyze Reason files when your editor calls Merlin.

- `*.mllib`: related: see the [OCaml extensions list](https://reasonml.github.io/docs/en/faq.html#i-m-seeing-a-weird-cmi-cmx-cmj-cma-file-referenced-in-a-compiler-error-where-do-these-files-come-from-). These are generated file from `pkg/build.ml`, which describes the package we distribute. No need to worry about them.

- `src/reason-parser/reason_config.ml`: global configuration that says whether the parser should run in "recoverable" mode. Merlin has a neat feature which lets it continue diagnosing e.g. type errors even when the file is syntactically invalid (at the expense of the accuracy of those type error reports' quality). Searching `reason_config` in the codebase will show you how this is used.

- `src/refmttype/reason_format_type.ml`, `reason_type_of_ocaml_type.ml`: again, see `pkg/build.ml`. These produce the `refmttype` binary, used by [BetterErrors](refmttype) to output compiler errors in Reason syntax rather than the OCaml one.

- `src/reason-parser/reason_parser.messages`: auto-generated from parser changes. Menhir generates parsing code that assigns each syntax error to a code, and lets us customize these errors. Syntax errors can be very precisely pinpointed and explained this way.

- `src/reason-parser/reason_toolchain.ml`, `src/reason-parser/refmt_impl.ml`: the entry point that calls the parsing logic.

- `src/rtop/reason_utop.ml`, `src/rtop/reason_toploop.ml`, `src/rtop/rtop_init.ml`: Reason's [Utop](https://github.com/diml/utop) integration. Utop's the terminal-based REPL you see when executing `utop` (in Reason's case, the wrapper `rtop`).

- `*.sh`: some of the binaries' entries.

- `src/rtop/reason_util.ml`, `reason_syntax_util.ml`: utils.

- `src/reason-parser/reactjs_jsx_ppx_v2.ml`: the ReactJS interop that translates [Reason JSX](https://reasonml.github.io/docs/en/jsx.html) into something that ReactJS understands. See the comments in the file and the description in [ReasonReact](https://reasonml.github.io/reason-react/#reason-react-jsx).

- `src/reason-parser-tests/testOprint.ml`: unit tests for the outcome printer mentioned above. See the file for more info on how outcome printing is tested.

## Working With Parser

Here's a recommended workflow:

- First put your code in the current master syntax in a file `test.re`
- `esy x refmt --print ast test.re`
- look closely at the ast, spot the thing you need
- Search for your item in `reason_parser.mly`
- Change the logic
- `esy test`

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

Run the main parser through Menhir with the `--explain` flag to have it print
out details about the conflict. `esy menhir --explain src/reason-parser/reason_parser.mly`.
The debug information can be found at `src/reason-parser/reason_parser.conflicts`.

### Debugging the Parser State at Runtime

If you set the environment variable as follows, the parser state will be printed out as it parses files.

```sh
export OCAMLRUNPARAM='p'
```

### Add a Menhir Error Message

To add a Menhir error message, you first need to know the error code. To find the error code, you can run the following commands from the Reason project root:

```
esy x refmt --parse re foo.re
```

Where `foo.re` contains a syntax error. This will result in an error message like:

```
File "test2.re", line 4, characters 2-6:
Error: 2665: <syntax error>
```

Here, the error code is 2665. We then search for this code in `src/reason-parser/reason_parser.messages`.

- If you find it, you can add a better error message instead of the not so descriptive `<syntax error>`.

To test the new error message you can run the following commands again:

```
esy x refmt --parse re foo.re
```

Then submit a PR!

### Improve Error Message Locations

In some situations, Reason might report errors in a different place than where it occurs. This is caused by the AST not having a correct location for a node. When the error reports a location that's simply at the top of the file, that means we likely didn't attach the location in the parser correctly, altogether.

Before digging into Reason parser, make sure this isn't actually caused by some PPX. Otherwise, run:

```
esy x refmt --parse re --print ast test.re
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


## Testing Two Different Syntax Versions

If you'd like to convert from an old Reason syntax version to the one in master (whether to debug things, or if you're interested in trying out some syntax changes from master and then explaining to us why your perspective on the Reason syntax is the best, lol):

- Revert the repo to the old commit you want
- Build, through `esy`
- Move the built refmt binary `esy x which refmt` somewhere else
- Revert back to master
- `esy x which refmt` again to get the master binary.

Then do:

```
esy x refmt --parse my_old_syntax_file.re --print binary_reason | ./refmt_impl --parse binary_reason --print re
```

Basically, turn your old syntax into an AST (which is resilient to syntax changes), then turn it back into the new, textual code. If you're reverting to an old enough version, the old binary's flags and/or the old build instructions might be different. In that case, see `esy x refmt --help` and/or the old README.
