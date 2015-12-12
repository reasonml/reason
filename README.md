Reason: Meta Language Utility
=========================================

- Approachable ES6 style syntax.
- Powerful, automatic source code formatting.
- Adopt Incrementally with straightforward `JavaScript/C` interop.
- Bare metal compilation - *No Virtual Machine*.
- Type inference, pattern matching, immutable by default.


Install/Build
----------
Download the [ReasonDocs](https://github.com/facebook/Reason/blob/master/docs.zip?raw=true)
and open the `index.html` file.

Integrating with Existing Languages.
------------------------

 **`JavaScript` and `CommonJS`**:

See the [ReasonDocs](https://github.com/facebook/Reason/blob/master/docs.zip?raw=true)


**`clang`/`ocamlc`/`ocamlopt`**:

Though the easiest way to use `Reason` with `ocamlc/opt` is by running the compiler with the following flags:
```sh
# intf-suffix tells the compiler where to look for corresponding interface files
ocamlopt -pp reasonfmt -intf-suffix rei -impl myFile.re
ocamlopt -pp reasonfmt -intf myFile.rei
```

`Reason` can also be activated using `ocamlfind`.

``` sh
ocamlfind ocamlc -package reason ...
```

Convert Your Project to Reason:
------------------------------------------------------------
`Reason` includes a program `reasonfmt` which will parse and print and
convert various syntaxes. You can specify which syntax to parse, and
which to print via that flags `-parse` and `-print` flags. For example,
you can convert your `ocaml` project to `Reason` by processing each file
with the command `reasonfmt -parse ml -print re yourFile.ml`. Execute
`reasonfmt -help` for more options.


Upgrading Existing Source Code After Changing Parse/Printing:
------------------------------------------------------------
To upgrade existing `Reason` syntax to a new version of the `Reason` parser
you should parse the code in the old version of the parser, and print it with
the new version of the printer. The easiest way to do this is to build the new
version locally, while the old one is pinned globally, but any way that you
have two versions installed locally will work.

The script `upgradeSyntax.sh` will execute reformattings for the entire
subdirectory repo across two separate builds of `Reason`. It expects you to
supply the path/command to the old build of Reason, the path/command for the
new build of Reason, and the print width (use `110`).  For example, if you had
an old version of `Reason` globally pinned as `reasonfmt`, and a local build of
`Reason` at `~/github/Reason/reasonfmt_impl.native`:

```sh
# Run from whichever directory you want to search
~/github/Reason/upgradeSyntax.sh reasonfmt ~/github/Reason/reasonfmt_impl.native 110
```

Deeper OCaml integration
---------------------------

If you are using `ocamlbuild`, add the following to your `_tags` file, but
this likely won't be enough because `ocamlc`/`ocamlopt` will need the
`-intf/-impl/-intf-suffix` flags:

```
<**/*.{re,.rei}>: package(reason), syntax(utf8)
```

[ReasonDocs](https://github.com/facebook/Reason/blob/master/docs.zip?raw=true) explains how to use
`Reason` with the top level or with `utop`, but it can be manually activated
from any top level by:

```
#require "reason";;
```

Additionally, the OCaml compiler exports its internals, including the parser,
in a package `compiler-libs`. This allows us to parse *directly* into the
`OCaml` AST, instead of having an additional AST -> AST conversion step.

Findlib provides an interface that allows registering a preprocessor.
Additionally, it will pass all package include paths to such a preprocessor.

Debugging Yacc Grammar Conflicts:
-------------------------
Run the main parser through yacc with the `-v` flag to have it print out
details about the conflict.  `ocamlyacc -v src/reason_parser.mly`.

Testing:
------------------
Run the tests in the `./formatTest/` directory and observe differences in
output. The test files contain the most obscure syntax forms intentionally.

Credit:
-------
The general structure of this project's organization and build/packaging
process was taken from @whitequark's m17n project, including parts of the
`README` that instruct how to use this with the OPAM toolchain.

License
-------

New content is licensed under the MIT license, works that are forked from other
projects are under their original licenses.
[MIT license](LICENSE.txt)

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.

Tests cases in formatTest/ocp/ were copied from ocp-indent and all retain their
original LGPL license.
