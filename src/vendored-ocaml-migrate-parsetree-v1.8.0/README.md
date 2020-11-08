# OCaml-migrate-parsetree
Convert OCaml parsetrees between different major versions

This library converts between parsetrees of different OCaml versions.

Supported versions are 4.02, 4.03, 4.04, 4.05, 4.06, 4.07, 4.08 and 4.09.
For each version, there is a snapshot of the parsetree and conversion functions
to the next and/or previous version.

## Asts

```ocaml
module Ast_402, Ast_403, Ast_404, Ast_405, Ast_406, Ast_407, Ast_408, Ast_409 : sig

  (* These two modules didn't change between compiler versions.
     Just share the ones from compiler-libs. *)
  module Location = Location
  module Longident = Longident

  (* Version specific copy of AST *)
  module Asttypes
  module Parsetree
  module Outcometree

  (* Other modules that are useful for implementing PPX.

     Docstrings and Ast_mapper only contain general definitions
     In particular, the internal state used by compiler-libs has been
     removed.
     Also equalities are lost for abstract types (Docstring.docstring).  *)
  module Docstrings
  module Ast_helper
  module Ast_mapper

  (* Magic numbers used for marshalling *)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end
```

These embed copies of AST definitions for each supported OCaml major version.

The AST matching the version of the OCaml toolchain will contain equalities
relating the copy of types to the definitions from compiler-libs.  For
instance, when installed with OCaml 4.04.x, `Ast_404.Parsetree` looks
like.

## Migration modules

For each pair of versions `$(n)` and `$(n+1)`, the two modules
`Migrate_parsetree_$(n)_$(n+1)` and `Migrate_parsetree_$(n+1)_$(n)` convert the AST forward and backward.

The forward conversion is total while the backward conversion is partial: when
a feature is not available in a previous version of the parsetree, a
`Migrate_parsetree_def.Migration_error` exception is raised detailing the
failure case.

`Migrate_parsetree_versions` abstract versions of the compiler. Each version is
represented as a module with `OCaml_version` signature.  Instances are named
`OCaml_402`, `OCaml_403`, ... `OCaml_current` is an alias to the version of the
current compiler.
The `Convert` functor takes two versions of OCaml and produce conversion
functions.

Finally, the `Migrate_parsetree_ast_io` provides an easy interface for
marshalling/unmarshalling.

## Migrate_parsetree.Driver

The `Migrate_parsetree.Driver` provides an API for ppx rewriters to
register OCaml AST rewriters. Ppx rewriters using this API can be used
as standalone rewriter executable or as part of a _driver_ including
several rewriters.

Using a single driver for several rewritings has the advantage that it
is faster. Especially when using many ppx rewriters, it can speed up
compilation a lot.

If using [Dune](https://github.com/ocaml/dune), you can
consult the dune manual to see how to define and use ppx
rewriters. Dune automatically creates drivers based on
ocaml-migrate-parsetree on demand.

The rest of this section describes how to do things manually or with
[ocamlbuild](https://github.com/ocaml/ocamlbuild).

## Building a custom driver using ocamlfind

To build a custom driver using ocamlfind, simply link all the ppx
rewriter libraries together with the
`ocaml-migrate-parsetree.driver-main` package at the end:

    ocamlfind ocamlopt -predicates ppx_driver -o ppx -linkpkg \
      -package ppx_sexp_conv -package ppx_bin_prot \
      -package ocaml-migrate-parsetree.driver-main

Normally, ocaml-migrate-parsetree based rewriters should be build with
the approriate `-linkall` option on individual libraries. If one is
missing this option, the rewriter might not get linked in. If this is
the case, a workaround is to pass `-linkall` when linking the custom
driver.

The resulting `ppx` program can be used as follow:

- `./ppx file.ml` to print the transformed code
- `ocamlc -pp './ppx --as-pp' ...` to use it as a pre-processor
- `ocamlc -ppx './ppx --as-ppx' ...` to use it as a `-ppx` rewriter

# Development

It started from the work of Alain Frisch in
[ppx\_tools](https://github.com/alainfrisch/ppx_tools).

The library is distributed under LGPL 2.1 and is copyright INRIA.

## Adding a new OCaml version

We use [Cinaps](https://github.com/janestreet/cinaps) to generate boilerplate.
You can install it via opam: `opam install cinaps`.

Add the new version in
[src/cinaps_helpers](https://github.com/ocaml-ppx/ocaml-migrate-parsetree/blob/master/src/cinaps_helpers)
`supported_versions`.

Copy the last `src/ast_xxx.ml` file to `src/ast_<new_version>.ml`,
then go over the file and update each sub-module by replacing its
signature and implementation with the code from the compiler. For the
`Config` sub-module, update the two variables with the values in
`utils/config.mlp` in the compiler source tree.

Once this is done, call:

    $ dune exec tools/add_special_comments.exe src/ast_<new_version>.ml

Then diff the `src/ast_xxx.ml` and `src/ast_<new_version>.ml` and go
over the diff to make sure the difference are relevant. The `ast_...`
files require some adjustments which should pop up when you do this
diff. Port the old adjustments to the new file as required.

Add migration functions:
- Manually compile the asts (`ocamlc -c src/ast_{NEW,OLD}.ml -I +compiler-libs -I _build/default/src/.migrate_parsetree.objs/byte/ -open Migrate_parsetree__`)
- Using `tools/gencopy.exe` (`dune build tools/gencopy.exe`), generate copy code to and from previous version (assuming it is 408):
```
_build/default/tools/gencopy.exe -I . -I src/ -I +compiler-libs -map Ast_409:Ast_408 Ast_409.Parsetree.{expression,expr,pattern,pat,core_type,typ,toplevel_phrase} Ast_409.Outcometree.{out_phrase,out_type_extension} > src/migrate_parsetree_409_408_migrate.ml
_build/default/tools/gencopy.exe -I . -I src/ -I +compiler-libs -map Ast_408:Ast_409 Ast_408.Parsetree.{expression,expr,pattern,pat,core_type,typ,toplevel_phrase} Ast_408.Outcometree.{out_phrase,out_type_extension} > src/migrate_parsetree_408_409_migrate.ml
```
- Fix the generated code by implementing new cases
- The migration functor expects specific names, look at `Migrate_parsetree_versions` interface.

*TODO*: specialize and improve gencopy for these cases

Add mapper lifting functions in the files `migrate_parsetree_NEW_408.ml` and
`migrate_parsetree_408_NEW.ml`:
- include the corresponding `Migrate_parsetree_40x_40y_migrate` module
- define `copy_mapper` function, look at existing `Migrate_parsetree_40x_40y`
  for guidance.

At any time, you can expand boilerplate code by running `make cinaps`.

Update build system:
- make sure `make cinaps` reaches a fixed point :)
- `make` should succeed
