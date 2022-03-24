Title:  Guide to OCaml Migrate Parsetree
Author: Frédéric Bour, @let-def
Date:   March 9, 2017


**Table of Contents**

- [Manipulating parsetree](#manipulating-parsetree)
  - [Talking about different versions of the compiler](#talking-about-different-versions-of-the-compiler)
  - [Migrating between compiler versions](#migrating-between-compiler-versions)
  - [(Un)marshalling AST](#unmarshalling-ast)
- [Drivers](#drivers)
  - [The legacy way](#the-legacy-way)
  - [New registration interface](#new-registration-interface)
  - [A minimal driver](#a-minimal-driver)
  - [Custom and standalone drivers](#custom-and-standalone-drivers)
- [ppx_tools_versioned](#ppx_tools_versioned)
    - [ppx_metaquots](#ppx_metaquots)
- [Findlib specification](#findlib-specification)
    - [Standalone *"--as-ppx"* rewriters in META](#standalone---as-ppx-rewriters-in-meta)
    - [Using arguments in META ppxopt](#using-arguments-in-meta-ppxopt)
    - [Conventions for distributing a linkable ppx rewriter](#conventions-for-distributing-a-linkable-ppx-rewriter)
- [Troubleshooting](#troubleshooting)
  - [Accessing shadowed compiler libs module](#accessing-shadowed-compiler-libs-module)
  - [Using functions from compiler-libs results in (unfriendly) type errors](#using-functions-from-compiler-libs-results-in-unfriendly-type-errors)
  - [Features not supported in targeted version](#features-not-supported-in-targeted-version)
    - [What kind of guarantees to expect in practice?](#what-kind-of-guarantees-to-expect-in-practice)

This library is designed to make PPX rewriters portable across compiler versions.

It works by versioning the definitions of OCaml AST. This includes `Parsetree`, `Asttypes`, `Outcometree`, `Ast_helper` and most of `Docstrings` and `Ast_mapper`.

*Note:* `Docstrings` and `Ast_mapper` contain some global state which was removed during versioning. This affect registration of rewriters when using `Ast_mapper` as a driver. See the [driver section](#drivers) for reliable solutions.

# Manipulating parsetree

Most of the work happens by shadowing. If your PPX rewriter was written against OCaml 4.04 AST, just `open Ast_404` (alternatively, you can pass `-open Ast_404` when building the file).

This will introduce the versioned modules in scope. When compiled with other supported versions of OCaml, the definitions are still compatible with 4.04.

While this is enough to manipulate the AST from within your code, you can no longer have expectations on the version of `compiler-libs`. The rest of the `Migrate_parsetree` module provides tools to deal with that.

## Talking about different versions of the compiler

The module `Migrate_parsetree.Versions` provides a way of abstracting compiler versions and getting functions to migrate from one version to another.

The interface of the module is quite technical, but one doesn't need to understand all details to work with the module.

The main problem that it solves is being able to talk about a "signature" or "structure" without being tied to a specific compiler version (that is, while being polymorphic over concrete versions of the compiler).

The module type `Ast` lists all the types that are abstracted for each version. The type `ocaml_version` and the module type `OCaml_version` represent ocaml versions in the term and module languages.

Instances are given by the values `ocaml_402`, `ocaml_403`, ... and the modules `OCaml_402`, `OCaml_403`...

The `ocaml_current` and `OCaml_current` definitions are special in that they refer to versions compatible with compiler-libs (the current compiler).

Functions and functors that operate across compiler versions will take these as arguments.

## Migrating between compiler versions

When migrating between two known compiler versions, the modules `Migrate_parsetree.Migrate_40x_40y` contain functions to transform values between two consecutive versions.

For instance `Migrate_402_403.copy_signature` turns a signature of OCaml 4.02 into a signature for OCaml 4.03. `Migrate_404_403.copy_mapper` transforms an `Ast_mapper.mapper` for OCaml 4.04 into a mapper for OCaml 4.03.

When working with an arbitrary version, it becomes useful to quantify over versions and migrations. The `Migrate_parsetree.Versions` module comes again to the rescue.

The `migrate_functions` record is a list of functions for converting each type.

The function `Versions.migrate` takes two OCaml version and returns a migration record between the two. The functor `Convert` does the same at the module level.

## (Un)marshalling AST

The `Ast_io` module implements AST marshalling and unmarshalling abstracted over OCaml versions.

It can read and write binary implementation and interface files from different compiler versions and pack them with the corresponding `Versions.OCaml_version` module.

(FIXME: marshalling format is not guaranteed to be stable accross versions)

## Parsing source file

The `Parse` module implements an interface similar to the one from compiler-libs, but parsing functions take an OCaml version as first argument.

It uses the distributed OCaml parser (current version) then migrate the resulting AST to the requested version. Beware, these parsing functions can alse raise `Migration_error` exceptions.

# Driver

So far, all tools presented were for working with parsetrees. This is helpful to implement a mapper object, but it is not enough to get to a PPX binary.

Drivers fulfill this last step: going from one or more AST mappers to a concrete binary that will do the rewriting.

## The legacy way

Traditionally, mappers had to be registered in `Ast_mapper`; either with `Ast_mapper.register` or `Ast_mapper.run_main`.

The registration interface was removed from versioned modules.  If you try to register with `Ast_mapper` from compiler-libs, remember to migrate the version.

In a few lines of code:

```ocaml
(* Assuming rewriter is written against OCaml 4.04 parsetree *)
let migration =
  Versions.migrate Versions.ocaml_404 Versions.ocaml_current

let () =
  (* Refer to unshadowed mapper *)
  Compiler_libs.Ast_mapper.register
    (fun args -> migration.copy_mapper (my_mapper args))
```

This method might be convenient for quickly migrating existing rewriters, but we are trying to get away from `Ast_mapper` global state.

*ocaml-migrate-parsetree* offers a new, forward looking, interface.

## New registration interface

In the new interface, the state that can be accessed by a PPX rewriter is made more explicit.
- *Compiler configuration* via `Driver.config`; it snapshots the few compiler settings that are guaranteed to be set by the compiler API.
- *Cookies* via `Driver.cookies`, `get_cookies` and `set_cookies`, which work across different versions.
- *Command-line arguments*; when registering a mapper, one can provide argument specifications, as defined by the [`Arg`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html) module.

Rewriters no longer receive an arbitrary list of arguments.  Everything happens through the specifications.  Collision in rewriter names and argument keys is *an error*: a rewriter should be registered only once, each key should be used only once.

```ocaml
open Ast_404 (* Target 4.04 parsetree *)

(* Rewriter settings *)
let foo_config : string option ref = ref None

let set_foo bar = foo_config := Some bar
let reset_args () = foo_config := None

let args = [
  ("-foo", Arg.String set_foo, "<bar> Foo value to use in the rewriter")
]

(* Rewriter implementation *)

let my_rewriter config cookies =
  let foo = match !foo_config with
    | None -> raise (Arg.Bad "-foo is mandatory")
    | Some foo -> foo
  in
  {Ast_mapper.default_mapper with ...}

(* Registration *)

let () =
  Driver.register ~name:"hello_world" ~reset_args ~args
    Versions.ocaml_404 my_rewriter
```

## A minimal driver

The code above gets the rewriter registered, but this won't produce a runnable binary.  One or more rewriters can be registered, the final step will be to run them.

`Driver.run_as_ast_mapper` is suitable as an argument to `Ast_mapper.run_main` (or even `Ast_mapper.register`). It acts as a "meta-mapper" that will apply all the registered mappers.

`Driver.run_as_ppx_rewriter` does that, calling `Ast_mapper.run_main Driver.run_as_ast_mapper`.

The order is chosen to minimize the number of rewriting that happens:
- rewriters are sorted by versions, lower versions first
- rewriters targeting the same version are applied in the registration order

## Custom and standalone drivers

Using `Driver.run_main` as an entry point offers a way to make custom and standalone rewriters.

A standalone rewriter can be used independently of the OCaml compiler.
It can rewrite source files or save processed ASTs. Try `./myrewriter --help` for more information.

When the first argument is "--as-ppx", it behaves like a normal PPX and is suitable for use with "-ppx" (`ocamlc -ppx "./myrewriter --as-ppx"`).

Linking the `ocaml-migrate-parsetree.driver-main` package has the effect of just calling `Driver.run_main`. It should be linked last.

The purpose is to let you make a custom rewriter that link all the PPX in use in your project to reduce the overhead of rewriting:

```shell
ocamlfind ocamlopt -linkpkg -package rewriter1,rewriter2,... \
  -package ocaml-migrate-parsetree.driver-main -o myrewriter
```

# ppx_tools_versioned

Some rewriters make use of the *ppx_tools* package that offers conveniences for manipulating parsetrees.  As *ppx_tools* itself uses compiler-libs, using it directly defeats the purpose of *ocaml-migrate-parsetree*.

We provide the [ppx_tools_versioned](https://github.com/let-def/ppx_tools_versioned) package to overcome this. It offers migrate friendly versions of `Ast_convenience`, `Ast_lifter`, `Ast_mapper_class` and `Ppx_metaquot`.

To use these versions, just append `_40x` to the module names or `open Ppx_tool_40x` module.

```ocaml
(* Original code *)
open Ast_mapper_class

class my_mapper =
  object
    inherit mapper
    ...
end

(* Targeting 4.04 *)
open Ast_404
open Ppx_tools_404

open Ast_mapper_class

class my_mapper =
  object
    inherit mapper
    ...
end

(* Alternatively, if you use a single module from Ppx_tools *)
open Ast_mapper_class_404

class my_mapper =
  object
    inherit mapper
    ...
end
```

### ppx_metaquots

The *metaquot* rewriter allows quoting of the OCaml AST. The version provided by *ppx_tools* will quote the Parsetree from *compiler-libs*.

The versioned ones are accessed by using *ppx_tools_versioned.metaquot_40x* packages.

For instance, *ppx_tools_versioned.metaquot_404* will quote `Ast_404.Parsetree`.

# Findlib specification

Some precautions have to be taken when writing *META* files for *ocaml-migrate-parsetree* driven PPXs. The ppx and ppxopt directives are affected.

## Standalone *"--as-ppx"* rewriters in META

If your rewriter is produced as standalone rewriter, then you have to pass the "--as-ppx" argument first:
```diff
-ppx = "./my_ppx"
+ppx = "./my_ppx --as-ppx"
```

As long as the PPX command line begins with `./`, findlib will expand the path to an absolute directory and you will get the correct invocation:

```
/home/me/.opam/.../my_lib/./my_ppx --as-ppx
```

## Using arguments in META ppxopt

Since rewriters use the `Arg` module to specify command-line arguments,  anonymous arguments are no longer allowed.

If you used to pass anonymous arguments with ppxopt, you should pick an argument name and prefix them. For instance:

```
-ppxopt = "my_ppx,./bar"
+ppxopt = "my_ppx,-foo,./bar"
```

As you can see, arguments are separated by commas. Commas ensure that filename expansion still happens, such that invocation looks like:

```
/home/me/.opam/.../my_lib/./my_ppx ... -foo /home/me/.opam/.../my_lib/./bar
```

## Conventions for distributing a linkable ppx rewriter

The common case is to run ppx binaries on-demand: a findlib package describing a ppx rewriter will essentially add a new `-ppx my_binary` argument to the compiler invocation.

It is also possible to link and run a dedicated binary that will apply many rewriters consecutively. A package following that convention will use *ocaml-migrate-parsetree* to register a rewriter using `Driver.register`, but not do any actual rewriting (no `-ppx ...`).

The build system of a project making use of this feature will first build a custom rewriter that links all the necessary packages to produce a first binary.  This binary is then used as the only ppx rewriter for the main source files of this project.

The convention to distinguish when a ppx package is used as a rewriter and when it is used a library is to use two findlib predicates (see [META](http://projects.camlcity.org/projects/dl/findlib-1.7.1/doc/ref-html/r759.html) documentation and also `ocamlfind(1)` man page):

- `custom_ppx`: we are building a custom ppx driver, no rewriting should be done now (in other words, don't pass `-ppx ...` argument)
- `ppx_driver`: we are making our own driver, registration should be done using `Driver.register`

### Linking example

```shell
$ ocamlfind opt -o my_driver -linkpkg -predicates custom_ppx,ppx_driver -package ppx_tools_versioned.metaquot_402 -package ocaml-migrate-parsetree.driver-main
```

The predicates change the behavior of `ppx_tools_versioned.metaquot_402` package. Linking `ocaml-migrate-parsetree.driver-main` lasts executes all the rewriters that were registered.

### Package example

META
```
version = "1.0"
description = "dummy ppx"
requires = "ocaml-migrate-parsetree"
ppx(-custom_ppx,-ppx_driver) = "./ppx_dummy --as-ppx"
archive(byte,ppx_driver) = "ppx_dummy.cma"
archive(native,ppx_driver) = "ppx_dummy.cmxa"
```

Rewrite only when `custom_ppx` is not defined.
Link *ppx_dummy* objects when `ppx_driver` is defined.

# Troubleshooting

## Accessing shadowed compiler libs module

`Migrate_parsetree` defines a `Compiler_libs` module that reexports all modules that could have been shadowed by `Ast_40x` modules.

## Using functions from compiler-libs results in (unfriendly) type errors

Remember that because of abstraction, most values manipulated from within the rewriter have types that are unrelated to compiler-libs definitions.

For instance, you cannot directly use `Pprintast.core_type` to print a type. You should first make a migration record for the version you are targeting and then lift the `core_type` instance:

```ocaml
(* Assuming rewriter is written against OCaml 4.04 parsetree *)
let migration =
  Versions.migrate Versions.ocaml_404 Versions.ocaml_current

let print_core_type fmt typ =
  Pprintast.core_type fmt (migration.copy_core_type typ)
```

As for the error message, it contains all information needed to be polymorphic over a whole version of compiler parsetree. Pick what is relevant to your use case :-).

## Features not supported in targeted version

When converting to an earlier version, some features might not be supported. In this case, the migration library will raise an exception. You can find the definition of these cases in `Migrate_parsetree.Def`.

A reasonable error message is provided by default, otherwise you should catch `Migration_error` exceptions after any call to a migration function  (either a call to a function from `Migrate_40x_40y` or to a field of `migrate_functions` record). Only backward migrations are partials.

### What kind of guarantees to expect in practice?

The fact that migrations are partial functions can seem too restrictive.
In practice, a problem only happens when an OCaml construction is used that didn't exist in the version the PPX rewriter was implemented with.

This cannot occur when a new version of the compiler is released: existing code that was working before should work immediately after an update, since new features are not yet in use. This use case is the critical one for helping the introduction of a new compiler version (an opam switch should be usable readily after update).

In the future, we might allow rewriting of unsupported features into extensions or attributes for rewriters that opt-in. Rewriting would succeed as long as all extensions disappeared when reaching the compiler (for instance, an OCaml 4.04 file using inline records could be rewritten by a rewriter targeting 4.02; however, a 4.02 files couldn't be rewritten by a 4.04 PPX that introduces inline records).

Please voice your concerns if you have any, so that this use case is better understood.4.02
