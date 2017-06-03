This subdirectory is used for packing up the entire Reason `refmt` into a single file through BuckleScript's [bspack](https://github.com/bloomberg/bucklescript/blob/master/jscomp/core/bspack_main.ml), thus discarding all intermediate steps needed to build Reason, except for the final `refmt` binary compilation. Likewise for `reactjs_jsx_ppx`.

This makes our installation much friendlier to e.g. Windows. BuckleScript currently includes the two bundles in its own repo, thus making Reason first-class.

## Build

Here is what we found to be required to create bspacks of reason 1.13.5. These
instructions may or may not be applicable to the next version of Reason.

- Install reason-cli globally (because reason-cli installation that
  keeps the sources and build artifacts around after the installation).
  - Opam by default doesn't keep build artifacts around so it makes it more
    difficult.
  - Reason-cli does keep those artifacts around by default in
    `~/.esy/store-3.x.x/_build`.
- The more reason's dependencies are converted to jBuilder, the easier it is
  to run bspack.
  - This is because jBuilder constructs its own ppx pipeline instead of using
    the compiler's built in pipeline, which would normally discard the output
    of ppx. jBuilder keeps the output around in the build artifacts
    directories. jBuilder + esy's deafult of keeping build artifacts means that
    it's much easier to make progress on getting a bspack (which doesn't
    support ppx)
- Things that make bspacking harder: Reason's vendored copy of
  `Ppx_deriving_show`, or package dependencies that use ppx, but that don't use
  jBuilder.


## Manually creating a bspack.

There is no automated way to create a bspack from an arbitrary set of
dependencies, but a good portion of them will work automatically if you add the
right paths. For the remaining, you just try running bspack and seeing what
fails to compile, and fixing issues one by one. As reason itself is refactored
and evolved, the process would need to be repeated, but for small changes and
improvements to reason, you won't need to go through the whole process, and
instead can just "splice in" the small changes to Reason parser/printer
directly by hand in the bspack `.ml` files.


## Version 1.13.5

Here is what was found to be required to run bspack on version 1.13.5. This
will likely be different for future versions of Reason.

To generate packs that compile, we need to substitute some some stubs for ppx
heavy dependencies that weren't yet converted to jBuilder (`Ppx_deriving_show`
for example). If `Ppx_deriving_show` is converted to `jBuilder`, stubs will no
longer be needed.

For some reason, the `Result` module also needed stubbing out. Here is the
bspack prelude string that creates the appropriate stubs:

    preludeStr="module Ppx_deriving_show = struct let sig_of_type ~options ~path decl = [] let str_of_type ~options ~path typ = [] end module Result = struct type ('a, 'b) result = Ok of 'a | Error of 'b end open Result"

When creating the bspack, we need to tell bspack to ignore those modules for
which we've created and injected stubs. There's a flag for that.

Then you can grab the directories with the build artifacts that you want bspack
to consume. If you used reason-cli then they are stored in the ~/.esy/ cache
here: (Find their respective locations on your machine)

    menhirSuggestedLib=`menhir --suggest-menhirLib'`

    reasonTargetDir=~/.esy/store-3.x.x/_build/reason-blahblah
    ppxToolsVersionedTargetDir=~/.esy/store-3.x.x/_build/ppx__tools__versioned-blahblah
    reasonParserTargetDir=~/.esy/store-3.x.x/_build/blahblah
    resultTargetDir=~/.esy/store-3.x.x/_build/blahblah
    # Notice the _build/default/src
    ocamlMigrateParseTreeTargetDir=~/.esy/store-3.x.x/_build/blahblah/_build/default/src/


bspack needs the fully processed files with ppx already applied to them, and
jBuilder keeps them around in files like `x.pp.ml`, so you would need to create
a copied directory of the dependencies that use jBuilder(migrate parsetree
being one), and rename all the `x.pp.ml` to `x.ml` because bspack only
recognizes the `.ml` extension, not `.pp.ml` (maybe bspack should get a feature
that can prioritize `.pp.ml` since this will be very common?) For example, set
`ocamlMigrateParseTreeTargetDir` equal to that directory that has all the file
extensions fixed.

You can then run:

    bspack -bs-exclude-I ppx_deriving -bs-exclude-I ppx_deriving_show -bs-exclude-I Ppx_deriving_runtime -prelude-str "$preludeStr" -I "$menhirSuggestedLib" -bs-main Refmt_impl -I "$ppxToolsVersionedTargetDir" -I "$reasonTargetDir" -I "$reasonTargetDir/_build/src" -I "$reasonTargetDir/vendor/cmdliner" -I "$reasonParserTargetDir/_build/src" -I "$reasonParserTargetDir/vendor/easy_format" -I "$ocamlMigrateParseTreeTargetDir" -o "$origDir/refmt_main.ml"
    bspack -bs-exclude-I ppx_deriving -bs-exclude-I ppx_deriving_show -bs-exclude-I Ppx_deriving_runtime -prelude-str "$preludeStr" -I "$menhirSuggestedLib" -bs-main Reactjs_jsx_ppx -I "$ppxToolsVersionedTargetDir" -I "$reasonTargetDir" -I "$reasonTargetDir/_build/src" -I "$reasonTargetDir/vendor/cmdliner" -I "$reasonParserTargetDir/_build/src" -I "$reasonParserTargetDir/vendor/easy_format" -I "$ocamlMigrateParseTreeTargetDir" -o "$origDir/reactjs_ppx.ml"

After generating the `refmt_main` and `reactjs_ppx`, run this command: (the
`-no-alias-deps` flag is important).


    ocamlopt -no-alias-deps -linkall -I . -I +compiler-libs ocamlcommon.cmxa  "refmt_main.ml" -o "refmt_main.out"
    ocamlopt -no-alias-deps -linkall -I . -I +compiler-libs ocamlcommon.cmxa  "reactjs_ppx.ml" -o "reactjs_ppx.out"
