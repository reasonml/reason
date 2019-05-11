This subdirectory is used for packing up the entire Reason `refmt` into a single file through BuckleScript's [bspack](https://github.com/bloomberg/bucklescript/blob/master/jscomp/core/bspack_main.ml), thus discarding all intermediate steps needed to build Reason, except for the final `refmt` binary compilation.

This makes our installation much friendlier to e.g. Windows. BuckleScript currently includes the three bundles in its own repo, thus making Reason first-class (Btw, BS also uses a few other pieces of code from Reason, in its vendor/reason folder and jscomp/reason_outcome_printer).

## Build

This whole process needs to happen with OCaml 4.02.3, so make sure you have a switch for this version first:

```console
opam switch create 4.02.3
```

If you already have the proper switch, you can do

```console
opam switch 4.02.3
```

Build / install the main Reason repo. Follow the instructions in https://github.com/facebook/reason/blob/master/src/README.md#contributor-setup.

Then, install the dependencies:

```console
opam install js_of_ocaml.3.0
opam install utop menhir merlin-extend
cd .. && npm install
```

Also, have `java` installed in your system. This is needed to use closure compiler to compress the final `refmt.js`. In most cases, this means installing the [Java Development Kit](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).

Now, from the project root run:

```console
version=VERSION_NUMBER_HERE npm run prepublishOnly
```

to pack up Reason into a single file. Check the extensive comments in both `sh` files here if something goes wrong.
