This subdirectory is used for packing up the entire Reason `refmt` into a single file through BuckleScript's [bspack](https://github.com/bloomberg/bucklescript/blob/master/jscomp/core/bspack_main.ml), thus discarding all intermediate steps needed to build Reason, except for the final `refmt` binary compilation.

This makes our installation much friendlier to e.g. Windows. BuckleScript currently includes the three bundles in its own repo, thus making Reason first-class (Btw, BS also uses a few other pieces of code from Reason, in its vendor/reason folder and jscomp/reason_outcome_printer).

## Build

First, install the dependencies:

```sh
opam install js_of_ocaml.3.0
cd .. && npm install
```

Also, have `java` installed in your system. This is needed to use closure compiler to compress the final `refmt.js`.

Now, go back to project root and run:

```sh
version=VERSION_NUMBER_HERE npm run prepublishOnly
```

to pack up Reason into a single file. Check the extensive comments in both `sh` files here if something goes wrong.
