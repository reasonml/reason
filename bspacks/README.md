This subdirectory is used for packing up the entire Reason `refmt` into a single file through BuckleScript's [bspack](https://github.com/bloomberg/bucklescript/blob/master/jscomp/core/bspack_main.ml), thus discarding all intermediate steps needed to build Reason, except for the final `refmt` binary compilation.

This makes our installation much friendlier to e.g. Windows. BuckleScript currently includes the three bundles in its own repo, thus making Reason first-class (Btw, BS also uses a few other pieces of code from Reason, in its vendor/reason folder and jscomp/reason_outcome_printer).

## Build

You need to first run `npm install` at the root of the project to get bs-platform. This is where we get the `bspack` binary. Then, come back in this directory. 

The build step requires `ocaml-migrate-parsetree`. You can get it by running `./setupOmp.sh`.

If things go well, run `./reason_bspack.sh` to pack up Reason into a single file. If not, check the extensive comments in both `sh` files!
