This subdirectory is used for packing up the entire Reason `refmt` into a single file through BuckleScript's [bspack](https://github.com/bloomberg/bucklescript/blob/master/jscomp/core/bspack_main.ml), thus discarding all intermediate steps needed to build Reason, except for the final `refmt` binary compilation. Likewise for `reactjs_jsx_ppx`.

This makes our installation much friendlier to e.g. Windows. BuckleScript currently includes the two bundles in its own repo, thus making Reason first-class.

## Build

`./reason_bspack.sh`
