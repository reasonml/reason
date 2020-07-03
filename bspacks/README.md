This subdirectory is used for packing up the entire Reason `refmt` into a
single file through BuckleScript's
[bspack](https://github.com/bloomberg/bucklescript/blob/master/jscomp/core/bspack_main.ml),
thus discarding all intermediate steps needed to build Reason, except for the
final `refmt` binary compilation.

This makes our installation much friendlier to e.g. Windows. BuckleScript
currently includes the three bundles in its own repo, thus making Reason
first-class (Btw, BS also uses a few other pieces of code from Reason, in its
vendor/reason folder and jscomp/reason_outcome_printer).

## Build (4.06 / BuckleScript v6 and above)

We use this workflow for building `build/refmt_api.ml` and
`build/refmt_binary.ml` so we can easily vendor Reason for newer BuckleScript
releases.  More details on that are in the [BuckleScript
CONTRIBUTING](https://github.com/BuckleScript/CONTRIBUTING.md) file.

For inlining the right version number in the bundle, the script uses
`../reason.json` as the source of truth.

**Note:** Currently you will need to build BuckleScript yourself to get access
to the `bspack.exe` executable. Also we skip the building of the `refmt.js`
artifact entirely here. Will maybe added back later as soon as we need it!

**Instructions:**

```
cd bspacks

opam switch 4.06.1

./downloadSomeDependencies.sh

# Point to your locally built bspack.exe
BSPACK_EXE=~/Projects/bucklescript/jscomp/bin/bspack.exe ./reason_bspack406.sh
```

The bspacked files are also compiled to make sure that the bundle actually
compiles. You should then find all the relevant `.ml` files in the `./build`
directory, ready to be copied over to BuckleScript.

## Legacy Build (4.02 / BuckleScript v5 and below)

> This is an old workflow which also seem to be broken since `reerror` was
> merged. We discourage the use, unless you really need to build bspacked
> bundles for 4.02 based BuckleScript versions

This whole process needs to happen with OCaml 4.02.3, so make sure you switch
to that version first:

```sh
# Then switch to the right ocaml version and install all deps
opam switch 4.02.3
```

Build / install the main Reason repo. Follow the instructions in https://github.com/facebook/reason/blob/master/docs/GETTING_STARTED_CONTRIBUTING.md#contributor-setup.

Then, install the dependencies:

```sh
opam install js_of_ocaml.3.0
opam install utop
cd .. && npm install
```

Also, have `java` installed in your system. This is needed to use closure compiler to compress the final `refmt.js`. In most cases, this means installing the [Java Development Kit](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).

Now, from the project root run:

```sh
version=VERSION_NUMBER_HERE npm run prepublishOnly
```

to pack up Reason into a single file. Check the extensive comments in both `sh` files here if something goes wrong.
