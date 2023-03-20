# refmt cram test suite

This folder contains dune's cram tests.

More information on how they work and how to create them in [dune's documentation](https://dune.readthedocs.io/en/stable/tests.html#cram-tests).

## Run them locally use

```bash
esy dune runtest
# or in watch mode
esy dune runtest -w
```

## Update snapshot

The usual workflow is to run the test once, let it fail and then update the snapshot with the output you expect with promotion:

```bash
esy dune promote
```

## Run only one test

```bash
esy dune build @cram-test-name
# for example: esy dune build @assert
```

## Update only one snapshot

```bash
esy dune build @cram-test-name --auto-promote
# for example: esy dune build @assert
```

## Testing a specific version of OCaml

Some tests are specifically designed to test a specific version of OCaml. To run them, you have a few options:

### Install an specific OCaml version with esy

Make sure the OCaml's version is between the range of the `ocaml` field in esy.json (`>= 4.2.0 < 4.15.0`).

1. Install the specific version you want with: `esy add ocaml@{{ ocaml_version }}`
2. Run the test with: `esy dune runtest`

### Setup the local enviroment with opam

The opam workflow is only tested in CI and isn't needed for development, but can become handy since you can install many switches with different versions of OCaml and load them when needed.

Using 4.12 as an example:

1. Create the switch: `opam switch create reason-dev-4.12 -y --deps-only --with-test 4.12.0`
2. Install dependencies (this only needs to be done once) `opam install . --deps-only --with-test`
3. Load env variables for your switch `eval $(opam env --switch=reason-dev-4.12 --set-switch)`
4. Run the test with: `opam exec -- dune runtest`
