# refmt cram test suite

This folder contains cram tests from dune.

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

More information on how they work and how to create them in [dune's documentation](https://dune.readthedocs.io/en/stable/tests.html#cram-tests)
