# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build

build:
	dune build

install:
	opam pin add reason . -y

# CI uses opam. Regular workflow needn't.
test-ci: install test-once-installed

test-once-installed: test

test:
	esy dune runtest

test-watch:
	esy dune runtest --watch

.PHONY: coverage
coverage:
	find -iname "bisect*.out" -exec rm {} \;
	make test-once-installed
	bisect-ppx-report -ignore-missing-files -I _build/ -html coverage-after/ bisect*.out ./*/*/*/bisect*.out
	find -iname "bisect*.out" -exec rm {} \;

testFormat: build test-once-installed

all_errors:
	@ echo "Regenerate all the possible error states for Menhir."
	@ echo "Warning: This will take a while and use a lot of CPU and memory."
	@ echo "---"
	menhir --explain --strict --unused-tokens src/reason-parser/reason_parser.mly --list-errors > src/reason-parser/reason_parser.messages.checked-in

clean:
	dune clean

clean-for-ci:
	rm -rf ./_build

.PHONY: build clean

# For publishing esy releases to npm
esy-prepublish: build
	node ./scripts/esy-prepublish.js

# For OPAM
release_check:
	./scripts/release-check.sh

# For OPAM
release: release_check
	git add package.json src/refmt/package.ml reason.opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	# Push first the objects, then the tag.
	git push "git@github.com:reasonml/reason.git"
	git push "git@github.com:reasonml/reason.git" tag $(version)
	git clean -fdx
	./scripts/opam-release.sh

.PHONY: release

all-supported-ocaml-versions:
# the --dev flag has been omitted here but should be re-introduced eventually
	dune build @install @runtest --root .

.PHONY: all-supported-ocaml-versions

pprint_test: 
	ocamlformat src/reason-parser/reason_pprint_ast_pprint.ml -i
	dune exec src/refmt/refmt_impl.exe -- test/basicStructures.t/input.re --parse re --print ast > test/basicStructures.t/run.t.ast
	dune exec src/refmt/refmt_impl.exe -- test/basicStructures.t/input.re --parse re --print re > test/basicStructures.t/run.t.new

doc:
	esy dune build @doc

.PHONY: doc
