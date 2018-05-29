# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build

build:
	jbuilder build

install:
	opam pin add reason . -y

# CI uses opam. Regular workflow needn't.
test-ci: install test

test: build clean-tests
	node ./formatTest/testOprint.js
	# ./miscTests/rtopIntegrationTest.sh
	./miscTests/jsxPpxTest.sh
	cd formatTest; ./test.sh

.PHONY: coverage
coverage:
	find -iname "bisect*.out" -exec rm {} \;
	make test
	bisect-ppx-report -ignore-missing-files -I _build/ -html coverage-after/ bisect*.out ./*/*/*/bisect*.out
	find -iname "bisect*.out" -exec rm {} \;

clean-tests:
	rm -rf ./formatTest/**/actual_output
	rm -f ./formatTest/failed_tests
	rm -f ./miscTests/reactjs_jsx_ppx_tests/*.cm*

testFormat: build clean-tests
	cd formatTest; ./test.sh

genMessages:
	menhir --explain --strict --unused-tokens src/reason-parser/reason_parser.mly --list-errors > src/reason-parser/reason_parser.messages.checked-in

explain:
	menhir --explain src/reason-parser/reason_parser.mly

# Not all versions of jbuilder have the clean command.
# jbuilder clean
clean: clean-tests
	jbuilder clean

# The jbuilder that gets installed on CI doesn't have the make clean
# command yet.
clean-for-ci: clean-tests
	rm -rf ./_build

.PHONY: build clean

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUBSTS:=$(ROOT_DIR)/pkg/substs

# For publishing esy releases to npm
esy-prepublish: build clean-tests pre_release
	node ./scripts/esy-prepublish.js

# For OPAM
pre_release:
ifndef version
	$(error environment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) $(ROOT_DIR)/src/refmt/package.ml.in

.PHONY: pre_release

# For OPAM
release_check:
	./scripts/release-check.sh

# For OPAM
release: release_check pre_release
	git add package.json src/refmt/package.ml reason.opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	# Push first the objects, then the tag.
	git push "git@github.com:facebook/Reason.git"
	git push "git@github.com:facebook/Reason.git" tag $(version)
	git clean -fdx
	./scripts/opam-release.sh

.PHONY: release

all-supported-ocaml-versions:
# the --dev flag has been omitted here but should be re-introduced eventually
	jbuilder build @install @runtest --workspace jbuild-workspace.dev --root .

.PHONY: all-supported-ocaml-versions
