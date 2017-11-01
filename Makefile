# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build

build:
	jbuilder build

install:
	opam pin add reason . -y

test: build clean-tests
	# I don't have modern enough node to test. brb.
	# node ./formatTest/testOprint.js
	opam pin add -y reason .
	./miscTests/rtopIntegrationTest.sh
	./miscTests/jsxPpxTest.sh
	cd formatTest; ./test.sh

clean-tests:
	rm -rf ./formatTest/**/actual_output
	rm -f ./formatTest/failed_tests
	rm -f ./miscTests/reactjs_jsx_ppx_tests/*.cm*

# Not all versions of jbuilder have the clean command.
# jbuilder clean
clean: clean-tests
	rm -rf ./_build

.PHONY: build clean

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUBSTS:=$(ROOT_DIR)/pkg/substs

pre_release:
ifndef version
	$(error environment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) $(ROOT_DIR)/package.ml.in; \
	$(SUBSTS) $(ROOT_DIR)/reason.opam.in

.PHONY: pre_release

release_check:
	./scripts/release-check.sh

release: release_check pre_release
	git add package.json package.ml opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	# Push first the objects, then the tag.
	git push "git@github.com:facebook/Reason.git"
	git push "git@github.com:facebook/Reason.git" tag $(version)
	git clean -fdx
	./scripts/opam-release.sh

.PHONY: release
