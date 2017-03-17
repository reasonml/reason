# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build

setup_convenient_bin_links:
	mkdir -p $(shell pwd)/_build/bin

precompile:
	cp pkg/META.in pkg/META
	ocamlbuild -package topkg pkg/build.native

build: compile_error setup_convenient_bin_links precompile
	./build.native build

clean:
	ocamlbuild -clean

# Compile error messages into ml file, checks if the error messages are complete and not redundent
compile_error: update_error
	menhir --explain --strict --unused-tokens src/reason_parser.mly --compile-errors src/reason_parser.messages > src/reason_parser_message.ml

all_errors:
	@ echo "Regenerate all the possible error states for Menhir."
	@ echo "Warning: This will take a while and use a lot of CPU and memory."
	@ echo "---"
	menhir --explain --strict --unused-tokens src/reason_parser.mly --list-errors > src/reason_parser.all.messages

# Update error messages based on new grammar
update_error:
	@ cp -f src/reason_parser.messages src/reason_parser.messages.bak
	@ if ! menhir --explain --strict --unused-tokens src/reason_parser.mly --update-errors src/reason_parser.messages.bak | sed -e 's/[[:space:]]*$$//g' > src/reason_parser.messages ; then \
		cp src/reason_parser.messages.bak src/reason_parser.messages ; \
		exit 1 ; \
	fi
	@ echo "The auto-generated comments in src/reason_parser.messages have been re-generated. The old messages file has been backed up at src/reason_parser.messages.bak"

.PHONY: build clean update_error compile_error

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
SUBSTS:=$(ROOT_DIR)/pkg/substs

pre_release:
ifndef version
	$(error enviorment variable 'version' is undefined)
endif
	export git_version="$(shell git rev-parse --verify HEAD)"; \
	export git_short_version="$(shell git rev-parse --short HEAD)"; \
	$(SUBSTS) $(ROOT_DIR)/package.json.in; \
	$(SUBSTS) $(ROOT_DIR)/package.ml.in; \
	$(SUBSTS) $(ROOT_DIR)/opam.in

.PHONY: pre_release

release_check:
	./scripts/release-check.sh

release: release_check pre_release
	git add package.json package.ml opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	git push "git@github.com:facebook/Reason.git" tag $(version)
	git clean -fdx
	npm publish --access public

.PHONY: release
