# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build test

setup_convenient_bin_links:
	mkdir -p $(shell pwd)/_build/bin
	ln -fs $(shell pwd)/_build/src/refmt_impl.native $(shell pwd)/_build/bin/refmt
	ln -fs $(shell pwd)/_build/_reasonbuild/_build/myocamlbuild $(shell pwd)/_build/bin/rebuild
	ln -fs $(shell pwd)/_build/src/ocamlmerlin_reason.native $(shell pwd)/_build/bin/ocamlmerlin-reason
	ln -fs $(shell pwd)/_build/src/reason_format_type.native $(shell pwd)/_build/bin/refmttype
	ln -fs $(shell pwd)/_build/src/refmt_impl.native $(shell pwd)/_build/bin/refmt
	ln -fs $(shell pwd)/_build/src/share.sh $(shell pwd)/_build/bin/share

precompile:
	cp pkg/META.in pkg/META
	ocamlbuild -use-ocamlfind -package topkg pkg/build.native

build_without_utop: compile_error setup_convenient_bin_links precompile
	./build.native build --utop false
	chmod +x $(shell pwd)/_build/src/*.sh

build: compile_error setup_convenient_bin_links precompile
	./build.native build --utop true
	chmod +x $(shell pwd)/_build/src/*.sh

install:
	opam pin add reason . -y
	./refmt_impl.native --help=groff > $(shell opam config var man)/man1/refmt.1

test: build clean-tests
	# ./miscTests/rtopIntegrationTest.sh
	./miscTests/jsxPpxTest.sh
	cd formatTest; ./test.sh

clean-tests:
	rm -rf ./formatTest/**/actual_output
	rm -f ./formatTest/failed_tests
	rm -f ./miscTests/reactjs_jsx_ppx_tests/*.cm*

clean: clean-tests
	ocamlbuild -clean

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
	$(SUBSTS) $(ROOT_DIR)/opam.in

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

.PHONY: update_error compile_error
