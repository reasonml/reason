# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build test

setup_convenient_bin_links:
	mkdir -p $(shell pwd)/_build/bin
	ln -fs $(shell pwd)/_build/src/refmt_impl.native $(shell pwd)/_build/bin/refmt
	ln -fs $(shell pwd)/_build/_reasonbuild/_build/myocamlbuild $(shell pwd)/_build/bin/reasonbuild
	ln -fs $(shell pwd)/_build/src/ocamlmerlin_reason.native $(shell pwd)/_build/bin/ocamlmerlin-reason
	ln -fs $(shell pwd)/_build/src/reason_error_reporter.native $(shell pwd)/_build/bin/refmterr
	ln -fs $(shell pwd)/_build/src/reason_format_type.native $(shell pwd)/_build/bin/refmttype
	ln -fs $(shell pwd)/_build/src/rebuild.sh $(shell pwd)/_build/bin/rebuild
	ln -fs $(shell pwd)/_build/src/redoc.sh $(shell pwd)/_build/bin/redoc
	ln -fs $(shell pwd)/_build/src/refmt_impl.native $(shell pwd)/_build/bin/refmt
	ln -fs $(shell pwd)/_build/src/refmt_merlin_impl.sh $(shell pwd)/_build/bin/refmt_merlin
	ln -fs $(shell pwd)/_build/src/reopt.sh $(shell pwd)/_build/bin/reopt
	ln -fs $(shell pwd)/_build/src/rec.sh $(shell pwd)/_build/bin/rec
	ln -fs $(shell pwd)/_build/src/share.sh $(shell pwd)/_build/bin/share
	ln -fs $(shell pwd)/_build/src/reup.sh $(shell pwd)/_build/bin/reup

build_without_utop: compile_error setup_convenient_bin_links
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=false
	chmod +x $(shell pwd)/_build/src/*.sh
	ln -fs $(shell pwd)/_build/src/refmt_merlin_impl.sh refmt_merlin_impl.sh

build: compile_error setup_convenient_bin_links
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=true
	chmod +x $(shell pwd)/_build/src/*.sh
	ln -fs $(shell pwd)/_build/src/refmt_merlin_impl.sh refmt_merlin_impl.sh

install:
	opam pin add reason . -y

run: build
	rlwrap ocaml \
		$(shell ocamlfind query -predicates byte,toploop -r -a-format \
		                        findlib compiler-libs.common unix) \
		_build/src/reason.cma _build/src/reason_toploop.cmo

run_utop: build
	utop \
		$(shell ocamlfind query -predicates byte,toploop -r -a-format \
		                        compiler-libs.common) \
		_build/src/reason.cma _build/src/reason_utop.cmo

test: build
	./miscTests/rtopIntegrationTest.sh
	./miscTests/jsxPpxTest.sh
	cd formatTest; ./test.sh

clean:
	ocamlbuild -clean

# Compile error messages into ml file, checks if the error messages are complete and not redundent
compile_error: update_error
	menhir --explain --strict --unused-tokens src/reason_parser.mly --compile-errors src/reason_parser.messages > src/reason_parser_message.ml

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

release:
ifndef version
	$(error enviorment variable 'version' is undefined)
endif
	$(SUBSTS) $(ROOT_DIR)/package.json.in
	$(SUBSTS) $(ROOT_DIR)/opam.in
	git add package.json opam
	git commit -m "Version $(version)"
	git tag -a $(version) -m "Version $(version)."
	git push "git@github.com:facebook/Reason.git" $(version)

.PHONY: release
