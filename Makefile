# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail

default: build test

build_without_utop: compile_error
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=false
	chmod +x $(shell pwd)/_build/src/refmt_merlin_impl.sh
	ln -fs $(shell pwd)/_build/src/refmt_merlin_impl.sh refmt_merlin_impl.sh

build: compile_error
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=true
	chmod +x $(shell pwd)/_build/src/refmt_merlin_impl.sh
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

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

.PHONY: release
