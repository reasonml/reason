# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true utop=true
	chmod +x $(shell pwd)/_build/src/reasonfmt_merlin_impl.sh
	ln -fs $(shell pwd)/_build/src/reasonfmt_merlin_impl.sh reasonfmt_merlin_impl.sh

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

.PHONY: build clean

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
