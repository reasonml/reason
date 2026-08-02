# This file is part of the migrate-parsetree package. It is released under the
# terms of the LGPL 2.1 license (see LICENSE file).
# Copyright 2017  Frédéric Bour
#           2017  Jérémie Dimino

INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	dune build @install

.PHONY: install
install:
	dune install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	dune uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: test
test:
	dune runtest

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	dune runtest --workspace dune-workspace.dev

.PHONY: cinaps
cinaps:
	-dune build --root ../.. @src/vendored-omp/src/cinaps --auto-promote
	dune build --root ../.. @src/vendored-omp/src/cinaps

.PHONY: clean
clean:
	rm -rf _build *.install
	find . -name .merlin -delete
