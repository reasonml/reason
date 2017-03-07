# This file is part of the ppx_tools_versioned package. It is released under
# the terms of the LGPL 2.1 license (see LICENSE file).
# Copyright 2017 Frédéric Bour.
#
# It is forked from ppx_tools package, which is copyright 2013
# Alain Frisch and LexiFi.

# Don't forget to change META file as well
PACKAGE = ppx_tools_versioned
VERSION = 5.0

# Config
include $(shell ocamlc -where)/Makefile.config
OCAML_VERSION=$(shell ./ast_version.sh ocamlc)

PACKS = ocaml-migrate-parsetree
OCAMLC = ocamlfind c -package $(PACKS)
OCAMLOPT = ocamlfind opt -package $(PACKS)
COMPFLAGS = -bin-annot -w +A-4-17-44-45-105-42 -safe-string

MODULES = ast_convenience ast_mapper_class ppx_tools
VERSIONS = 402 403 404 405

# Files
OBJECTS= $(foreach version,$(VERSIONS), \
					 $(foreach module,$(MODULES), $(module)_$(version).cmo))

METAQUOTS= $(foreach version,$(VERSIONS),ppx_metaquot_$(version))
.PHONY: all
all: ppx_tools_versioned.cma ppx_tools_versioned.cmxa $(METAQUOTS)

$(METAQUOTS):

ppx_metaquots: $(METAQUOTS)

ifeq ($(NATDYNLINK),true)
all: ppx_tools_versioned.cmxs
endif

.PHONY: clean
clean:
	rm -f *.cm* *.o *.obj *.a *.lib $(METAQUOTS)

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx .native

%.cmo: %.ml
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(COMPFLAGS) -c $<

# Install/uninstall

targets = $(1).mli $(1).cmi $(1).cmt $(1).cmti $(wildcard $(1).cmx)
INSTALL = META \
	$(wildcard ppx_tools_versioned.*) \
	$(OBJECTS:.cmo=.cmi) $(wildcard $(OBJECTS:.cmo=.cmx)) \
	$(wildcard $(OBJECTS:.cmo=.cmt) $(OBJECTS:.cmo=.cmti)) \
	$(METAQUOTS)

.PHONY: reinstall install uninstall

install:
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

# Ast selection

ppx_tools_versioned.cma: $(OBJECTS)
	$(OCAMLC) -a -o $@ $^

ppx_tools_versioned.cmxa: $(OBJECTS:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $^

ppx_tools_versioned.cmxs: $(OBJECTS:.cmo=.cmx)
	$(OCAMLOPT) -shared -o $@ $^

# Metaquot

ppx_metaquot_%: ppx_tools_versioned.cmxa ast_lifter_%.ml ppx_metaquot_%.ml
	$(OCAMLOPT) -o $@ $(COMPFLAGS) -linkpkg $^

.PHONY: depend
depend:
	ocamldep *.ml *.mli > .depend
	dos2unix .depend
-include .depend
