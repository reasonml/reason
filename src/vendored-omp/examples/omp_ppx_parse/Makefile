PACKAGE=ppx_parse
OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
FLAGS=-package ocaml-migrate-parsetree
TARGETS=ppx_parse ppx_parse.cmo ppx_parse.cmx ppx_parse.cmxs

all: build
	
clean:
	rm -f *.o *.cm* $(TARGETS)

build: $(TARGETS)

install: build
	ocamlfind install $(PACKAGE) META $(TARGETS)

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

%.cmo: %.ml
	$(OCAMLC) $(FLAGS) -c $^

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c $^

ppx_parse.cmxs: ppx_parse.cmx
	$(OCAMLOPT) -o $@ -shared $^

ppx_parse: ppx_parse.cmx standalone.ml
	$(OCAMLOPT) $(FLAGS) -o $@ -linkpkg $^
