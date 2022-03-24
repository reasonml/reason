PACKAGE=omp_ppx_here
OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
FLAGS=-package ocaml-migrate-parsetree
TARGETS=ppx_here ppx_here.cmo ppx_here.cmx ppx_here.cmxs

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

ppx_here.cmxs: ppx_here.cmx
	$(OCAMLOPT) -o $@ -shared $^

ppx_here: ppx_here.cmx standalone.ml
	$(OCAMLOPT) $(FLAGS) -o $@ -linkpkg $^
