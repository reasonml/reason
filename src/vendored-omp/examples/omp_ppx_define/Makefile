PACKAGE=omp_ppx_define
OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
FLAGS=-package ocaml-migrate-parsetree
TARGETS=ppx_define ppx_define.cmo ppx_define.cmx ppx_define.cmxs

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

ppx_define.cmxs: ppx_define.cmx
	$(OCAMLOPT) -o $@ -shared $^

ppx_define: ppx_define.cmx standalone.ml
	$(OCAMLOPT) $(FLAGS) -o $@ -linkpkg $^

test: ppx_define
	@echo "(* Original file: cat test.ml *)"
	@cat test.ml
	@echo "(* Substituted file: ./ppx_define -D 'var="hello"' test.ml *)"
	@./ppx_define -D 'var="hello"' test.ml
