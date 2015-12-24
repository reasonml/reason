EMACS = emacs
EMACSFLAGS =
CASK = cask
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

export EMACS

SRCS = flycheck-ocaml.el
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: compile dist test \
	clean clean-elc clean-dist clean-deps

compile : $(OBJECTS)

dist :
	$(CASK) package

deps : $(PKGDIR)

# Testing
test:
	$(CASK) exec $(EMACSBATCH) \
		-l flycheck-ocaml.el -l test/flycheck-ocaml-test.el \
		-f ert-run-tests-batch-and-exit

# Cleanup targets
clean : clean-elc clean-dist clean-deps

clean-elc :
	rm -rf $(OBJECTS)

clean-dist :
	rm -rf $(DISTDIR)

clean-deps :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<
