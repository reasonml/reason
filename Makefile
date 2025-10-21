# Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

SHELL=bash -o pipefail
ESY-DUNE=esy dune
OPAM-DUNE=opam exec -- dune

# Use dune from opam if _opam folder exists, otherwise use dune from esy
ifneq ($(wildcard _opam),)
  DUNE=$(OPAM-DUNE)
else
  DUNE=$(ESY-DUNE)
endif

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

default: help

.PHONY: build
build: ## Build the project
	$(DUNE) build

.PHONY: pin-reason
pin-reason:
	opam pin add reason . -y

.PHONY: test
test: ## Run the test suite
	$(DUNE) runtest

.PHONY: test-watch
test-watch: ## Run the test suite in watch mode
	$(DUNE) runtest --watch

.PHONY: coverage
coverage: ## Run the test suite and generate coverage report
	find -iname "bisect*.out" -exec rm {} \;
	make test
	bisect-ppx-report -ignore-missing-files -I _build/ -html coverage-after/ bisect*.out ./*/*/*/bisect*.out
	find -iname "bisect*.out" -exec rm {} \;

.PHONY: menhir-all-errors
menhir-all-errors:
	@ echo "Regenerate all the possible error states for Menhir."
	@ echo "Warning: This will take a while and use a lot of CPU and memory."
	@ echo "---"
	menhir --explain --strict --unused-tokens src/reason-parser/reason_parser.mly --list-errors > src/reason-parser/reason_parser.messages.checked-in

.PHONY: clean
clean: ## Clean the project
	dune clean

.PHONY: clean-for-ci
clean-for-ci: ## Clean the project by removing the _build directory
	rm -rf ./_build

.PHONY: esy-prepublish
esy-prepublish: build ## For publishing esy releases to npm
	node ./scripts/esy-prepublish.js

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
# the --dev flag has been omitted here but should be re-introduced eventually
	dune build @install @runtest --root .

.PHONY: doc
doc: ## Build the documentation
	$(DUNE) build @doc

.PHONY: opam-create-switch
opam-create-switch: ## Create opam switch
	opam switch create . 5.2.0 --deps-only --with-test --no-install

.PHONY: opam-install
opam-install: ## Install project dependencies
	opam install . --deps-only --with-test --with-dev-setup --working-dir . -y
