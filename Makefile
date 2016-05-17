UGLIFY := ./node_modules/.bin/uglifyjs --comments "/^!/"
STYLUS := ./node_modules/.bin/stylus -U -u nib
DOX := ./node_modules/.bin/dox

all: \
	legacy.js \
	theme-white/style.css \
	theme-white/script.js \
	Reference.md

watch:
	while true; do make all | grep -v "Nothing"; sleep 1; done

# Legacy shims for IE
legacy.js: \
	support/legacy-header.js \
	support/vendor/html5shiv.js \
	support/vendor/respond.js
	cat $^ > $@

%.css: %.styl
	(echo "/*\n\nPlease don't edit this file directly.\nInstead, edit the stylus (.styl) files and compile it to CSS on your machine.\n\n*/" ; $(STYLUS) < $<) > $@

Reference.md: flatdoc.js
	$(DOX) -r < $< | node support/dox2md.js --default-level 3 > $@

# $ make v/0.1.0
# Makes a distribution.
#
v/%: all
	mkdir -p $@
	$(UGLIFY) < flatdoc.js > $@/flatdoc.js
	$(UGLIFY) < legacy.js > $@/legacy.js
	cp -R templates $@/templates
	mkdir -p $@/theme-white
	cp theme-white/style.css $@/theme-white
	cp theme-white/script.js $@/theme-white

.PHONY: watch
