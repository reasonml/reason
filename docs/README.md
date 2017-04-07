Interested in contributing to our docs? It's pretty straightforward!

Editing Content:
----------------

**There's no build step needed**. Open the corresponding `.html` file (e.g.
`index.html`) and make the changes to the markdown embedded in the `<head>`.
Refresh the page and your changes will be rendered.

Warning about editing code example tables: ensure there are no empty lines
inside the tables. Empty lines will cause formatting errors.

Editing Style:
--------------

These docs were forked from the `Flatdoc` project, which uses build
systems to generate styles, but these docs merely edited those generated
`.css` builds manually instead of changing the original `.styl` files.

- From the root of the `docs` directory, run `npm install`.
- To make changes to the core styles, edit `theme-white/style.styl` and then run `make`.
- To edit the additional page styles, change `support/theme.css`. Then reload the page.

Editing the `Reason` Syntax Parser in the Docs Examples
-----------------------------------

We include a custom fork of `highlight.js` with support for `.re` syntax. To
fix/customize the parsing:

    # First make changes to the parsing code in highlightJs/src/languages/reason.js
    cd highlightJs
    npm install
    node tools/build.js -n reason ocaml bash
