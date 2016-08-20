ReasonDocs
==========
Docs for [Reason](http://github.com/facebook/Reason).

- Custom syntax highlighting for `Reason` `.re` examples.
- No build script required to make changes to content or style. Just reload.

Editing Content:
----------------

To edit, just open the corresponding `.html` file (such as `index.html`)
and make the changes to the markdown embedded in the `<head>`. Refresh the
page and your changes are rendered.


Editing Style:
--------------
These docs were forked from the `Flatdoc` project, which uses build
systems to generate styles, but these docs merely edited those generated
`.css` builds manually instead of changing the original `.styl` files.

- To make changes to the core styles, edit `theme-white/style.styl` and then run `make`.
- To edit the additional page styles, change `support/theme.css`.
Then reload the page.

Changing the `Reason` syntax parser
-----------------------------------
To change the parser for `.re` source examples. `ReasonDocs` includes a custom
fork of `highlight.js` with support for `.re` syntax. To fix/customize the
parsing

    # First make changes to the parsing code in highlightJs/src/languages/reason.js
    cd highlightJs
    npm install
    node tools/build.js -n reason ocaml bash


Creating a minimal archive:
-----------------------------------
Run a local server and run `wget`. This will automatically pull a minimum
set of files suitable for deployment.

      wget -mpck --user-agent="" -e robots=off http://localhost:8000/index.html
