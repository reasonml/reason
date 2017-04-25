Interested in contributing to our docs? It's pretty straightforward!

Editing Content & Style:
----------------

**There's no build step needed**. Open the corresponding `.html` file (e.g. `index.html`) and make the changes to the markdown embedded in the `<head>`. Refresh the page and your changes will be rendered.

`flatdoc.js` is the script that turns our inline markdown into html. `theme-white/script.js` attaches some scrolling logic. `theme-white/style.css` is our syles.


Editing the `Reason` Syntax Parser in the Docs Examples
-----------------------------------

We include a fork of `highlight.js` with support for Reason syntax. To fix/customize the parsing:

    # First make changes to the parsing code in highlightJs/src/languages/reason.js
    cd highlightJs
    npm install
    node tools/build.js -n reason ocaml bash
