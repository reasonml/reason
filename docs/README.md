Interested in contributing to our docs? It's pretty straightforward!

Editing Content & Style:
----------------

**There's no build step needed**. Open the corresponding `.html` file (e.g.
`index.html`) and make the changes to the markdown embedded in the `<head>`.
Refresh the page and your changes will be rendered.

There's `support/theme.css` and `theme-white/style.css` for styling.


Editing the `Reason` Syntax Parser in the Docs Examples
-----------------------------------

We include a custom fork of `highlight.js` with support for `.re` syntax. To
fix/customize the parsing:

    # First make changes to the parsing code in highlightJs/src/languages/reason.js
    cd highlightJs
    npm install
    node tools/build.js -n reason ocaml bash
