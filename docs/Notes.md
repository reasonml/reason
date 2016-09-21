Developer notes
===============

### Distributions

There are 4 official ways to point to flatdoc releases. That is:

    # via cdnjs:
    https://cdn.rawgit.com/rstacruz/flatdoc/v0.8.6/flatdoc.js

    # Specific versions
    # (built using `make v/0.8.1`)
    http://rstacruz.github.io/flatdoc/v/0.8.6/*

    # Latest from the 0.8 series.
    # Updated every time there's a 0.8 release.
    # (built using `make v/0.8`)
    http://rstacruz.github.io/flatdoc/v/0.8/*

    # Latest
    # (built using `make`)
    http://rstacruz.github.io/flatdoc/*

...just replace `*` with `flatdoc.js` (or any of the other support files).

### GNU make

This project uses GNU make (`Makefile`) to manage the builds.

 * `make` - builds the latest version
 * `make v/0.x.x` - builds a given version distribution
 * `make watch` - keep rebuilding as needed (useful for development)

### Notes on structure

 * `flatdoc.js` is the main script.
 * Distributions are stored in `/v/{version}/*`.
 * CSS files are compiled from Stylus sources.

### Updating files

    # build files
    make

    # ensure it works, and install build tools
    npm install
    npm test

    # update prescribed versions
    perl -p -i -e "s/v\d+\.\d+\.\d+/v0.9.0/g" templates/*.html Readme.md
    bump package.json
    git diff

    # build files
    make
    make v/0.9
    make v/0.9.0

    # add release date
    vim History.md
    git release v0.8.3
    npm publish
