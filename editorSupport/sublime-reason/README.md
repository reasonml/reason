# Sublime Text package for Reason - based directly on sublime-rust

https://github.com/jhasse/sublime-rust


## Installation (ST3)

When Reason is public, this plugin can go on Package Control.

For now, you must copy this plugin into your Sublime Packag directory. Anytime
you upgrad/reclone/rebuild Reason, you should make sure to upgrade this Sublime
plugin as well - language syntax and this plugin will be evolved together.

## Configuration

In your Sublime Text preferances (`Cmd+,` on Mac) you can add

```json
"refmt_bin": "/Users/frantic/.opam/4.02.1/bin/refmt",
"reason_max_width": 80,
```

When working on Reason itself, it's useful to point `refmt_bin` to your
local development version (usually `/path/to/reason/refmt_impl.native`).

## Development

The files are written in the JSON format supported by the Sublime Text
package [AAAPackageDev](https://github.com/SublimeText/AAAPackageDev),
because the format is much easier to read / edit
than the xml based plist format.

So install that package and then work on the .JSON-* files. There is a
build system that comes with that package, so if everything is set up
right, you should just be able to trigger the build (F7) and get the
corresponding .tmLanguage / .tmPreferences files. It will also display
errors if you have not formatted the file correctly.

One impact of using this indirect format is that you usually have to double
escape anything in the match patterns, ie, "\\(" has to be "\\\\(" as otherwise
it will try to interpret '\\(' as a JSON escape code (which doesn't exist).

## Credits

Created 2012 by [Daniel Patterson](mailto:dbp@riseup.net), as a near complete from
scratch rewrite of a package by [Felix Martini](https://github.com/fmartini).

Derived primarily from the Vim syntax file, maintained by
[Patrick Walton](https://github.com/pcwalton) and
[Ben Blum](https://github.com/bblum)

With a little help from the (now very outdated) TextMate rust mode written
by [Tom Ellis](https://github.com/tomgrohl).

## License

This package is licensed under the MIT License.
