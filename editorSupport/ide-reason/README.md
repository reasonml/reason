# IDE-Reason

Forked from [ide-haskell](https://github.com/atom-haskell/ide-haskell)
License copied from ide-haskell.

Make sure Merlin is installed:
(I've tested with 2.1.2, so let's stick with that for now)
- `opam install merlin.2.1.2`


Make sure to install the two included included Atom plugins.

- `ide-reason`       (Autocomplete, file compilation, syntax checking, formatting)
- `language-reason`  (Syntax highlighting)

1. Create symlinks from the following locations to the corresponding directories in the `Reason` repo
  - `~/.atom/packages/ide-reason`
  - `~/.atom/packages/language-reason`

2. Run `apm install` from within each of those two plugin directories in the `Reason` repo (`ide-reason`, `language-reason`).

All of this will be "one click" from the Atom GUI when `Reason` is public. For now, this is how you must install Atom plugins from private repos.


Add this to your Atom config (accessible via the menu `Atom > Open Your Config`):
Note that you must include a config
entry for `nuclide-ocaml` - which will be automatically installed as a
dependency of `ide-reason`.

```cson
 "nuclide-ocaml":
   pathToMerlin: "/Users/you/.opam/system/bin/ocamlmerlin"
 "ide-reason":
   pathToReasonfmt: "/Users/you/.opam/system/bin/reasonfmt"
   pathToCompiler: "/usr/local/bin/ocamlc"
```


To configure keyboard shortcuts, put this in your keyboard config
(accessible via the menu `Atom > Open Your Keymap`)
This configures Command+Shift+m to automatically format your source
code.
```cson
'atom-text-editor[data-grammar~="reason"]':
  'escape': 'ide-reason:close-tooltip' #this is set by default
  'cmd-shift-m':'ide-reason:prettify-file'
  '':'ide-reason:next-error'
  '':'ide-reason:prev-error'

```

