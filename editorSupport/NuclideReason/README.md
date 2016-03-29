Supported Features
------------------

- __Auto Complete:__ Make sure your project has a .merlin file near the file you have open.

- __In-Editor Errors:__ Make sure your project has a `.merlin` file.

- __Code formatting:__ With Nuclide installed, either hit `cmd+shift_c`, or use the menu `Edit > Text > Format Code`

Installing
------------------

1. Make sure bleeding edge merlin `master` branch, and `Reason` installed via `OPAM`.
  ```sh
  opam switch 4.02.1
  opam pin add -y merlin https://github.com/the-lambda-church/merlin.git
  opam pin add -y reasonsyntax git@github.com:facebook/ReasonSyntax.git
  opam pin add -y reason git@github.com:facebook/Reason.git

  ```
2. Install `nuclide` from within Atom. (`Preferences` > `Install` > (search/install `nuclide`).
3. Install the plugins from within the `Reason` repo. Restart when finished:
```sh
# Install the basic syntax
cd Reason/editorSupport/language-reason
# Get all the dependencies.
npm install
# Make this module globally accessible for npm.
npm link
# Same, but for atom.
apm link

# Install the IDE integration
cd ../NuclideReason
npm install
# Specifically install the previously npm-linked language-reason.
npm link language-reason
apm link
```

4. Add this to your Atom config (accessible via the menu `Atom > Open Your Config`). Replace `{{yourname}}` with your login name. Also, confirm that those locations exist and are correct. Indentation matters!
```cson
"*":
  NuclideReason:
    merlinFlags: "-pp /Users/{{yourname}}/.opam/system/bin/reasonfmt_merlin"
    pathToMerlin: "/Users/{{yourname}}/.opam/system/bin/ocamlmerlin"
    pathToReasonfmt: "/Users/{{yourname}}/.opam/system/bin/reasonfmt"
```

5. Finally, open a `.re` file and select `Reason` as the current language.

Troubleshooting
------------------
Because this plugin is actively being iterated on, and because you're likely on master, here are a few tricks to keep the package's dependencies non-stale if you ever pull from master and see something broken. **Note** that you should run all the following commands in the current directory where this README resides.

- `opam upgrade reason`/`opam upgrade reasonsyntax`: this will repin reason/reasonsyntax, rebuilding the fresh command line tools (e.g. `reasonfmt`, `refmttype`) which the editor plugin uses internally.

- `rm -rf node_modules && npm install && npm link language-reason`: this will re-install all the dependencies of the plugin. `node_modules` doesn't automatically refresh itself when you build the plugin.

- `apm install`: if you see e.g. "Module version mismatch" in a red box in your editor, do this. This is Atom failing to... see some stuff.

Note that all these issues are temporary. When we release the official plugin, these would go away.

Building and Adding Features
------------------
To build `NuclideReason`, you must have `js_of_ocaml` installed, in addition to `Reason`, and `ocamlmerlin`.
```sh
opam pin add -y js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git
cd NuclideReason
# Build the changes (npm install/apm link are only needed the first time)
npm install
npm start
apm link
```
