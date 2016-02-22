Supported Features
------------------

- __Auto Complete:__ Make sure your project has a .merlin file near the file you have open.

- __In-Editor Errors:__ Make sure your project has a `.merlin` file.

- __Code formatting:__ With Nuclide installed, either hit `cmd+shift_c`, or use the menu `Edit > Text > Format Code`

Installing
------------------

Currently, only works with bleeding edge merlins/patches that have not
yet been checked into merlin/master.

1. Make sure bleeding edge merlin, and `Reason` installed via `OPAM`.
  ```sh
  opam switch 4.02.1
  opam install merlin
  opam pin add -y reasonsyntax git@github.com:facebook/ReasonSyntax.git
  opam pin add -y reason git@github.com:facebook/Reason.git

  ```
2. Install `nuclide` from within Atom. (`Preferences` > `Install` > (search/install `nuclide`).
3. Install the plugins from within the `Reason` repo. Restart when finished:
```sh
# Install the basic syntax
cd Reason/editorSupport/language-reason
npm install
apm link
# Install the IDE integration
cd ../NuclideReason
npm install
apm link
```

4. Add this to your Atom config (accessible via the menu `Atom > Open Your Config`). Replace `user` with your login name. Also, confirm that those locations exist and are correct. Indentation matters.
```cson
"*":
  NuclideReason:
    pathToCompiler: "/usr/local/bin/ocamlc"
    pathToReasonfmt: "/Users/user/.opam/system/bin/reasonfmt"

  nuclide:
    "nuclide-ocaml":
      pathToMerlin: "/Users/user/github/merlin/ocamlmerlin"
```

5. Finally, open a `.re` file and select `Reason` as the current language.

Building and Adding Features
------------------
To build `NuclideReason`, you must have `js_of_ocaml` installed, in addition to `Reason`, and `ocamlmerlin`.
```sh
opam install js_of_ocaml
cd NuclideReason
# Build the changes.
npm install
npm start
apm link
```
