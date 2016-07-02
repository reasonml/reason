
Installing
------------------
Read the [docs](http://facebook.github.io/reason/tools.html#merlin-atom) for installation instructions.


Upgrading Reason
------------------

- To upgrade `Reason`, you should do `opam upgrade reason`: this will repin reason, rebuilding the fresh command line tools (e.g. `refmt`, `refmttype`) which the editor plugin uses internally. Then restarting Atom will cause `atom-reason-loader` to detect the new version of `Reason` and (if needed) install the new version of the plugins.


Building and Adding Features
------------------
To build `atom-reason`, you must have `js_of_ocaml` and `jenga` (a build system) installed, in addition to `Reason`, and `ocamlmerlin`.
Open the Atom settings for `atom-reason-loader` by using the menu (`Packages > Settings View > Manage Packages`). Click on `atom-reason-loader` and uncheck the box that tells Atom to automatically "sync" the Reason plugins. Now, you can correctly have a symlink from the official Atom packages install folder to your development repo for the plugins.

```sh
opam update
opam pin add -y js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git
opam pin add -y jenga https://github.com/chenglou/jenga.git#2a0eb726f503038ad70d43f8e8bbb4c41223108a
opam install yojson
cd editorSupport/atom-reason
# Build the changes (npm install/apm link are only needed the first time)
npm install
npm start
apm link
```

**The `opam update`** step is important! In particular, for Jenga to work with our step, its dependency `core` needs to be >=113.33.03 and `js-build-tools` at >=113.33.04. If the Jenga installation fails, make sure these two packages' versions are satisfied.

`jengarootReal.re` file is used to specify the build rules for the repo. `npm start`'s script turns it into `jengaroot.ml` for Jenga's consumption. Please don't modify the ml file!
