
Installing
------------------
Read the [docs](https://github.com/facebook/Reason/tree/docs) for installation instructions.


Upgrading Reason
------------------

- To upgrade `Reason`, you should do `opam upgrade reason`: this will repin reason, rebuilding the fresh command line tools (e.g. `refmt`, `refmttype`) which the editor plugin uses internally. Then restarting Atom will cause `atom-reason-loader` to detect the new version of `Reason` and (if needed) install the new version of the plugins.


Building and Adding Features
------------------
To build `NuclideReason`, you must have `js_of_ocaml` installed, in addition to `Reason`, and `ocamlmerlin`.
Open the Atom settings for `atom-reason-loader` by using the menu (`Packages > Settings View > Manage Packages`). Click on `atom-reason-loader` and uncheck the box that tells Atom to automatically "sync" the Reason plugins. Now, you can correctly have a symlink from the official Atom packages install folder to your development repo for the plugins.

```sh
opam pin add -y js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git
cd editorSupport/atom-reason
# Build the changes (npm install/apm link are only needed the first time)
npm install
npm start
apm link
```
