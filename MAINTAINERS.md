## Releasing Native Packages To Npm:

There's a few native `esy` packages included which are released to npm.

- `@esy-ocaml/reason`
- `@esy-ocaml/rtop`

## Releasing

The Reason repo is a "monorepo" `esy` project. To actually created individual
packages from the monorepo that should be published, there is a script
`./scripts/esy-prepublish.js` which accepts the relative paths to various
`.json` files you wish to publish as individual packages. To release packages
`@esy-ocaml/reason` and `@esy-ocaml/rtop` which have json files `reason.json`
and `rtop.json` respectively in the repo root, you would run that script after
committing/bumping some versions:


```sh
git checkout -b MYRELEASE origin/master
git rebase origin/master
vim -O esy.json reason.json
# Then edit the version number accordingly on BOTH files. With that same VERSION do:
version=3.5.0 make pre_release
git commit -m "Bump version"
git push origin HEAD:PullRequestForVersion # Commit these version bumps
node ./scripts/esy-prepublish.js ./reason.json ./rtop.json

# Then publish. For example:
# cd _release/reason.json/package/
# npm publish --access=public
# cd _release/refmt.json/package/
# npm publish --access=public
```

Then follow the printed instructions for pushing any of the packages to npm.
They will show up under `@esy-ocaml/reason` etc.


## Releasing Native Packages To Opam:

*note: it is recommended to install opam-publish via* `opam-depext -i opam-publish`

*Also, the commands below are examples based on specific Reason and rtop versions, the version numbers and possibly source urls will need to be changed to match the relevant release.

1. `cd` into a directory that you don't mind having stuff downloaded into
2. `opam-publish prepare reason.3.2.0 "https://registry.npmjs.org/@esy-ocaml/reason/-/reason-3.2.0.tgz"`
3. `opam-publish submit reason.3.2.0`
4. `opam-publish prepare rtop.3.2.0 "https://registry.npmjs.org/@esy-ocaml/rtop/-/rtop-3.2.0.tgz"`
5. `opam-publish rtop.3.2.0`

## [Depracated] reason-cli
Those two Reason packages are combined together into a separate npm package
`reason-cli` which prebuilds those as well as merlin. They can also be used
individually from `esy` projects without prebuilding, but they are more or less
just npm hosted versions of the Opam packages. `reason-cli` is now no longer
necessary, as projects can/should declare `@esy-ocaml` and `@opam/merlin` as
`devDependencies` in their project's `package.json`/`esy.json`.  We may revive
a separate project just for prebuilt binaries of `rtop` without merlin.

