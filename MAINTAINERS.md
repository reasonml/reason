## Releasing Native Packages To Npm:

There's a few native `esy` packages included which are released to npm.

- `reason`
- `rtop`

These are combined together into a separate package `reason-cli` which
prebuilds those as well as merlin. They can also be used individually from
`esy` projects without prebuilding, but they are more or less just npm hosted
versions of the Opam packages.


    git checkout -b MYRELEASE origin/master
    git rebase origin/master
    vim -O esy.json scripts/esy/esy.reason.json
    # Then edit the version number accordingly on BOTH files.
    git commit - "Bump version"
    git push origin HEAD:PullRequestForVersion
    node ./scripts/esy-prepublish.js

Then follow the printed instructions for pushing any of the packages to npm.
They will show up under `@esy-ocaml/reason` etc.

## Releasing rtop

`rtop` is also a separate `esy` package hosted on `npm`. You can
release it in the same way as you released the `reason` package


## When a new version of Reason is pushed, you might like to release some
prebuilt global installs of refmt, and merlin together in one npm instal.
`reason-cli` is the project that performs that task, and it packages up
prebuilts of the packages you already pushed to npm above.

See the [https://github.com/reasonml/reason-cli](reason-cli) page for
instructions on performing that release.

## Releasing Native Packages To Opam:

*note: it is recommended to install opam-publish via* `opam-depext -i opam-publish`

*Also, the commands below are examples based on specific Reason and rtop versions, the version numbers and possibly source urls will need to be changed to match the relevant release.

1. `cd` into a directory that you don't mind having stuff downloaded into
2. `opam-publish prepare reason.3.2.0 "https://registry.npmjs.org/@esy-ocaml/reason/-/reason-3.2.0.tgz"`
3. `opam-publish submit reason.3.2.0`
4. `opam-publish prepare rtop.3.2.0 "https://registry.npmjs.org/@esy-ocaml/reason/-/reason-3.2.0.tgz"`
5. `opam-publish rtop.3.2.0`

