Reason: Friendly Syntax & Toolchain for OCaml [![Build Status](https://travis-ci.org/facebook/reason.svg?branch=master)](https://travis-ci.org/facebook/reason) [![CircleCI](https://circleci.com/gh/facebook/reason/tree/master.svg?style=svg)](https://circleci.com/gh/facebook/reason/tree/master)
=========================================

Getting Started
---------------

[JS Workflow](http://facebook.github.io/reason/jsWorkflow.html)
[Native Workflow](http://facebook.github.io/reason/nativeWorkflow.html)

Community
=======

Come say hi!

- [Discord](https://discord.gg/reasonml)
- [Twitter](https://twitter.com/reasonml)
- [Stack Overflow](http://stackoverflow.com/questions/tagged/reason)
- IRC (freenode #reasonml)

We also maintain [BuckleTypes](https://github.com/BuckleTypes), where community members submit Reason/OCaml JavaScript bindings.


Contributing
---------------

### Documentations

The docs are in the [docs folder](https://github.com/facebook/reason/tree/master/docs) and the corresponding README there that describes the docs contribution method.

### Codebase

See the [src folder](ttps://github.com/facebook/reason/tree/master/src) and the corresponding README.

```
# On OSX, install opam via Homebrew:
brew update
brew install opam
# On Linux, see here (you will need opam >= 1.2.2): http://opam.ocaml.org/doc/Install.html

opam init
# Add this to your ~/.bashrc (or ~/.zshrc):
#   eval $(opam config env)

opam update
opam switch 4.03.0
eval $(opam config env)
git clone git@github.com:facebook/reason.git
cd reason
opam pin add -y reason-parser reason-parser
opam pin add -y reason .
```

**Note**: during the last `opam pin` step, make sure your local repo is clean. In particular, remove artifacts and `node_modules`. Otherwise the pinning might go stale or stall due to the big `node_modules`.

License
-------

See Reason license in [LICENSE.txt](LICENSE.txt).

Works that are forked from other projects are under their original licenses.

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.


Credit
-------
The general structure of `refmt` repo was copied from @whitequark's m17n
project, including parts of the `README` that instruct how to use this with the
OPAM toolchain.
