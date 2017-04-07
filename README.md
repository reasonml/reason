


Reason: Build Systems Rapidly [![Build Status](https://travis-ci.org/facebook/reason.svg?branch=master)](https://travis-ci.org/facebook/reason) [![CircleCI](https://circleci.com/gh/facebook/reason/tree/master.svg?style=svg)](https://circleci.com/gh/facebook/reason/tree/master)
=========================================

- Approachable syntax.
- Powerful, automatic source code formatting.
- Adopt incrementally with `JavaScript/C` interop.
- Ahead-of-time compilation to assembly - without a language level VM.
- Rapidly develop and share projects.

Project Workflows
-------------------

Compile to native: http://facebook.github.io/reason/nativeWorkflow.html
Compile to JS: http://facebook.github.io/reason/jsWorkflow.html

Installing via OPAM
-------------------
The OPAM installation doesn't use the isolated directory based sandbox model that `ExampleProject` does, but if you are very familiar with `opam`, you should be able to work out conflicts in the global switch, or create a new switch for the purpose of using `Reason`.

```sh
# On OSX, install opam via Homebrew:
brew update
brew install opam
# On Linux, see here (you will need opam >= 1.2.2): http://opam.ocaml.org/doc/Install.html

opam init
# Add this to your ~/.bashrc (or ~/.zshrc):
#   eval $(opam config env)

opam update
opam switch 4.02.3
eval $(opam config env)
opam install reason
```

#### Testing OPAM installation.

Test the installation by compiling the following program:


```sh
echo 'print_string "Hello world"' > Hello.re

rebuild Hello.native # Automatically generates Hello.native from Hello.re

./Hello.native

```

Get Started Now
---------------
Check out the [docs](http://facebook.github.io/reason) which guide you through the basic syntax and toolchain features.

Contribute back to that documentation in the [docs folder](https://github.com/facebook/reason/tree/master/docs).


Community
---------------
Get in touch! We're on IRC freenode #reasonml, and [Discord](https://discord.gg/reasonml).


Contributing To Development
----------
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
