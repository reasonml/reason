


Reason: Build Systems Rapidly [![Build Status](https://travis-ci.org/facebook/reason.svg?branch=master)](https://travis-ci.org/facebook/reason) [![CircleCI](https://circleci.com/gh/facebook/reason/tree/master.svg?style=svg)](https://circleci.com/gh/facebook/reason/tree/master)
=========================================


- Approachable syntax.
- Powerful, automatic source code formatting.
- Adopt incrementally with `JavaScript/C` interop.
- Ahead-of-time compilation to assembly - without a language level VM.
- Rapidly develop and share projects.

Install Via `npm`
----------------
`npm` version > 3.0 is required - [install here](https://nodejs.org/en/download/current/).

#### Reason Project

Installing the [`ReasonProject`](https://reasonml.github.io/ReasonProject/) starter kit using `npm` is the easiest way to get started with `Reason`. It will install the `master` `Reason` branch and all of the dependencies for you into a local directory based sandbox. It even includes the compiler, IDE support, and REPL. Simply delete the directory when you're done and it's gone from your computer.

```
git clone https://github.com/reasonml/ReasonProject.git
cd ReasonProject
npm install
npm start
```

While it's installing, read about [how to use ReasonProject](https://reasonml.github.io/ReasonProject/) to compile your simple project and use its built-in editor support and top level.

Rebel
-----
The `ReasonProject` uses a very simple build system called `rebuild` that comes with `Reason`, and is enough to start a small project. For large-scale development, we are currently developing a build and namespacing workflow called [`rebel`](https://github.com/reasonml/rebel), which is built on [`jenga`](https://github.com/janestreet/jenga). It is an early work in progress, but you can also try it out via [`RebelExampleProject`](https://github.com/reasonml/RebelExampleProject). `rebel`  currently takes a long time to compile the first time it's used, so for getting started quickly, use [`ReasonProject`](https://reasonml.github.io/ReasonProject/) instead.

`rebel` features:

- Easy to use [`bucklescript`](https://github.com/bloomberg/bucklescript) integration.
- Easy to use [`js_of_ocaml`](https://github.com/ocsigen/js_of_ocaml) integration.
- Automatic namespacing based on your `package.json` package name.
- Automatic generation of `.merlin` files.
- Powered by `jenga`, a fast, parallel, recoverable build system.
- Uses modern compiler features such as "module aliases" for faster compilation of projects.


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
opam switch 4.04.0
eval $(opam config env)
git clone git@github.com:facebook/reason.git
cd reason
opam pin add -y reason .
```

License
-------

See Reason license in [LICENSE.txt](LICENSE.txt).

Works that are forked from other projects are under their original licenses.

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.
