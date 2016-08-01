


Reason: Build Systems Rapidly [![Build Status](https://travis-ci.org/facebook/reason.png?branch=master)](https://travis-ci.org/facebook/reason)
=========================================


- Approachable syntax.
- Powerful, automatic source code formatting.
- Adopt incrementally with `JavaScript/C` interop.
- Ahead-of-time compilation to assembly - without a language level VM.
- Rapidly develop and share projects.

Install Stable
----------

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
opam pin add -y merlin 'https://github.com/the-lambda-church/merlin.git#reason-0.0.1'
opam pin add -y merlin_extend 'https://github.com/let-def/merlin-extend.git#reason-0.0.1'
opam pin add -y reason 'https://github.com/facebook/reason.git#0.0.6'

```

Test Installation
----------

Test the [installation](#install-stable) by compiling the following program:


```sh
echo 'print_string "Hello world"' > Hello.re

rebuild Hello.native # Automatically generates Hello.native from Hello.re

./Hello.native

```

Get Started Now
---------------
Check out the [docs](http://facebook.github.io/reason) which guide you through the basic syntax and toolchain features.

Contribute back to that documentation in the [documentation branch](https://github.com/facebook/reason/tree/gh-pages).


Community
---------------
Get in touch! We're on IRC freenode #reasonml, and [Gitter](https://gitter.im/facebook/reason).


License
-------

New content is licensed under the MIT license, works that are forked from other
projects are under their original licenses.
[MIT license](LICENSE.txt)

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.
