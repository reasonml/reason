


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
opam pin add -y reason 'https://github.com/facebook/reason.git#1.3.0'

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

Contribute back to that documentation in the [docs folder](https://github.com/facebook/reason/tree/master/docs).


Community
---------------
Get in touch! We're on IRC freenode #reasonml, and [Gitter](https://gitter.im/facebook/reason).

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
opam switch 4.02.3
eval $(opam config env)
opam pin add -y merlin 'https://github.com/the-lambda-church/merlin.git'
opam pin add -y merlin_extend 'https://github.com/let-def/merlin-extend.git'
git clone git@github.com:facebook/reason.git
cd reason
opam pin add -y reason .
```

If the compilation of `Reason` does not succeed *after* pinning
`merlin/merlin-extend` as described above, then a change to `merlin` or
`merlin-extend` may have broken `Reason` (please file a Github Issue).  We
should try to keep all three projects' master branches compatible with each
other.


License
-------

New content is licensed under the MIT license, works that are forked from other
projects are under their original licenses.
[MIT license](LICENSE.txt)

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.
