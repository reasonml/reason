
Reason: Build Systems Rapidly
=========================================

- Approachable syntax.
- Powerful, automatic source code formatting.
- Adopt Incrementally with straightforward `JavaScript/C` interop.
- Bare metal compilation - *No Virtual Machine*.
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
#   eval `opam config env`

opam switch 4.02.3
opam pin add -y merlin https://github.com/the-lambda-church/merlin.git#reason-0.0.1
opam pin add -y merlin_extend https://github.com/let-def/merlin-extend.git#reason-0.0.1
opam pin add -y reason https://github.com/facebook/reason.git#0.0.5

```

Test Installation
----------

Test the [installation](#install-stable) by compiling the following program:


```sh
echo print_string \"Hello world\" > Hello.re

rebuild Hello.native # Automatically generates Hello.native from Hello.re

./Hello.native

```

Get Started Now
---------------
Checkout the [docs](http://facebook.github.io/Reason) which guide you through the basic syntax and toolchain features.

Contribute back to that documentation in the [`gh-pages` branch](https://github.com/facebook/Reason/tree/gh-pages).



License
-------

New content is licensed under the MIT license, works that are forked from other
projects are under their original licenses.
[MIT license](LICENSE.txt)

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.
