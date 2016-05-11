NOTE: THIS REPO IS PRIVATE BUT HAS PUBLIC CONTRIBUTORS!
=======================================================

-----

Reason: A New Interface To The ML Language
=========================================

- Approachable ES6 style syntax.
- Powerful, automatic source code formatting.
- Adopt Incrementally with straightforward `JavaScript/C` interop.
- Bare metal compilation - *No Virtual Machine*.
- Rapidly develop and share projects.

Install Stable
----------

```sh
brew update
brew install opam --HEAD
opam init
# Add this to your ~/.bashrc (or ~/.zshrc):
#   eval `opam config env`

opam update
opam switch 4.02.3
opam pin add -y merlin git@github.com:the-lambda-church/merlin.git#87ea0e7998c04f16e4821676c27f19d3879dc2d1
opam pin add -y merlin_extend git@github.com:def-lkb/merlin-extend.git#ef634252a793542b05ec00a90f3c17de8fe0a357
opam pin add -y reason git@github.com:facebook/reason.git#b105ecd2b6728e3f62f11f82a2947c8ca2b59fed

```

Get Started Now
---------------
Download the up-to-date [docs](https://github.com/facebook/Reason/archive/docs.zip) which guide you through the basic syntax and toolchain features.

Contribute back to that documentation in the [`docs` branch](https://github.com/facebook/Reason/tree/docs).



License
-------

New content is licensed under the MIT license, works that are forked from other
projects are under their original licenses.
[MIT license](LICENSE.txt)

Editor plugins (which have also been forked) in the `editorSupport/` directory
include their own licenses.
