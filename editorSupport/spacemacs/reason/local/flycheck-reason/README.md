flycheck-reason — Flycheck for Reason (based on Flycheck-ocaml)
===================================

[![License GPL 3][badge-license]][copying]

Add Reason/Merlin support to [Flycheck][]:

- Add a `reason-merlin` syntax checker using [Merlin][]

Installation
------------

As usual, from [MELPA][] or [MELPA Stable][].

In your [`Cask`][cask] file:

```cl
(source gnu)
(source melpa)

(depends-on "flycheck-reason")
```

In your `init.el`:

```cl
(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-reason-setup))

(add-hook 'tuareg-mode-hook #'merlin-mode)
```

Usage
-----

Just use Flycheck as usual in Tuareg Mode buffers.  Flycheck will automatically
use the new `reason-merlin` syntax checker if Merlin Mode is enabled and Merlin's
own error checking (`merlin-error-after-save`) is disabled.

If you enable Merlin's error checking with `M-x merlin-toggle-view-errors`
Flycheck will not use the `reason-merlin` syntax checker anymore, to avoid
duplicate and redundant error reporting.

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [`COPYING`][copying] for details.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[COPYING]: https://github.com/flycheck/flycheck-ocaml/blob/master/COPYING
[badge-melpa]: http://melpa.org/packages/flycheck-ocaml-badge.svg
[badge-melpa-stable]: http://stable.melpa.org/packages/flycheck-ocaml-badge.svg
[badge-travis]: https://travis-ci.org/flycheck/flycheck-ocaml.svg?branch=master
[Flycheck]: http://www.flycheck.org
[Merlin]: https://github.com/the-lambda-church/merlin
[MELPA]: http://melpa.org
[MELPA Stable]: http://stable.melpa.org
[cask]: http://cask.readthedocs.org

