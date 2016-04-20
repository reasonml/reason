`reason-mode`: A major Emacs mode for Reason syntax
============================================================

This plugin is almost a drop in copy of the Rust plugin, with some
modifications made. See original plugin here:
https://github.com/rust-lang/rust-mode


### Manual Installation

To install manually, install both reason and merlin, add this to your
`.emacs` file:

```lisp
;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

(setq opam (substring (shell-command-to-string "opam config var prefix 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam "/share/emacs/site-lisp"))
(setq refmt-command (concat opam "/bin/refmt"))
(setq reason-merlinfmt-command (concat opam "/bin/refmt_merlin"))

(require 'reason-mode)
(require 'merlin)
(setq merlin-ac-setup t)
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))
(setq merlin-default-flags (list "-pp" reason-merlinfmt-command))
```

This associates `reason-mode` with `.re` and `.rei` files. To enable it explicitly, do
<kbd>M-x reason-mode</kbd>.

### Features

#### Auto-format before saving

If you have refmt installed, you can add this to your `.emacs` file to enable
auto-format:
```
(add-hook 'reason-mode-hook (lambda ()
          (add-hook 'before-save-hook 'refmt-before-save)))
```

### Tests via ERT

The file `reason-mode-tests.el` contains tests that can be run via
[ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html).
You can use `run_reason_emacs_tests.sh` to run them in batch mode, if
you set the environment variable EMACS to a program that runs emacs.

To test it under emacs 23, which does not ship with ERT, download ert.el from
https://raw.githubusercontent.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el
and put it in a place where emacs can find it.  (ERT has shipped with emacs
since emacs 24.)

## License

`reason-mode` is distributed under the terms of both the MIT license and the
Apache License (Version 2.0).

See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.
