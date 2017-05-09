`reason-mode`: A major Emacs mode for Reason syntax
============================================================

## Deprecated. Please use https://github.com/arichiardi/reason-mode

This plugin is almost a drop in copy of the Rust plugin, with some
modifications made. See original plugin here:
https://github.com/rust-lang/rust-mode


### Manual Installation

To install manually, install the reason-cli (`npm -g install git://github.com/reasonml/reason-cli.git`) and add this to your
`.emacs` file:

```lisp
;;----------------------------------------------------------------------------
;; Reason setup
;; Expects reason-cli to be installed:
;; npm install -g git://github.com/reasonml/reason-cli.git
;;----------------------------------------------------------------------------

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(let ((support-base-dir (concat (replace-regexp-in-string "refmt" "" (file-truename (chomp-end (shell-command-to-string "which refmt")))) ".."))
      (merlin-base-dir (concat (replace-regexp-in-string "ocamlmerlin" "" (file-truename (chomp-end (shell-command-to-string "which ocamlmerlin")))) "..")))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (add-to-list 'load-path (concat merlin-base-dir "/share/emacs/site-lisp/"))
  (setq merlin-command (concat merlin-base-dir "/bin/ocamlmerlin"))

  ;; Add npm reason-mode to the emacs load path and tell emacs where to find refmt
  (add-to-list 'load-path (concat support-base-dir "/share/emacs/site-lisp"))
  (setq refmt-command (concat support-base-dir "/bin/refmt")))

(require 'reason-mode)
(require 'merlin)
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook 'refmt-before-save)
                              (merlin-mode)))

(setq merlin-ac-setup t)
```
If you have iedit mode set up:
```
(require 'merlin-iedit)
(defun evil-custom-merlin-iedit ()
  (interactive)
  (if iedit-mode (iedit-mode)
    (merlin-iedit-occurrences)))
(define-key merlin-mode-map (kbd "C-c C-e") 'evil-custom-merlin-iedit)
```
(Thanks @sgrove: [https://gist.github.com/sgrove/c9bdfed77f4da8db108dfb2c188f7baf](https://gist.github.com/sgrove/c9bdfed77f4da8db108dfb2c188f7baf))

This associates `reason-mode` with `.re` and `.rei` files. To enable it explicitly, do
<kbd>M-x reason-mode</kbd>.

### Spacemacs

There is currently no offical reason layer available, but you can install the `reason-mode` package automatically.
Some are working on a layer in the meantime [#1149](https://github.com/facebook/reason/issues/1149).

```lisp
dotspacemacs-additional-packages
  '(
    (reason-mode
      :location (recipe
        :repo "facebook/reason"
        :fetcher github
        :files ("editorSupport/emacs/reason-mode.el" "editorSupport/emacs/refmt.el")))
)
```

Afterwards add the [snippet](#manual-installation) to your `dotspacemacs/user-config`.
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
