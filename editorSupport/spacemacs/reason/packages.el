;;; packages.el --- reason Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq reason-packages
  '(
    ;; We do not "own" the auto-complete package, but we list it as a dependency,
    ;; and run a post-init hook.
    auto-complete
    company
    ;; Provided by ocaml layer which is required as specified in config.el
    ;; We still declare our dependency on merlin but do not init it since we
    ;; don't own it (the ocaml layer does).
    merlin
    ;; This layer "owns" these two packages (locally even).
    (reason-mode :location local)
    (flycheck-reason :location local)
  )
)

;; (defun reason/post-init-auto-complete ()
;;   (spacemacs|enable-auto-complete merlin-mode))

(defun reason/post-init-company ()
 (spacemacs|add-company-hook merlin-mode))

(when (configuration-layer/layer-usedp 'syntax-checking)
 (defun reason/init-flycheck-reason ()
   ;; Again, having trouble defering local packages, so we'll have to do all of
   ;; the setup in a with-eval-after-load
   (use-package flycheck-reason))
)

(with-eval-after-load 'flycheck-reason
  (with-eval-after-load 'merlin
    (message "with-eval-after-load flycheck-reason/merlin")
    (setq merlin-error-after-save nil)
    ;; This just does the following when flycheck enabled by default:
    ;; (add-hook 'merlin-mode-hook 'flycheck-mode)))
    ;; Which turns on flycheck whenever merlin-mode is activated
    (spacemacs/add-flycheck-hook 'merlin-mode-hook)
    (flycheck-reason-setup)
  )
)

;; We know that the ocaml layer would have already initialized merlin.
;; Therefore, we just need to make reason mode trigger merlin mode.
(defun reason/post-init-merlin ()
  (add-hook 'reason-mode-hook 'merlin-mode)
;;    (set-default 'merlin-use-auto-complete-mode t)
  (set-default 'merlin-use-auto-complete-mode nil)
  (setq merlin-completion-with-doc t)
  (push 'merlin-company-backend company-backends-merlin-mode)
  ;; These never worked in .ml or .re files.
  (spacemacs/set-leader-keys-for-major-mode 'reason-mode
    "cp" 'merlin-project-check
    "cr" 'merlin-refresh
    "cv" 'merlin-goto-project-file
    "eC" 'merlin-error-check
    "en" 'merlin-error-next
    "eN" 'merlin-error-prev
    "gb" 'merlin-pop-stack
    "gg" #'(lambda ()
            (interactive)
            (let ((merlin-locate-in-new-window 'never))
              (merlin-locate)))
    "gG" #'(lambda ()
            (interactive)
            (let ((merlin-locate-in-new-window 'always))
              (merlin-locate)))
    "gl" 'merlin-locate-ident
    "gi" 'merlin-switch-to-ml
    "gI" 'merlin-switch-to-mli
    "hh" 'merlin-document
    "ht" 'merlin-type-enclosing
    "hT" 'merlin-type-expr
    "rd" 'merlin-destruct
    )
)


;; Initialize the reason syntax mode
(defun reason/init-reason-mode ()
  ;; For some reason, deferring does not work
  (use-package reason-mode
    :init
      (progn
        (spacemacs//init-ocaml-opam)
        (add-to-list 'auto-mode-alist '("\\.re\\'" . reason-mode))
        ;; Make OCaml-generated files invisible to filename completion
        (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))
          (add-to-list 'completion-ignored-extensions ext))
      )
    :config
    (progn
      (when (fboundp 'sp-local-pair)
        ;; Don't pair type variable identifiers, or poly variants.
        (sp-local-pair 'reason-mode "'" nil :actions nil))
        (sp-local-pair 'reason-mode "`" nil :actions nil))
     ))

