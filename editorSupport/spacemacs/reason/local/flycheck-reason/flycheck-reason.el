;;; flycheck-ocaml.el --- Flycheck: OCaml support    -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2015  Frédéric Bour <frederic.bour@lakaban.net>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-ocaml
;; Keywords: convenience, tools, languages
;; Version: 0.4-cvs
;; Package-Requires: ((emacs "24.1") (flycheck "0.22") (merlin "2.3") (let-alist "1.0.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This Flycheck extension provides a new `ocaml-merlin' syntax checker which
;; uses Merlin Mode (see URL `https://github.com/the-lambda-church/merlin') to
;; check OCaml buffers for errors.
;;
;; # Setup
;;
;; Add the following to your init file:
;;
;;    (with-eval-after-load 'merlin
;;      ;; Disable Merlin's own error checking
;;      (setq merlin-error-after-save nil)
;;
;;      ;; Enable Flycheck checker
;;      (flycheck-ocaml-setup))
;;
;;    (add-hook 'tuareg-mode-hook #'merlin-mode)
;;
;; # Usage
;;
;; Just use Flycheck as usual in Tuareg Mode buffers.  Flycheck will
;; automatically use the new `ocaml-merlin` syntax checker if Merlin Mode is
;; enabled and Merlin's own error checking (`merlin-error-after-save`) is
;; disabled.
;;
;; If you enable Merlin's error checking with `M-x merlin-toggle-view-errors`
;; Flycheck will not use the `ocaml-merlin` syntax checker anymore, to avoid
;; duplicate and redundant error reporting.

;;; Code:

(eval-when-compile
  (require 'let-alist))

(require 'merlin)
(require 'flycheck)

(defconst flycheck-ocaml-merlin-message-re
  (rx string-start
      ;; Skip over leading spaces and punctuation
      (zero-or-more (any punct space control))
      ;; Take the level...
      (group-n 1 (or "Warning" "Error"))
      ;; ...and skip over trailing digits (e.g. Warning 8:)
      (zero-or-more (any space digit)) ": "
      ;; The rest is the real error message
      (group-n 2 (one-or-more anything)) string-end)
  "Regular expression to parse a Merlin error message.")

(defun flycheck-ocaml-merlin-parse-message (message)
  "Parse an error MESSAGE from a Merlin error.

Return `(LEVEL . PARSED-MESSAGE)', where LEVEL is the Flycheck
error level, and PARSED-MESSAGE is the real error message with
irrelevant parts removed."
  (when (string-match flycheck-ocaml-merlin-message-re message)
    (let ((level (pcase (match-string 1 message)
                   (`"Warning" 'warning)
                   (`"Error" 'error)
                   (level (lwarn 'flycheck-ocaml :error
                                 "Unknown error level %S" level)))))
      (cons level
            ;; Collapse whitespace in error messages
            (replace-regexp-in-string (rx (one-or-more (any space "\n" "\r")))
                                      " " (string-trim (match-string 2 message))
                                      'fixed-case 'literal)))))

(defun flycheck-ocaml-merlin-parse-error (alist checker)
  "Parse a Merlin error ALIST from CHECKER into a `flycheck-error'.

Return the corresponding `flycheck-error'."
  (let-alist alist
    (when .message
      (pcase-let* ((`(,level . ,message)
                    (flycheck-ocaml-merlin-parse-message .message)))
        ;; OCaml columns seem to be zero-based, see
        ;; https://github.com/flycheck/flycheck-ocaml/issues/2
        (flycheck-error-new-at (or .start.line 1)
                               (when .start.col (1+ .start.col))
                               (or level 'error) (or message .message)
                               :checker checker)))))

(defun flycheck-verify-ocaml-merlin (_checker)
  "Verify the OCaml Merlin syntax checker."
  (let ((command (executable-find (merlin-command))))
    (list
     (flycheck-verification-result-new
      :label "Merlin command"
      :message (if command (format "Found at %s" command) "Not found")
      :face (if command 'success '(bold error)))
     (flycheck-verification-result-new
      :label "Merlin mode"
      :message (if merlin-mode "enabled" "disabled")
      :face (if merlin-mode 'success '(bold warning)))
     (flycheck-verification-result-new
      :label "Merlin error checking"
      :message (if merlin-error-after-save "enabled" "disabled")
      :face (if merlin-error-after-save '(bold warning) 'success)))))

(defun flycheck-ocaml-merlin-start (checker callback)
  "Start a Merlin syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  (let ((callback-err (lambda (msg) (funcall callback 'errored msg))))
    (merlin/sync-async
      (lambda ()
        (merlin/send-command-async
         'errors
         (lambda (data)
           (condition-case err
               (let ((errors (delq nil
                                   (mapcar
                                    (lambda (alist)
                                      (flycheck-ocaml-merlin-parse-error alist
                                                                         checker))
                                    data))))
                 (funcall callback 'finished errors))
             (error (funcall callback 'errored (error-message-string err)))))
         callback-err))
      callback-err)))

(flycheck-define-generic-checker 'ocaml-merlin
  "A syntax checker for OCaml using Merlin Mode.

See URL `https://github.com/the-lambda-church/merlin'."
  :start #'flycheck-ocaml-merlin-start
  :verify #'flycheck-verify-ocaml-merlin
  :modes '(caml-mode tuareg-mode)
  :predicate (lambda () (and merlin-mode
                             ;; Don't check if Merlin's own checking is
                             ;; enabled, to avoid duplicate overlays
                             (not merlin-error-after-save))))

;;;###autoload
(defun flycheck-ocaml-setup ()
  "Setup Flycheck OCaml.

Add `ocaml-merlin' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'ocaml-merlin))

(provide 'flycheck-ocaml)

;;; flycheck-ocaml.el ends here
