;;; reasonfmt.el --- utility functions to format reason code

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)

;;; Commentary:
;;

;;; Code:

(defcustom reasonfmt-command "reasonfmt"
  "The 'reasonfmt' command."
  :type 'string
  :group 'reason-fmt)

(defcustom reasonfmt-show-errors 'buffer
    "Where to display reasonfmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite reasonfmt's echo output if used from inside
a `before-save-hook'."
    :type '(choice
            (const :tag "Own buffer" buffer)
            (const :tag "Echo area" echo)
            (const :tag "None" nil))
      :group 'reason-fmt)

;;;###autoload
(defun reasonfmt-before-save ()
  "Add this to .emacs to run reasonfmt on the current buffer when saving:
 (add-hook 'before-save-hook 'reasonfmt-before-save)."
    (interactive)
      (when (eq major-mode 'reason-mode) (reasonfmt)))

(defun reason--goto-line (line)
  (goto-char (point-min))
    (forward-line (1- line)))

(defun reason--delete-whole-line (&optional arg)
    "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
    (setq arg (or arg 1))
    (if (and (> arg 0)
             (eobp)
             (save-excursion (forward-visible-line 0) (eobp)))
        (signal 'end-of-buffer nil))
    (if (and (< arg 0)
             (bobp)
             (save-excursion (end-of-visible-line) (bobp)))
        (signal 'beginning-of-buffer nil))
    (cond ((zerop arg)
           (delete-region (progn (forward-visible-line 0) (point))
                          (progn (end-of-visible-line) (point))))
          ((< arg 0)
           (delete-region (progn (end-of-visible-line) (point))
                          (progn (forward-visible-line (1+ arg))
                                 (unless (bobp)
                                   (backward-char))
                                 (point))))
          (t
           (delete-region (progn (forward-visible-line 0) (point))
                                                  (progn (forward-visible-line arg) (point))))))

(defun reason--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in reason--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (reason--goto-line (- from line-offset))
                (incf line-offset len)
                (reason--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in reason--apply-rcs-patch")))))))))

(defun reasonfmt--process-errors (filename tmpfile errorfile errbuf)
  (with-current-buffer errbuf
    (if (eq reasonfmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (reasonfmt--kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the reasonfmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "reasonfmt errors:\n")
      (while (search-forward-regexp (regexp-quote tmpfile) nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun reasonfmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun reasonfmt ()
   "Format the current buffer according to the reasonfmt tool."
   (interactive)
   (let ((bufferfile (make-temp-file "reasonfmt" nil ".re"))
         (outputfile (make-temp-file "reasonfmt" nil ".re"))
         (errorfile (make-temp-file "reasonfmt" nil ".re"))
         (errbuf (if reasonfmt-show-errors (get-buffer-create "*Reasonfmt Errors*")))
         (patchbuf (get-buffer-create "*Reasonfmt patch*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
     (unwind-protect
         (save-restriction
           (widen)
           (write-region nil nil bufferfile)
           (if errbuf
               (with-current-buffer errbuf
                 (setq buffer-read-only nil)
                 (erase-buffer)))
           (with-current-buffer patchbuf
                         (erase-buffer))
           (if (zerop (call-process reasonfmt-command nil (list (list :file outputfile) errorfile)
                                    nil "-parse" "re" "-print" "re" bufferfile))
               (progn
                 (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-"
                                      outputfile)
                 (reason--apply-rcs-patch patchbuf)
                 (message "Applied reasonfmt")
                (if errbuf (reasonfmt--kill-error-buffer errbuf)))
             (message "Could not apply reasonfmt")
             (if errbuf
               (reasonfmt--process-errors (buffer-file-name) bufferfile errorfile errbuf)))))
   (kill-buffer patchbuf)
   (delete-file errorfile)
   (delete-file bufferfile)
   (delete-file outputfile)))

(provide 'reasonfmt)

;;; reasonfmt.el ends here
