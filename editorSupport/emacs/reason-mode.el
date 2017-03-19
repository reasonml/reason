;;; reason-mode.el --- A major emacs mode for editing Reason (based on rust-mode) -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.


;; Version: 0.2.0
;; Author: Mozilla
;; Url:
;; Keywords: languages

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:
;;

;;; Code:

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(defvar electric-pair-inhibit-predicate)

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defconst rust-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(defconst rust-re-non-standard-string
  (rx
   (or
    ;; Raw string: if it matches, it ends up with the starting character
    ;; of the string as group 1, any ending backslashes as group 4, and
    ;; the ending character as either group 5 or group 6.
    (seq
     ;; The "r" starts the raw string.  Capture it as group 1 to mark it as such syntactically:
     (group "r")

     ;; Then either:
     (or
      ;; a sequence at least one "#" (followed by quote).  Capture all
      ;; but the last "#" as group 2 for this case.
      (seq (group (* "#")) "#\"")

      ;; ...or a quote without any "#".  Capture it as group 3. This is
      ;; used later to match the opposite quote only if this capture
      ;; occurred
      (group "\""))

     ;; The contents of the string:
     (*? anything)

     ;; If there are any backslashes at the end of the string, capture
     ;; them as group 4 so we can suppress the normal escape syntax
     ;; parsing:
     (group (* "\\"))

     ;; Then the end of the string--the backreferences ensure that we
     ;; only match the kind of ending that corresponds to the beginning
     ;; we had:
     (or
      ;; There were "#"s - capture the last one as group 5 to mark it as
      ;; the end of the string:
      (seq "\"" (backref 2) (group "#"))

      ;; No "#"s - capture the ending quote (using a backref to group 3,
      ;; so that we can't match a quote if we had "#"s) as group 6
      (group (backref 3))

      ;; If the raw string wasn't actually closed, go all the way to the end
      string-end))

    ;; Character literal: match the beginning ' of a character literal
    ;; as group 7, and the ending one as group 8
    (seq
     (group "'")
     (or
      (seq
       "\\"
       (or
        (: "U" (= 8 xdigit))
        (: "u" (= 4 xdigit))
        (: "x" (= 2 xdigit))
        (any "'nrt0\"\\")))
      (not (any "'\\"))
      )
     (group "'"))
    )
   ))

(defun rust-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun rust-looking-back-symbols (SYMS)
  "Return non-nil if the point is just after a complete symbol that is a member of the list of strings SYMS"
  (save-excursion
    (let* ((pt-orig (point))
           (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
           (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
      (and
       (= end-of-symbol pt-orig)
       (member (buffer-substring-no-properties beg-of-symbol pt-orig) SYMS)))))

(defun rust-looking-back-ident ()
  "Non-nil if we are looking backwards at a valid rust identifier"
  (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
    (looking-back rust-re-ident beg-of-symbol)))

(defun rust-looking-back-macro ()
  "Non-nil if looking back at an ident followed by a !"
  (save-excursion (backward-char) (and (= ?! (char-after)) (rust-looking-back-ident))))

;; Syntax definitions and helpers
(defvar reason-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.  We suppress this with syntactic fontification when
    ;; needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defgroup reason-mode nil
  "Support for Rust code."
  :link '(url-link "http://www.rust-lang.org/")
  :group 'languages)

(defcustom reason-mode-hook nil
  "Hook called by `reason-mode'."
  :type 'hook
  :group 'reason-mode)

(defcustom reason-indent-offset 2
  "Indent Rust code by this number of spaces."
  :type 'integer
  :group 'reason-mode
  :safe #'integerp)

(defcustom rust-indent-method-chain nil
  "Indent Rust method chains, aligned by the '.' operators"
  :type 'boolean
  :group 'reason-mode
  :safe #'booleanp)

(defcustom rust-indent-where-clause t
  "Indent the line starting with the where keyword following a
function or trait.  When nil, where will be aligned with fn or trait."
  :type 'boolean
  :group 'reason-mode
  :safe #'booleanp)

(defcustom rust-playpen-url-format "https://play.rust-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen"
  :type 'string
  :group 'reason-mode)
(defcustom rust-shortener-url-format "http://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission"
  :type 'string
  :group 'reason-mode)

(defcustom rust-match-angle-brackets t
  "Enable angle bracket matching.  Attempt to match `<' and `>' where
  appropriate."
  :type 'boolean
  :safe #'booleanp
  :group 'reason-mode)

(defface rust-unsafe-face
  '((t :inherit font-lock-warning-face))
  "Face for the `unsafe' keyword."
  :group 'reason-mode)

(defun reason-paren-level () (nth 0 (syntax-ppss)))
(defun reason-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun reason-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))
(defun reason-rewind-irrelevant ()
  (interactive)
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (rust-looking-back-str "*/") (backward-char))
    (if (reason-in-str-or-cmnt)
        (reason-rewind-past-str-cmnt))
    (if (/= starting (point))
        (reason-rewind-irrelevant))))
(defun rust-in-macro ()
  (save-excursion
    (when (> (reason-paren-level) 0)
      (backward-up-list)
      (reason-rewind-irrelevant)
      (or (rust-looking-back-macro)
          (and (rust-looking-back-ident) (save-excursion (backward-sexp) (reason-rewind-irrelevant) (rust-looking-back-str "macro_rules!")))
          (rust-in-macro))
      )))

(defun rust-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
      (current-column))))

(defun reason-rewind-to-beginning-of-current-level-expr ()
  (interactive)
  (let ((current-level (reason-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (reason-rewind-irrelevant)
      (back-to-indentation))
    (while (> (reason-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))
    ;; When we're in the where clause, skip over it.  First find out the start
    ;; of the function and its paren level.
    (let ((function-start nil) (function-level nil))
      (save-excursion
        (reason-beginning-of-defun)
        (back-to-indentation)
        ;; Avoid using multiple-value-bind
        (setq function-start (point)
              function-level (reason-paren-level)))
      ;; On a where clause
      (when (or (looking-at "\\bwhere\\b")
                ;; or in one of the following lines, e.g.
                ;; where A: Eq
                ;;       B: Hash <- on this line
                (and (save-excursion
                       (re-search-backward "\\bwhere\\b" function-start t))
                     (= current-level function-level)))
        (goto-char function-start)))))

(defun reason-mode-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (reason-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket. That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                      (save-excursion
                        (reason-rewind-irrelevant)
                        (backward-up-list)
                        (reason-rewind-to-beginning-of-current-level-expr)
                        (if (looking-at "|")
                            (+ (current-column) (* reason-indent-offset 2))
                            (+ (current-column) reason-indent-offset)
                            )
                        ))))
             (cond
              ;; A function return type is indented to the corresponding function arguments
              ((looking-at "->")
               (save-excursion
                 (backward-list)
                 (or (rust-align-to-expr-after-brace)
                     (+ baseline reason-indent-offset))))

              ((reason-in-str-or-cmnt)
               (cond
                ;; In the end of the block -- align with star
                ((looking-at "*/") (+ baseline 1))
                ;; Indent to the following shape:
                ;; /* abcd
                ;;  * asdf
                ;;  */
                ;;
                ((looking-at "*") (+ baseline 1))
                ;; Indent to the following shape:
                ;; /* abcd
                ;;    asdf
                ;;  */
                ;;
                (t (+ baseline (+ reason-indent-offset 1)))))

              ;; A closing brace is 1 level unindented
              ((looking-at "}\\|)\\|\\]") (- baseline reason-indent-offset))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (reason-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (rust-align-to-expr-after-brace)))

                ;; When where-clauses are spread over multiple lines, clauses
                ;; should be aligned on the type parameters.  In this case we
                ;; take care of the second and following clauses (the ones
                ;; that don't start with "where ")
                (save-excursion
                  ;; Find the start of the function, we'll use this to limit
                  ;; our search for "where ".
                  (let ((function-start nil) (function-level nil))
                    (save-excursion
                      (reason-beginning-of-defun)
                      (back-to-indentation)
                      ;; Avoid using multiple-value-bind
                      (setq function-start (point)
                            function-level (reason-paren-level)))
                    ;; When we're not on a line starting with "where ", but
                    ;; still on a where-clause line, go to "where "
                    (when (and
                           (not (looking-at "\\bwhere\\b"))
                           ;; We're looking at something like "F: ..."
                           (and (looking-at (concat rust-re-ident ":"))
                                ;; There is a "where " somewhere after the
                                ;; start of the function.
                                (re-search-backward "\\bwhere\\b"
                                                    function-start t)
                                ;; Make sure we're not inside the function
                                ;; already (e.g. initializing a struct) by
                                ;; checking we are the same level.
                                (= function-level level)))
                      ;; skip over "where"
                      (forward-char 5)
                      ;; Unless "where" is at the end of the line
                      (if (eolp)
                          ;; in this case the type parameters bounds are just
                          ;; indented once
                          (+ baseline reason-indent-offset)
                        ;; otherwise, skip over whitespace,
                        (skip-chars-forward "[:space:]")
                        ;; get the column of the type parameter and use that
                        ;; as indentation offset
                        (current-column)))))

                (progn
                  (back-to-indentation)
                  (cond ((looking-at (regexp-opt '("constraint" "and" "type")))
                         baseline)
                        ((save-excursion
                         (reason-rewind-irrelevant)
                         (= (point) 1))
                         baseline)
                        ((save-excursion
                           (while (looking-at "|")
                             (reason-rewind-irrelevant)
                             (back-to-indentation))
                           (looking-at (regexp-opt '("type")))
                           )
                         (+ baseline reason-indent-offset))
                        ((looking-at "|\\|/[/*]")
                         baseline)
                        ((save-excursion
                         (reason-rewind-irrelevant)
                         (looking-back "[{;,\\[(]" (- (point) 2)))
                         baseline)
                        (t
                         (+ baseline reason-indent-offset)
                         )
                        )
                  ;; Point is now at the beginning of the current line
                  ))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;; Font-locking definitions and helpers
(defconst reason-mode-keywords
  '("and" "as"
    "else" "extern"
    "false" "fun" "for"
    "if" "impl" "in"
    "let"
    "module" "match" "mod" "move" "mut"
    "priv" "pub"
    "rec" "ref" "return"
    "self" "static" "switch" "struct" "super"
    "true" "trait" "type"
    "use"
    "virtual"
    "where" "while"))

(defconst reason-special-types
  '("int" "float" "string" "char"
    "bool" "unit" "list" "array" "exn"
    "option" "ref"
))

(defconst rust-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst rust-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")
(defun rust-re-word (inner) (concat "\\<" inner "\\>"))
(defun rust-re-grab (inner) (concat "\\(" inner "\\)"))
(defun rust-re-item-def (itype)
  (concat (rust-re-word itype) "[[:space:]]+" (rust-re-grab rust-re-ident)))

;; (See PR #42 -- this is just like `(regexp-opt words 'symbols)` from
;; newer Emacs versions, but will work on Emacs 23.)
(defun regexp-opt-symbols (words)
  (concat "\\_<" (regexp-opt words t) "\\_>"))
(defconst reason-re-special-types (regexp-opt-symbols reason-special-types))

(defvar reason-mode-font-lock-keywords
  (append
   `(
     ;; Keywords proper
     (,(regexp-opt-symbols reason-mode-keywords) . font-lock-keyword-face)

     ;; Special types
     (,(regexp-opt-symbols reason-special-types) . font-lock-builtin-face)

     ;; The unsafe keyword
     ("\\_<unsafe\\_>" . 'rust-unsafe-face)

     ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
     (,(rust-re-grab (concat "#\\!?\\[" rust-re-ident "[^]]*\\]"))
      1 font-lock-preprocessor-face keep)

     ;; Syntax extension invocations like `foo!`, highlight including the !
     (,(concat (rust-re-grab (concat rust-re-ident "!")) "[({[:space:][]")
      1 font-lock-preprocessor-face)

     ;; Field names like `foo:`, highlight excluding the :
     (,(concat (rust-re-grab rust-re-ident) ":[^:]") 1 font-lock-variable-name-face)

     ;; Module names like `foo::`, highlight including the ::
     (,(rust-re-grab (concat rust-re-ident "::")) 1 font-lock-type-face)

     ;; Lifetimes like `'foo`
     (,(concat "'" (rust-re-grab rust-re-ident) "[^']") 1 font-lock-variable-name-face)

     ;; CamelCase Means Type Or Constructor
     (,rust-re-type-or-constructor 1 font-lock-type-face)
     )

   ;; Item definitions
   (mapcar #'(lambda (x)
               (list (rust-re-item-def (car x))
                     1 (cdr x)))
           '(("and" . font-lock-type-face)
             ("enum" . font-lock-type-face)
             ("struct" . font-lock-type-face)
             ("type" . font-lock-type-face)
             ("mod" . font-lock-type-face)
             ("use" . font-lock-type-face)
             ("fn" . font-lock-function-name-face)
             ("static" . font-lock-constant-face)))))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun rust-font-lock-extend-region ()
  "Extend the region given by `font-lock-beg' and `font-lock-end'
  to include the beginning of a string or comment if it includes
  part of it.  Adjusts to include the r[#] of a raw string as
  well."

  (save-excursion
    (let ((orig-beg font-lock-beg)
          (orig-end font-lock-end))

      (let*
          ;; It's safe to call `syntax-ppss' here on positions that are
          ;; already syntactically fontified
          ((beg-ppss (syntax-ppss font-lock-beg))
           (beg-in-cmnt (and beg-ppss (nth 4 beg-ppss) (nth 8 beg-ppss)))
           (beg-in-str (and beg-ppss (nth 3 beg-ppss) (nth 8 beg-ppss))))

        (when (and beg-in-str (>= font-lock-beg beg-in-str))
          (setq font-lock-beg (nth 8 beg-ppss))
          (while (equal ?# (char-before font-lock-beg))
            (setq font-lock-beg (1- font-lock-beg)))
          (when (equal ?r (char-before font-lock-beg))
            (setq font-lock-beg (1- font-lock-beg))))

        (when (and beg-in-cmnt (> font-lock-beg beg-in-cmnt))
          (setq font-lock-beg beg-in-cmnt)))

      ;; We need to make sure that if the region ends inside a raw string, we
      ;; extend it out past the end of it.  But we can't use `syntax-ppss' to
      ;; detect that, becaue that depends on font-lock already being done, and we
      ;; are trying to figure out how much to font-lock before that.  So we use
      ;; the regexp directly.
      (save-match-data
        (goto-char font-lock-beg)
        (while (and (< (point) font-lock-end)
                    (re-search-forward rust-re-non-standard-string (buffer-end 1) t)
                    (<= (match-beginning 0) font-lock-end))
          (setq font-lock-end (max font-lock-end (match-end 0)))
          (goto-char (1+ (match-beginning 0)))))

      (or (/= font-lock-beg orig-beg)
          (/= font-lock-end orig-end))
      )))

(defun rust-conditional-re-search-forward (regexp bound condition)
  ;; Search forward for regexp (with bound).  If found, call condition and return the found
  ;; match only if it returns true.
  (let* (found
         found-ret-list
         (ret-list (save-excursion
                     (while (and (not found) (re-search-forward regexp bound t))
                       (setq
                        found-ret-list (list (point) (match-data))
                        found (save-match-data (save-excursion (ignore-errors (funcall condition)))))
                       ;; If the condition filters out a match, need to search
                       ;; again just after its beginning.  This will allow
                       ;; cases such as:
                       ;;    "bar" r"foo"
                       ;; where the filtered out search (r" r") should not
                       ;; prevent finding another one that begins in the middle
                       ;; of it (r"foo")
                       (when (not found)
                         (goto-char (1+ (match-beginning 0))))
                       )
                     (when found found-ret-list))))
    (when ret-list
      (goto-char (nth 0 ret-list))
      (set-match-data (nth 1 ret-list))
      (nth 0 ret-list))))

(defun rust-look-for-non-standard-string (bound)
  ;; Find a raw string or character literal, but only if it's not in the middle
  ;; of another string or a comment.

  (rust-conditional-re-search-forward
   rust-re-non-standard-string
   bound
   (lambda ()
     (let ((pstate (syntax-ppss (match-beginning 0))))
       (not
        (or
         (nth 4 pstate) ;; Skip if in a comment
         (and (nth 3 pstate) (wholenump (nth 8 pstate)) (< (nth 8 pstate) (match-beginning 0))) ;; Skip if in a string that isn't starting here
         ))))))

(defun rust-syntax-class-before-point ()
  (when (> (point) 1)
    (syntax-class (syntax-after (1- (point))))))

(defun rust-rewind-qualified-ident ()
  (while (rust-looking-back-ident)
    (backward-sexp)
    (when (save-excursion (reason-rewind-irrelevant) (rust-looking-back-str "::"))
      (reason-rewind-irrelevant)
      (backward-char 2)
      (reason-rewind-irrelevant))))

(defun rust-rewind-type-param-list ()
  (cond
   ((and (rust-looking-back-str ">") (equal 5 (rust-syntax-class-before-point)))
    (backward-sexp)
    (reason-rewind-irrelevant))

   ;; We need to be able to back up past the Fn(args) -> RT form as well.  If
   ;; we're looking back at this, we want to end up just after "Fn".
   ((member (char-before) '(?\] ?\) ))
    (let* ((is-paren (rust-looking-back-str ")"))
           (dest (save-excursion
                   (backward-sexp)
                   (reason-rewind-irrelevant)
                   (or
                    (when (rust-looking-back-str "->")
                      (backward-char 2)
                      (reason-rewind-irrelevant)
                      (when (rust-looking-back-str ")")
                        (backward-sexp)
                        (point)))
                    (and is-paren (point))))))
      (when dest
        (goto-char dest))))))

(defun rust-rewind-to-decl-name ()
  "If we are before an ident that is part of a declaration that
  can have a where clause, rewind back to just before the name of
  the subject of that where clause and return the new point.
  Otherwise return nil"

  (let* ((ident-pos (point))
         (newpos (save-excursion
                   (reason-rewind-irrelevant)
                   (rust-rewind-type-param-list)
                   (cond
                    ((rust-looking-back-symbols '("fn" "trait" "enum" "struct" "impl" "type")) ident-pos)

                    ((equal 5 (rust-syntax-class-before-point))
                     (backward-sexp)
                     (rust-rewind-to-decl-name))

                    ((looking-back "[:,'+=]" (1- (point)))
                     (backward-char)
                     (rust-rewind-to-decl-name))

                    ((rust-looking-back-str "->")
                     (backward-char 2)
                     (rust-rewind-to-decl-name))

                    ((rust-looking-back-ident)
                     (rust-rewind-qualified-ident)
                     (rust-rewind-to-decl-name))))))
    (when newpos (goto-char newpos))
    newpos))

(defun rust-is-in-expression-context (token)
  "Return t if what comes right after the point is part of an
  expression (as opposed to starting a type) by looking at what
  comes before.  Takes a symbol that roughly indicates what is
  after the point.

  This function is used as part of `rust-is-lt-char-operator' as
  part of angle bracket matching, and is not intended to be used
  outside of this context."

  (save-excursion
    (let ((postchar (char-after)))
      (reason-rewind-irrelevant)

      ;; A type alias or ascription could have a type param list.  Skip backwards past it.
      (when (member token '(ambiguous-operator open-brace))
        (rust-rewind-type-param-list))

      (cond

       ;; Certain keywords always introduce expressions
       ((rust-looking-back-symbols '("if" "while" "match" "return" "box" "in")) t)

       ;; "as" introduces a type
       ((rust-looking-back-symbols '("as")) nil)

       ;; An open angle bracket never introduces expression context WITHIN the angle brackets
       ((and (equal token 'open-brace) (equal postchar ?<)) nil)

       ;; An ident! followed by an open brace is a macro invocation.  Consider
       ;; it to be an expression.
       ((and (equal token 'open-brace) (rust-looking-back-macro)) t)

       ;; An identifier is right after an ending paren, bracket, angle bracket
       ;; or curly brace.  It's a type if the last sexp was a type.
       ((and (equal token 'ident) (equal 5 (rust-syntax-class-before-point)))
        (backward-sexp)
        (rust-is-in-expression-context 'open-brace))

       ;; If a "for" appears without a ; or { before it, it's part of an
       ;; "impl X for y", so the y is a type.  Otherwise it's
       ;; introducing a loop, so the y is an expression
       ((and (equal token 'ident) (rust-looking-back-symbols '("for")))
        (backward-sexp)
        (reason-rewind-irrelevant)
        (looking-back "[{;]" (1- (point))))

       ((rust-looking-back-ident)
        (rust-rewind-qualified-ident)
        (reason-rewind-irrelevant)
        (cond
         ((equal token 'open-brace)
          ;; We now know we have:
          ;;   ident <maybe type params> [{([]
          ;; where [{([] denotes either a {, ( or [.  This character is bound as postchar.
          (cond
           ;; If postchar is a paren or square bracket, then if the brace is a type if the identifier is one
           ((member postchar '(?\( ?\[ )) (rust-is-in-expression-context 'ident))

           ;; If postchar is a curly brace, the brace can only be a type if
           ;; ident2 is the name of an enum, struct or trait being declared.
           ;; Note that if there is a -> before the ident then the ident would
           ;; be a type but the { is not.
           ((equal ?{ postchar)
            (not (and (rust-rewind-to-decl-name)
                      (progn
                        (reason-rewind-irrelevant)
                        (rust-looking-back-symbols '("enum" "struct" "trait" "type"))))))
           ))

         ((equal token 'ambiguous-operator)
          (cond
           ;; An ampersand after an ident has to be an operator rather than a & at the beginning of a ref type
           ((equal postchar ?&) t)

           ;; A : followed by a type then an = introduces an expression (unless it is part of a where clause of a "type" declaration)
           ((and (equal postchar ?=)
                 (looking-back "[^:]:" (- (point) 2))
                 (not (save-excursion (and (rust-rewind-to-decl-name) (progn (reason-rewind-irrelevant) (rust-looking-back-symbols '("type"))))))))

           ;; "let ident =" introduces an expression--and so does "const" and "mut"
           ((and (equal postchar ?=) (rust-looking-back-symbols '("let" "const" "mut"))) t)

           ;; As a specific special case, see if this is the = in this situation:
           ;;     enum EnumName<type params> { Ident =
           ;; In this case, this is a c-like enum and despite Ident
           ;; representing a type, what comes after the = is an expression
           ((and
             (> (reason-paren-level) 0)
             (save-excursion
               (backward-up-list)
               (reason-rewind-irrelevant)
               (rust-rewind-type-param-list)
               (and
                (rust-looking-back-ident)
                (progn
                  (rust-rewind-qualified-ident)
                  (reason-rewind-irrelevant)
                  (rust-looking-back-str "enum")))))
            t)

           ;; Otherwise the ambiguous operator is a type if the identifier is a type
           ((rust-is-in-expression-context 'ident) t)))

         ((equal token 'colon)
          (cond
           ;; If we see a ident: not inside any braces/parens, we're at top level.
           ;; There are no allowed expressions after colons there, just types.
           ((<= (reason-paren-level) 0) nil)

           ;; We see ident: inside a list
           ((looking-back "[{,]" (1- (point)))
            (backward-up-list)

            ;; If a : appears whose surrounding paren/brackets/braces are
            ;; anything other than curly braces, it can't be a field
            ;; initializer and must be denoting a type.
            (when (looking-at "{")
              (reason-rewind-irrelevant)
              (rust-rewind-type-param-list)
              (when (rust-looking-back-ident)
                ;; We have a context that looks like this:
                ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                ;; the point is sitting just after ident2, and we trying to
                ;; figure out if the colon introduces an expression or a type.
                ;; The answer is that ident1 is a field name, and what comes
                ;; after the colon is an expression, if ident2 is an
                ;; expression.
                (rust-rewind-qualified-ident)
                (rust-is-in-expression-context 'ident))))


           ;; Otherwise, if the ident: appeared with anything other than , or {
           ;; before it, it can't be part of a struct initializer and therefore
           ;; must be denoting a type.
           (t nil)
           ))
         ))

       ;; An operator-like character after a string is indeed an operator
       ((and (equal token 'ambiguous-operator)
             (member (rust-syntax-class-before-point) '(5 7 15))) t)

       ;; A colon that has something other than an identifier before it is a
       ;; type ascription
       ((equal token 'colon) nil)

       ;; A :: introduces a type (or module, but not an expression in any case)
       ((rust-looking-back-str "::") nil)

       ((rust-looking-back-str ":")
        (backward-char)
        (rust-is-in-expression-context 'colon))

       ;; A -> introduces a type
       ((rust-looking-back-str "->") nil)

       ;; If we are up against the beginning of a list, or after a comma inside
       ;; of one, back up out of it and check what the list itself is
       ((or
         (equal 4 (rust-syntax-class-before-point))
         (rust-looking-back-str ","))
        (backward-up-list)
        (rust-is-in-expression-context 'open-brace))

       ;; A => introduces an expression
       ((rust-looking-back-str "=>") t)

       ;; A == introduces an expression
       ((rust-looking-back-str "==") t)

       ;; These operators can introduce expressions or types
       ((looking-back "[-+=!?&*]" (1- (point)))
        (backward-char)
        (rust-is-in-expression-context 'ambiguous-operator))

       ;; These operators always introduce expressions.  (Note that if this
       ;; regexp finds a < it must not be an angle bracket, or it'd
       ;; have been caught in the syntax-class check above instead of this.)
       ((looking-back rust-re-pre-expression-operators (1- (point))) t)
       ))))

(defun rust-is-lt-char-operator ()
  "Return t if the < sign just after point is an operator rather
  than an opening angle bracket, otherwise nil."

  (let ((case-fold-search nil))
    (save-excursion
      (reason-rewind-irrelevant)
      ;; We are now just after the character syntactically before the <.
      (cond

       ;; If we are looking back at a < that is not an angle bracket (but not
       ;; two of them) then this is the second < in a bit shift operator
       ((and (rust-looking-back-str "<")
             (not (equal 4 (rust-syntax-class-before-point)))
             (not (rust-looking-back-str "<<"))))

       ;; On the other hand, if we are after a closing paren/brace/bracket it
       ;; can only be an operator, not an angle bracket.  Likewise, if we are
       ;; after a string it's an operator.  (The string case could actually be
       ;; valid in rust for character literals.)
       ((member (rust-syntax-class-before-point) '(5 7 15)) t)

       ;; If we are looking back at an operator, we know that we are at
       ;; the beginning of an expression, and thus it has to be an angle
       ;; bracket (starting a "<Type as Trait>::" construct.)
       ((looking-back rust-re-pre-expression-operators (1- (point))) nil)

       ;; If we are looking back at a keyword, it's an angle bracket
       ;; unless that keyword is "self", "true" or "false"
       ((rust-looking-back-symbols reason-mode-keywords)
        (rust-looking-back-symbols '("self" "true" "false")))

       ;; If we're looking back at an identifier, this depends on whether
       ;; the identifier is part of an expression or a type
       ((rust-looking-back-ident)
        (backward-sexp)
        (or
         ;; The special types can't take type param lists, so a < after one is
         ;; always an operator
         (looking-at reason-re-special-types)

         (rust-is-in-expression-context 'ident)))

       ;; Otherwise, assume it's an angle bracket
       ))))

(defun rust-electric-pair-inhibit-predicate-wrap (char)
  "Wraps the default `electric-pair-inhibit-predicate' to prevent
  inserting a \"matching\" > after a < that would be treated as a
  less than sign rather than as an opening angle bracket."
  (or
   (when (= ?< char)
     (save-excursion
       (backward-char)
       (rust-is-lt-char-operator)))
   (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(defun rust-look-for-non-angle-bracket-lt-gt (bound)
  "Find an angle bracket (\"<\" or \">\") that should be part of
  a matched pair Relies on the fact that when it finds a < or >,
  we have already decided which previous ones are angle brackets
  and which ones are not.  So this only really works as a
  font-lock-syntactic-keywords matcher--it won't work at
  arbitrary positions without the earlier parts of the buffer
  having already been covered."

  (rust-conditional-re-search-forward
   "[<>]" bound
   (lambda ()
     (goto-char (match-beginning 0))
     (cond
      ;; If matching is turned off suppress all of them
      ((not rust-match-angle-brackets) t)

      ;; We don't take < or > in strings or comments to be angle brackets
      ((reason-in-str-or-cmnt) t)

      ;; Inside a macro we don't really know the syntax.  Any < or > may be an
      ;; angle bracket or it may not.  But we know that the other braces have
      ;; to balance regardless of the < and >, so if we don't treat any < or >
      ;; as angle brackets it won't mess up any paren balancing.
      ((rust-in-macro) t)

      ((looking-at "<")
       (rust-is-lt-char-operator))

      ((looking-at ">")
       (cond
        ;; Don't treat the > in -> or => as an angle bracket
        ((member (char-before (point)) '(?- ?=)) t)

        ;; If we are at top level and not in any list, it can't be a closing
        ;; angle bracket
        ((>= 0 (reason-paren-level)) t)

        ;; Otherwise, treat the > as a closing angle bracket if it would
        ;; match an opening one
        ((save-excursion
           (backward-up-list)
           (not (looking-at "<"))))))))))

(defvar reason-mode-font-lock-syntactic-keywords
  (append
   ;; Handle raw strings and character literals:
   `((rust-look-for-non-standard-string (1 "|" nil t) (4 "_" nil t) (5 "|" nil t) (6 "|" nil t) (7 "\"" nil t) (8 "\"" nil t)))
   ;; Find where < and > characters represent operators rather than angle brackets:
   '((rust-look-for-non-angle-bracket-lt-gt (0 "." t)))))

(defun reason-mode-syntactic-face-function (state)
  "Syntactic face function to distinguish doc comments from other comments."
  (if (nth 3 state) 'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face
        ))))

(defun reason-mode-try-find-alternate-file (mod-name extension)
  "Switch to the file given by MOD-NAME and EXTENSION."
  (let* ((filename (concat mod-name extension))
         (buffer (get-file-buffer filename)))
    (if buffer (switch-to-buffer buffer)
      (find-file filename))))

(defun reason-mode-find-alternate-file ()
  "Switch to implementation/interface file."
  (interactive)
  (let ((name buffer-file-name))
    (when (string-match "\\`\\(.*\\)\\.re\\([il]\\)?\\'" name)
      (let ((mod-name (match-string 1 name))
            (e (match-string 2 name)))
        (cond
         ((string= e "i")
            (reason-mode-try-find-alternate-file mod-name ".re"))
         (t
          (reason-mode-try-find-alternate-file mod-name ".rei")))))))

(defun rust-fill-prefix-for-comment-start (line-start)
  "Determine what to use for `fill-prefix' based on what is at the beginning of a line."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+\\)[!*]"
          (lambda (s)
            ;; We want the * to line up with the first * of the comment start
            (concat (make-string (- (length s) 2) ?\x20) "*"))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun rust-in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the user
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (nth 4 (syntax-ppss)))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; We need this when we're moving the point around and then checking syntax
    ;; while doing paragraph fills, because the cache it uses isn't always
    ;; invalidated during this.
    (syntax-ppss-flush-cache 1)
    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we use the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (let ((next-bol (line-beginning-position 2)))
      (while (save-excursion
               (end-of-line)
               (syntax-ppss-flush-cache 1)
               (and (nth 4 (syntax-ppss))
                    (save-excursion
                      (beginning-of-line)
                      (looking-at paragraph-start))
                    (looking-at "[[:space:]]*$")
                    (nth 4 (syntax-ppss next-bol))))
        (goto-char next-bol)))

    (syntax-ppss-flush-cache 1)
    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun rust-with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (nth 4 (syntax-ppss))
          (cond
           ;; If we're inside the comment and see a * prefix, use it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to use for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (rust-fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun rust-comment-indent-new-line (&optional arg)
  (rust-with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))

;;; Defun Motions

;;; Start of a reason binding
(defvar reason-binding
  (regexp-opt '("let" "type")))

(defun reason-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function'.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" reason-binding "\\)\\_>")
                      nil 'move (or arg 1)))

(defun reason-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after beginning-of-defun. So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Reason."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'reason-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(defvar reason-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" 'reason-mode-find-alternate-file)
    map
    )
  "Keymap for Reason major mode."
)

;;;###autoload
(define-derived-mode reason-mode reason-parent-mode "Reason"
  "Major mode for Reason code.

\\{reason-mode-map}"
  :group 'reason-mode
  :syntax-table reason-mode-syntax-table

  ;; Indentation
  (setq-local indent-line-function 'reason-mode-indent-line)

  ;; Fonts
  (add-to-list 'font-lock-extend-region-functions 'rust-font-lock-extend-region)
  (setq-local font-lock-defaults '(reason-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-keywords . reason-mode-font-lock-syntactic-keywords)
                                   (font-lock-syntactic-face-function . reason-mode-syntactic-face-function)
                                   ))

  ;; Misc
  (setq-local comment-start "/*")
  (setq-local comment-end   "*/")
  (setq-local indent-tabs-mode nil)

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local paragraph-start
              (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local require-final-newline t)
  (setq-local normal-auto-fill-function nil)
  (setq-local comment-multi-line t)
  (setq-local beginning-of-defun-function 'reason-beginning-of-defun)
  (setq-local end-of-defun-function 'reason-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rei?\\'" . reason-mode))


(defun reason-mode-reload ()
  (interactive)
  (unload-feature 'reason-mode)
  (require 'reason-mode)
  (reason-mode))

(require 'refmt)

(provide 'reason-mode)

;;; reason-mode.el ends here
