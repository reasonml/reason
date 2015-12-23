;;; rust-mode-tests.el --- ERT tests for rust-mode.el

(require 'rust-mode)
(require 'ert)
(require 'cl)

(setq rust-test-fill-column 32)

(defun rust-compare-code-after-manip (original point-pos manip-func expected got)
  (equal expected got))

(defun rust-test-explain-bad-manip (original point-pos manip-func expected got)
  (if (equal expected got)
      nil
    (list
     ;; The (goto-char) and (insert) business here is just for
     ;; convenience--after an error, you can copy-paste that into emacs eval to
     ;; insert the bare strings into a buffer
     "Rust code was manipulated wrong after:"
     `(insert ,original)
     `(goto-char ,point-pos)
     'expected `(insert ,expected)
     'got `(insert ,got)
     (loop for i from 0 to (max (length original) (length expected))
           for oi = (if (< i (length got)) (elt got i))
           for ei = (if (< i (length expected)) (elt expected i))
           while (equal oi ei)
           finally return `(first-difference-at
                            (goto-char ,(+ 1 i))
                            expected ,(char-to-string ei)
                            got ,(char-to-string oi))))))
(put 'rust-compare-code-after-manip 'ert-explainer
     'rust-test-explain-bad-manip)

(defun rust-test-manip-code (original point-pos manip-func expected)
  (with-temp-buffer
    (rust-mode)
    (insert original)
    (goto-char point-pos)
    (funcall manip-func)
    (should (rust-compare-code-after-manip
             original point-pos manip-func expected (buffer-string)))))

(defun test-fill-paragraph (unfilled expected &optional start-pos end-pos)
  "We're going to run through many scenarios here--the point should be able to be anywhere from the start-pos (defaults to 1) through end-pos (defaults to the length of what was passed in) and (fill-paragraph) should return the same result.  It should also work with fill-region from start-pos to end-pos.

Also, the result should be the same regardless of whether the code is at the beginning or end of the file.  (If you're not careful, that can make a difference.)  So we test each position given above with the passed code at the beginning, the end, neither and both.  So we do this a total of 1 + (end-pos - start-pos)*4 times.  Oy."
  (let* ((start-pos (or start-pos 1))
         (end-pos (or end-pos (length unfilled)))
         (padding "\n     \n")
         (padding-len (length padding)))
    (loop
     for pad-at-beginning from 0 to 1
     do (loop for pad-at-end from 0 to 1
              with padding-beginning = (if (= 0 pad-at-beginning) "" padding)
              with padding-end = (if (= 0 pad-at-end) "" padding)
              with padding-adjust = (* padding-len pad-at-beginning)
              with padding-beginning = (if (= 0 pad-at-beginning) "" padding)
              with padding-end = (if (= 0 pad-at-end) "" padding)
              ;; If we're adding space to the beginning, and our start position
              ;; is at the very beginning, we want to test within the added space.
              ;; Otherwise adjust the start and end for the beginning padding.
              with start-pos = (if (= 1 start-pos) 1 (+ padding-adjust start-pos))
              with end-pos = (+ end-pos padding-adjust)
              do (loop for pos from start-pos to end-pos
                       do (rust-test-manip-code
                           (concat padding-beginning unfilled padding-end)
                           pos
                           (lambda ()
                             (let ((fill-column rust-test-fill-column))
                               (fill-paragraph)))
                           (concat padding-beginning expected padding-end)))))
    ;; In addition to all the fill-paragraph tests, check that it works using fill-region
    (rust-test-manip-code
     unfilled
     start-pos
     (lambda ()
       (let ((fill-column rust-test-fill-column))
         (fill-region start-pos end-pos)))
     expected)
    ))

(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-second-line ()
  (test-fill-paragraph
   "/**
 * This is a very very very very very very very long string
 */"
   "/**
 * This is a very very very very
 * very very very long string
 */"))


(ert-deftest fill-paragraph-top-level-multi-line-style-doc-comment-first-line ()
  (test-fill-paragraph
   "/** This is a very very very very very very very long string
 */"
   "/** This is a very very very
 * very very very very long
 * string
 */"))

(ert-deftest fill-paragraph-multi-paragraph-multi-line-style-doc-comment ()
  (let
      ((multi-paragraph-unfilled
        "/**
 * This is the first really really really really really really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */"))
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/**
 * This is the first really
 * really really really really
 * really really long paragraph
 *
 * This is the second really really really really really really long paragraph
 */"
     1 89)
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/**
 * This is the first really really really really really really really long paragraph
 *
 * This is the second really
 * really really really really
 * really long paragraph
 */"
     90)))

(ert-deftest fill-paragraph-multi-paragraph-single-line-style-doc-comment ()
  (let
      ((multi-paragraph-unfilled
        "/// This is the first really really really really really really really long paragraph
///
/// This is the second really really really really really really long paragraph"))
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// This is the second really really really really really really long paragraph"
     1 86)
    (test-fill-paragraph
     multi-paragraph-unfilled
     "/// This is the first really really really really really really really long paragraph
///
/// This is the second really
/// really really really really
/// really long paragraph"
     87)))

(ert-deftest fill-paragraph-multi-paragraph-single-line-style-indented ()
  (test-fill-paragraph
   "     // This is the first really really really really really really really long paragraph
     //
     // This is the second really really really really really really long paragraph"
   "     // This is the first really
     // really really really
     // really really really
     // long paragraph
     //
     // This is the second really really really really really really long paragraph" 1 89))

(ert-deftest fill-paragraph-multi-line-style-inner-doc-comment ()
  (test-fill-paragraph
   "/*! This is a very very very very very very very long string
 */"
   "/*! This is a very very very
 * very very very very long
 * string
 */"))

(ert-deftest fill-paragraph-single-line-style-inner-doc-comment ()
  (test-fill-paragraph
   "//! This is a very very very very very very very long string"
   "//! This is a very very very
//! very very very very long
//! string"))

(ert-deftest fill-paragraph-prefixless-multi-line-doc-comment ()
  (test-fill-paragraph
   "/**
This is my summary. Blah blah blah blah blah. Dilly dally dilly dally dilly dally doo.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/"
   "/**
This is my summary. Blah blah
blah blah blah. Dilly dally
dilly dally dilly dally doo.

This is some more text.  Fee fie fo fum.  Humpty dumpty sat on a wall.
*/" 4 90))

(ert-deftest fill-paragraph-with-no-space-after-star-prefix ()
  (test-fill-paragraph
   "/**
 *This is a very very very very very very very long string
 */"
   "/**
 *This is a very very very very
 *very very very long string
 */"))

(ert-deftest fill-paragraph-single-line-style-with-code-before ()
  (test-fill-paragraph
   "fn foo() { }
/// This is my comment.  This is more of my comment.  This is even more."
   "fn foo() { }
/// This is my comment.  This is
/// more of my comment.  This is
/// even more." 14))

(ert-deftest fill-paragraph-single-line-style-with-code-after ()
  (test-fill-paragraph
   "/// This is my comment.  This is more of my comment.  This is even more.
fn foo() { }"
   "/// This is my comment.  This is
/// more of my comment.  This is
/// even more.
fn foo() { }" 1 73))

(ert-deftest fill-paragraph-single-line-style-code-before-and-after ()
  (test-fill-paragraph
   "fn foo() { }
/// This is my comment.  This is more of my comment.  This is even more.
fn bar() { }"
   "fn foo() { }
/// This is my comment.  This is
/// more of my comment.  This is
/// even more.
fn bar() { }" 14 85))

(defun test-auto-fill (initial position inserted expected)
  (rust-test-manip-code
   initial
   position
   (lambda ()
     (unwind-protect
         (progn
           (let ((fill-column rust-test-fill-column))
             (auto-fill-mode)
             (goto-char position)
             (insert inserted)
             (syntax-ppss-flush-cache 1)
             (funcall auto-fill-function)))
       (auto-fill-mode t)))
   expected))

(ert-deftest auto-fill-multi-line-doc-comment ()
  (test-auto-fill
   "/**
 *
 */"
   7
   " This is a very very very very very very very long string"
   "/**
 * This is a very very very very
 * very very very long string
 */"))

(ert-deftest auto-fill-single-line-doc-comment ()
  (test-auto-fill
   "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// "
   103
   "This is the second really really really really really really long paragraph"
   "/// This is the first really
/// really really really really
/// really really long paragraph
///
/// This is the second really
/// really really really really
/// really long paragraph"
   ))

(ert-deftest auto-fill-multi-line-prefixless ()
  (test-auto-fill
   "/*

 */"
   4
   "This is a very very very very very very very long string"
   "/*
This is a very very very very
very very very long string
 */"
   ))

(defun test-indent (indented &optional deindented)
  (let ((deindented (or deindented (replace-regexp-in-string "^[[:blank:]]*" "      " indented))))
    (rust-test-manip-code
     deindented
     1
     (lambda ()
       ;; The indentation will fail in some cases if the syntax properties are
       ;; not set.  This only happens when font-lock fontifies the buffer.
       (font-lock-fontify-buffer)
       (indent-region 1 (+ 1 (buffer-size))))
     indented)))


(ert-deftest indent-struct-fields-aligned ()
  (test-indent
   "
struct Foo { bar: int,
             baz: int }

struct Blah {x:int,
             y:int,
             z:String"))

(ert-deftest indent-doc-comments ()
  (test-indent
   "
/**
 * This is a doc comment
 *
 */

/// So is this

fn foo() {
    /*!
     * this is a nested doc comment
     */
    \n    //! And so is this
}"))

(ert-deftest indent-inside-braces ()
  (test-indent
   "
// struct fields out one level:
struct foo {
    a:int,
    // comments too
    b:char
}

fn bar(x:Box<int>) {   // comment here should not affect the next indent
    bla();
    bla();
}"))

(ert-deftest indent-top-level ()
  (test-indent
   "
// Everything here is at the top level and should not be indented
#[attrib]
mod foo;

pub static bar = Quux{a: b()}

use foo::bar::baz;

fn foo() { }
"))

(ert-deftest font-lock-multi-raw-strings-in-a-row ()
  (rust-test-font-lock
   "
r\"foo\\\", \"bar\", r\"bar\";
r\"foo\\.\", \"bar\", r\"bar\";
r\"foo\\..\", \"bar\", r\"foo\\..\\bar\";
r\"\\\", \"foo\", r\"\\foo\";
not_a_string();

"

   (apply 'append (mapcar (lambda (s) (list s 'font-lock-string-face))
                          '("r\"foo\\\"" "\"bar\"" "r\"bar\""
                            "r\"foo\\.\"" "\"bar\"" "r\"bar\""
                            "r\"foo\\..\"" "\"bar\"" "r\"foo\\..\\bar\""
                            "r\"\\\"" "\"foo\"" "r\"\\foo\"")))
   ))

(ert-deftest font-lock-raw-string-after-normal-string-ending-in-r ()
  (rust-test-font-lock
   "\"bar\" r\"foo\""
   '("\"bar\"" font-lock-string-face "r\"foo\"" font-lock-string-face)))

(ert-deftest indent-params-no-align ()
  (test-indent
   "
// Indent out one level because no params appear on the first line
fn xyzzy(
    a:int,
    b:char) { }

fn abcdef(
    a:int,
    b:char)
    -> char
{ }"))

(ert-deftest indent-params-align ()
  (test-indent
   "
// Align the second line of params to the first
fn foo(a:int,
       b:char) { }

fn bar(   a:int,
          b:char)
          -> int
{ }

fn baz(   a:int,  // should work with a comment here
          b:char)
          -> int
{ }
"))

(ert-deftest indent-open-after-arrow ()
  (test-indent
   "
// Indent function body only one level after `-> {`
fn foo1(a:int, b:char) -> int {
    let body;
}

fn foo2(a:int,
        b:char) -> int {
    let body;
}

fn foo3(a:int,
        b:char)
        -> int {
    let body;
}

fn foo4(a:int,
        b:char)
        -> int where int:A {
    let body;
}
"))

(ert-deftest indent-body-after-where ()
  (test-indent
   "
fn foo1(a: A, b: B) -> A
    where A: Clone + Default, B: Eq {
    let body;
    Foo {
        bar: 3
    }
}

fn foo2(a: A, b: B) -> A
    where A: Clone + Default, B: Eq
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style1a ()
  (test-indent
   "
fn foo1a(a: A, b: B, c: C) -> D
    where A: Clone + Default,
          B: Eq,
          C: PartialEq,
          D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style1b ()
  (test-indent
   "
fn foo1b(a: A, b: B, c: C) -> D
    where A: Clone + Default,
          B: Eq,
          C: PartialEq,
          D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style2a ()
  (test-indent
   "
fn foo2a(a: A, b: B, c: C) -> D where A: Clone + Default,
                                      B: Eq,
                                      C: PartialEq,
                                      D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style2b ()
  (test-indent
   "
fn foo2b(a: A, b: B, c: C) -> D where A: Clone + Default,
                                      B: Eq,
                                      C: PartialEq,
                                      D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style3a ()
  (test-indent
   "
fn foo3a(a: A, b: B, c: C) -> D where
    A: Clone + Default,
    B: Eq,
    C: PartialEq,
    D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style3b ()
  (test-indent
   "
fn foo3b(a: A, b: B, c: C) -> D where
    A: Clone + Default,
    B: Eq,
    C: PartialEq,
    D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style4a ()
  (let ((rust-indent-where-clause nil))
    (test-indent
     "
fn foo4a(a: A, b: B, c: C) -> D
where A: Clone + Default,
      B: Eq,
      C: PartialEq,
      D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-style4b ()
  (let ((rust-indent-where-clause nil))
    (test-indent
     "
fn foo4b(a: A, b: B, c: C) -> D
where A: Clone + Default,
      B: Eq,
      C: PartialEq,
      D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-impl-example ()
  (test-indent
   "
impl<'a, K, Q: ?Sized, V, S> Index<&'a Q> for HashMap<K, V, S>
    where K: Eq + Hash + Borrow<Q>,
          Q: Eq + Hash,
          S: HashState,
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-first-line ()
  (test-indent
   "fn foo1(a: A, b: B) -> A
    where A: Clone + Default, B: Eq {
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-square-bracket-alignment ()
  (test-indent
   "
fn args_on_the_next_line( // with a comment
    a:int,
    b:String) {
    let aaaaaa = [
        1,
        2,
        3];
    let bbbbbbb = [1, 2, 3,
                   4, 5, 6];
    let ccc = [   10, 9, 8,
                  7, 6, 5];
}
"))

(ert-deftest indent-nested-fns ()
  (test-indent
   "
fn nexted_fns(a: fn(b:int,
                    c:char)
                    -> int,
              d: int)
              -> uint
{
    0
}
"
   ))

(ert-deftest indent-multi-line-expr ()
  (test-indent
   "
fn foo()
{
    x();
    let a =
        b();
}
"
   ))

(ert-deftest indent-match ()
  (test-indent
   "
fn foo() {
    match blah {
        Pattern => stuff(),
        _ => whatever
    }
}
"
   ))

(ert-deftest indent-match-multiline-pattern ()
  (test-indent
   "
fn foo() {
    match blah {
        Pattern |
        Pattern2 => {
            hello()
        },
        _ => whatever
    }
}
"
   ))

(ert-deftest indent-indented-match ()
  (test-indent
   "
fn foo() {
    let x =
        match blah {
            Pattern |
            Pattern2 => {
                hello()
            },
            _ => whatever
        };
    y();
}
"
   ))

(ert-deftest indent-curly-braces-within-parens ()
  (test-indent
   "
fn foo() {
    let x =
        foo(bar(|x| {
            only_one_indent_here();
        }));
    y();
}
"
   ))

(ert-deftest indent-weirdly-indented-block ()
  (rust-test-manip-code
   "
fn foo() {
 {
this_block_is_over_to_the_left_for_some_reason();
 }

}
"
   16
   #'indent-for-tab-command
   "
fn foo() {
 {
     this_block_is_over_to_the_left_for_some_reason();
 }

}
"
   ))

(ert-deftest indent-multi-line-attrib ()
  (test-indent
   "
#[attrib(
    this,
    that,
    theotherthing)]
fn function_with_multiline_attribute() {}
"
   ))


;; Make sure that in effort to cover match patterns we don't mistreat || or expressions
(ert-deftest indent-nonmatch-or-expression ()
  (test-indent
   "
fn foo() {
    let x = foo() ||
        bar();
}
"
   ))

;; Closing braces in single char literals and strings should not confuse the indentation
(ert-deftest indent-closing-braces-in-char-literals ()
  (test-indent
   "
fn foo() {
    { bar('}'); }
    { bar(']'); }
    { bar(')'); }
}
"
   ))

;; This is a test for #103: a comment after the last struct member that does
;; not have a trailing comma. The comment used to be indented one stop too
;; far.
(ert-deftest indent-comment-after-last-struct-member ()
  (test-indent
   "
struct A {
    x: u8
    // comment
}

struct A {
    x: u8
    /* comment */
}
"
   ))

(setq rust-test-motion-string
      "
fn fn1(arg: int) -> bool {
    let x = 5;
    let y = b();
    true
}

fn fn2(arg: int) -> bool {
    let x = 5;
    let y = b();
    true
}

pub fn fn3(arg: int) -> bool {
    let x = 5;
    let y = b();
    true
}

struct Foo {
    x: int
}
"
      rust-test-region-string rust-test-motion-string
      rust-test-indent-motion-string
      "
fn blank_line(arg:int) -> bool {

}

fn indenting_closing_brace() {
    if(true) {
}
}

fn indenting_middle_of_line() {
    if(true) {
 push_me_out();
} else {
               pull_me_back_in();
}
}

fn indented_already() {

    // The previous line already has its spaces
}
"

      ;; Symbol -> (line column)
      rust-test-positions-alist '((start-of-fn1 (2 0))
                                  (start-of-fn1-middle-of-line (2 15))
                                  (middle-of-fn1 (3 7))
                                  (end-of-fn1 (6 0))
                                  (between-fn1-fn2 (7 0))
                                  (start-of-fn2 (8 0))
                                  (middle-of-fn2 (10 4))
                                  (before-start-of-fn1 (1 0))
                                  (after-end-of-fn2 (13 0))
                                  (beginning-of-fn3 (14 0))
                                  (middle-of-fn3 (16 4))
                                  (middle-of-struct (21 10))
                                  (before-start-of-struct (19 0))
                                  (after-end-of-struct (23 0))
                                  (blank-line-indent-start (3 0))
                                  (blank-line-indent-target (3 4))
                                  (closing-brace-indent-start (8 1))
                                  (closing-brace-indent-target (8 5))
                                  (middle-push-indent-start (13 2))
                                  (middle-push-indent-target (13 9))
                                  (after-whitespace-indent-start (13 1))
                                  (after-whitespace-indent-target (13 8))
                                  (middle-pull-indent-start (15 19))
                                  (middle-pull-indent-target (15 12))
                                  (blank-line-indented-already-bol-start (20 0))
                                  (blank-line-indented-already-bol-target (20 4))
                                  (blank-line-indented-already-middle-start (20 2))
                                  (blank-line-indented-already-middle-target (20 4))
                                  (nonblank-line-indented-already-bol-start (21 0))
                                  (nonblank-line-indented-already-bol-target (21 4))
                                  (nonblank-line-indented-already-middle-start (21 2))
                                  (nonblank-line-indented-already-middle-target (21 4))))

(defun rust-get-buffer-pos (pos-symbol)
  "Get buffer position from POS-SYMBOL.

POS-SYMBOL is a symbol found in `rust-test-positions-alist'.
Convert the line-column information from that list into a buffer position value."
  (interactive "P")
  (let* (
         (line-and-column (cadr (assoc pos-symbol rust-test-positions-alist)))
         (line (nth 0 line-and-column))
         (column (nth 1 line-and-column)))
    (save-excursion
      (goto-line line)
      (move-to-column column)
      (point))))

;;; FIXME: Maybe add an ERT explainer function (something that shows the
;;; surrounding code of the final point, not just the position).
(defun rust-test-motion (source-code init-pos final-pos manip-func &optional &rest args)
  "Test that MANIP-FUNC moves point from INIT-POS to FINAL-POS.

If ARGS are provided, send them to MANIP-FUNC.

INIT-POS, FINAL-POS are position symbols found in `rust-test-positions-alist'."
  (with-temp-buffer
    (rust-mode)
    (insert source-code)
    (font-lock-fontify-buffer)
    (goto-char (rust-get-buffer-pos init-pos))
    (apply manip-func args)
    (should (equal (point) (rust-get-buffer-pos final-pos)))))

(defun rust-test-region (source-code init-pos reg-beg reg-end manip-func &optional &rest args)
  "Test that MANIP-FUNC marks region from REG-BEG to REG-END.

INIT-POS is the initial position of point.
If ARGS are provided, send them to MANIP-FUNC.
All positions are position symbols found in `rust-test-positions-alist'."
  (with-temp-buffer
    (rust-mode)
    (insert source-code)
    (font-lock-fontify-buffer)
    (goto-char (rust-get-buffer-pos init-pos))
    (apply manip-func args)
    (should (equal (list (region-beginning) (region-end))
                   (list (rust-get-buffer-pos reg-beg)
                         (rust-get-buffer-pos reg-end))))))

(ert-deftest rust-beginning-of-defun-from-middle-of-fn ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn1
   'start-of-fn1
   #'beginning-of-defun))

(ert-deftest rust-beginning-of-defun-from-end ()
  (rust-test-motion
   rust-test-motion-string
   'end-of-fn1
   'start-of-fn1
   #'beginning-of-defun))

(ert-deftest rust-beginning-of-defun-before-open-brace ()
  (rust-test-motion
   rust-test-motion-string
   'start-of-fn1-middle-of-line
   'start-of-fn1
   #'beginning-of-defun))

(ert-deftest rust-beginning-of-defun-between-fns ()
  (rust-test-motion
   rust-test-motion-string
   'between-fn1-fn2
   'start-of-fn1
   #'beginning-of-defun))

(ert-deftest rust-beginning-of-defun-with-arg ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn2
   'start-of-fn1
   #'beginning-of-defun 2))

(ert-deftest rust-beginning-of-defun-with-negative-arg ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn1
   'beginning-of-fn3
   #'beginning-of-defun -2))

(ert-deftest rust-beginning-of-defun-pub-fn ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn3
   'beginning-of-fn3
   #'beginning-of-defun))

(ert-deftest rust-end-of-defun-from-middle-of-fn ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn1
   'between-fn1-fn2
   #'end-of-defun))

(ert-deftest rust-end-of-defun-from-beg ()
  (rust-test-motion
   rust-test-motion-string
   'start-of-fn1
   'between-fn1-fn2
   #'end-of-defun))

(ert-deftest rust-end-of-defun-before-open-brace ()
  (rust-test-motion
   rust-test-motion-string
   'start-of-fn1-middle-of-line
   'between-fn1-fn2
   #'end-of-defun))

(ert-deftest rust-end-of-defun-between-fns ()
  (rust-test-motion
   rust-test-motion-string
   'between-fn1-fn2
   'after-end-of-fn2
   #'end-of-defun))

(ert-deftest rust-end-of-defun-with-arg ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn1
   'after-end-of-fn2
   #'end-of-defun 2))

(ert-deftest rust-end-of-defun-with-negative-arg ()
  (rust-test-motion
   rust-test-motion-string
   'middle-of-fn3
   'between-fn1-fn2
   #'end-of-defun -2))

(ert-deftest rust-mark-defun-from-middle-of-fn ()
  (rust-test-region
   rust-test-region-string
   'middle-of-fn2
   'between-fn1-fn2 'after-end-of-fn2
   #'mark-defun))

(ert-deftest rust-mark-defun-from-end ()
  (rust-test-region
   rust-test-region-string
   'end-of-fn1
   'before-start-of-fn1 'between-fn1-fn2
   #'mark-defun))

(ert-deftest rust-mark-defun-start-of-defun ()
  (rust-test-region
   rust-test-region-string
   'start-of-fn2
   'between-fn1-fn2 'after-end-of-fn2
   #'mark-defun))

(ert-deftest rust-mark-defun-from-middle-of-struct ()
  (rust-test-region
   rust-test-region-string
   'middle-of-struct
   'before-start-of-struct 'after-end-of-struct
   #'mark-defun))

(ert-deftest indent-line-blank-line-motion ()
  (rust-test-motion
   rust-test-indent-motion-string
   'blank-line-indent-start
   'blank-line-indent-target
   #'indent-for-tab-command))

(ert-deftest indent-line-closing-brace-motion ()
  (rust-test-motion
   rust-test-indent-motion-string
   'closing-brace-indent-start
   'closing-brace-indent-target
   #'indent-for-tab-command))

(ert-deftest indent-line-middle-push-motion ()
  (rust-test-motion
   rust-test-indent-motion-string
   'middle-push-indent-start
   'middle-push-indent-target
   #'indent-for-tab-command))

(ert-deftest indent-line-after-whitespace-motion ()
  (rust-test-motion
   rust-test-indent-motion-string
   'after-whitespace-indent-start
   'after-whitespace-indent-target
   #'indent-for-tab-command))

(ert-deftest indent-line-middle-pull-motion ()
  (rust-test-motion
   rust-test-indent-motion-string
   'middle-pull-indent-start
   'middle-pull-indent-target
   #'indent-for-tab-command))

(ert-deftest indent-line-blank-line-indented-already-bol ()
  (rust-test-motion
   rust-test-indent-motion-string
   'blank-line-indented-already-bol-start
   'blank-line-indented-already-bol-target
   #'indent-for-tab-command))

(ert-deftest indent-line-blank-line-indented-already-middle ()
  (rust-test-motion
   rust-test-indent-motion-string
   'blank-line-indented-already-middle-start
   'blank-line-indented-already-middle-target
   #'indent-for-tab-command))

(ert-deftest indent-line-nonblank-line-indented-already-bol ()
  (rust-test-motion
   rust-test-indent-motion-string
   'nonblank-line-indented-already-bol-start
   'nonblank-line-indented-already-bol-target
   #'indent-for-tab-command))

(ert-deftest indent-line-nonblank-line-indented-already-middle ()
  (rust-test-motion
   rust-test-indent-motion-string
   'nonblank-line-indented-already-middle-start
   'nonblank-line-indented-already-middle-target
   #'indent-for-tab-command))

(defun rust-test-fontify-string (str)
  (with-temp-buffer
    (rust-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun rust-test-group-str-by-face (str)
  "Fontify `STR' in rust-mode and group it by face, returning a
list of substrings of `STR' each followed by its face."
  (loop with fontified = (rust-test-fontify-string str)
        for start = 0 then end
        while start
        for end   = (next-single-property-change start 'face fontified)
        for prop  = (get-text-property start 'face fontified)
        for text  = (substring-no-properties fontified start end)
        if prop
        append (list text prop)))

(defun rust-test-font-lock (source face-groups)
  "Test that `SOURCE' fontifies to the expected `FACE-GROUPS'"
  (should (equal (rust-test-group-str-by-face source)
                 face-groups)))

(ert-deftest font-lock-attribute-simple ()
  (rust-test-font-lock
   "#[foo]"
   '("#[foo]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-inner ()
  (rust-test-font-lock
   "#![foo]"
   '("#![foo]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-key-value ()
  (rust-test-font-lock
   "#[foo = \"bar\"]"
   '("#[foo = " font-lock-preprocessor-face
     "\"bar\"" font-lock-string-face
     "]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-around-comment ()
  (rust-test-font-lock
   "#[foo /* bar */]"
   '("#[foo " font-lock-preprocessor-face
     "/* " font-lock-comment-delimiter-face
     "bar */" font-lock-comment-face
     "]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-inside-string ()
  (rust-test-font-lock
   "\"#[foo]\""
   '("\"#[foo]\"" font-lock-string-face)))

(ert-deftest font-lock-attribute-inside-comment ()
  (rust-test-font-lock
   "/* #[foo] */"
   '("/* " font-lock-comment-delimiter-face
     "#[foo] */" font-lock-comment-face)))

(ert-deftest font-lock-double-quote-character-literal ()
  (rust-test-font-lock
   "'\"'; let"
   '("'\"'" font-lock-string-face
     "let" font-lock-keyword-face)))

(ert-deftest font-lock-fn-contains-capital ()
  (rust-test-font-lock
   "fn foo_Bar() {}"
   '("fn" font-lock-keyword-face
     "foo_Bar" font-lock-function-name-face)))

(ert-deftest font-lock-single-quote-character-literal ()
  (rust-test-font-lock
   "fn main() { let ch = '\\''; }"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "let" font-lock-keyword-face
     "'\\''" font-lock-string-face)))

(ert-deftest font-lock-escaped-double-quote-character-literal ()
  (rust-test-font-lock
   "fn main() { let ch = '\\\"'; }"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "let" font-lock-keyword-face
     "'\\\"'" font-lock-string-face)))

(ert-deftest font-lock-escaped-backslash-character-literal ()
  (rust-test-font-lock
   "fn main() { let ch = '\\\\'; }"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "let" font-lock-keyword-face
     "'\\\\'" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-no-hashes ()
  (rust-test-font-lock
   "r\"No hashes\";"
   '("r\"No hashes\"" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-double-quote ()
  (rust-test-font-lock
   "fn main() {
    r#\"With a double quote (\")\"#;
}
"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "r#\"With a double quote (\")\"#" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-two-hashes ()
  (rust-test-font-lock
   "r##\"With two hashes\"##;"
   '("r##\"With two hashes\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-backslash-at-end ()
  (rust-test-font-lock
   "r\"With a backslash at the end\\\";"
   '("r\"With a backslash at the end\\\"" font-lock-string-face)))

(ert-deftest font-lock-two-raw-strings ()
  (rust-test-font-lock
   "fn main() {
    r\"With a backslash at the end\\\";
    r##\"With two hashes\"##;
}"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "r\"With a backslash at the end\\\"" font-lock-string-face
     "r##\"With two hashes\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-with-inner-hash ()
  (rust-test-font-lock
   "r##\"I've got an octothorpe (#)\"##; foo()"
   '("r##\"I've got an octothorpe (#)\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-with-inner-quote-and-hash ()
  (rust-test-font-lock
   "not_the_string(); r##\"string \"# still same string\"##; not_the_string()"
   '("r##\"string \"# still same string\"##" font-lock-string-face)))

(ert-deftest font-lock-string-ending-with-r-not-raw-string ()
  (rust-test-font-lock
   "fn f() {
    \"Er\";
}

fn g() {
    \"xs\";
}"
   '("fn" font-lock-keyword-face
     "f" font-lock-function-name-face
     "\"Er\"" font-lock-string-face
     "fn" font-lock-keyword-face
     "g" font-lock-function-name-face
     "\"xs\"" font-lock-string-face)))

(ert-deftest font-lock-raw-string-trick-ending-followed-by-string-with-quote ()
  (rust-test-font-lock
   "r\"With what looks like the start of a raw string at the end r#\";
not_a_string();
r##\"With \"embedded\" quote \"##;"
   '("r\"With what looks like the start of a raw string at the end r#\"" font-lock-string-face
     "r##\"With \"embedded\" quote \"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-starter-inside-raw-string ()
  ;; Check that it won't look for a raw string beginning inside another raw string.
  (rust-test-font-lock
   "r#\"In the first string r\" in the first string \"#;
not_in_a_string();
r##\"In the second string\"##;"
   '("r#\"In the first string r\" in the first string \"#" font-lock-string-face
     "r##\"In the second string\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-starter-inside-comment ()
  ;; Check that it won't look for a raw string beginning inside another raw string.
  (rust-test-font-lock
   "// r\" this is a comment
\"this is a string\";
this_is_not_a_string();)"
   '("// " font-lock-comment-delimiter-face
     "r\" this is a comment\n" font-lock-comment-face
     "\"this is a string\"" font-lock-string-face)))

(ert-deftest font-lock-raw-string-constant ()
  ;; There was an issue in which a multi-line raw string would be fontified
  ;; correctly if inserted, but then incorrectly if one of the lines was then
  ;; edited.  This test replicates how font-lock responds when text in the
  ;; buffer is modified in order to reproduce it.
  (with-temp-buffer
    (rust-mode)
    (font-lock-fontify-buffer)
    (insert "const BOO:&str = r#\"\nBOO\"#;")
    (beginning-of-buffer)
    (insert " ")
    (font-lock-after-change-function 1 2 0)

    (should (equal 'font-lock-string-face (get-text-property 19 'face))) ;; Opening "r" of raw string
    (should (equal 'font-lock-string-face (get-text-property 27 'face))) ;; Closing "#" of raw string
    (should (equal nil (get-text-property 28 'face))) ;; Semicolon--should not be part of the string
    ))

(ert-deftest font-lock-runaway-raw-string ()
  (rust-test-font-lock
   "const Z = r#\"my raw string\";\n// oops this is still in the string"
   '("const" font-lock-keyword-face
     "Z" font-lock-type-face
     "r#\"my raw string\";\n// oops this is still in the string" font-lock-string-face))
  )

(ert-deftest font-lock-recognize-closing-raw-string ()
  (with-temp-buffer
    (rust-mode)
    (insert "const foo = r##\"
1...............................................50
1...............................................50
1...............................................50
1...............195-->\"; let ...................50
1...............................................50
1...............................................50
1...............................................50
1...............................................50
1...............................................50
1......................500......................50
\"#;
")
    (font-lock-fontify-buffer)
    (goto-char 530)
    (insert "#")
    ;; We have now closed the raw string.  Check that the whole string is
    ;; recognized after the change
    (font-lock-after-change-function (1- (point)) (point) 0)
    (should (equal 'font-lock-string-face (get-text-property 195 'face))) ;; The "let"
    (should (equal 'font-lock-string-face (get-text-property 500 'face))) ;; The "500"
    (should (equal nil (get-text-property 531 'face))) ;; The second ";"
    ))

;;; Documentation comments

(ert-deftest font-lock-doc-line-comment-parent ()
  (rust-test-font-lock
   "//! doc"
   '("//! doc" font-lock-doc-face)))

(ert-deftest font-lock-doc-line-comment-item ()
  (rust-test-font-lock
   "/// doc"
   '("/// doc" font-lock-doc-face)))

(ert-deftest font-lock-nondoc-line ()
  (rust-test-font-lock
   "////// doc"
   '("////// " font-lock-comment-delimiter-face
     "doc" font-lock-comment-face)))

(ert-deftest font-lock-doc-line-in-string ()
  (rust-test-font-lock
   "\"/// doc\""
   '("\"/// doc\"" font-lock-string-face))

  (rust-test-font-lock
   "\"//! doc\""
   '("\"//! doc\"" font-lock-string-face)))

(ert-deftest font-lock-doc-line-in-nested-comment ()
  (rust-test-font-lock
   "/* /// doc */"
   '("/* " font-lock-comment-delimiter-face
     "/// doc */" font-lock-comment-face))

  (rust-test-font-lock
   "/* //! doc */"
   '("/* " font-lock-comment-delimiter-face
     "//! doc */" font-lock-comment-face)))


(ert-deftest font-lock-doc-block-comment-parent ()
  (rust-test-font-lock
   "/*! doc */"
   '("/*! doc */" font-lock-doc-face)))

(ert-deftest font-lock-doc-block-comment-item ()
  (rust-test-font-lock
   "/** doc */"
   '("/** doc */" font-lock-doc-face)))

(ert-deftest font-lock-nondoc-block-comment-item ()
  (rust-test-font-lock
   "/***** doc */"
   '("/**" font-lock-comment-delimiter-face
     "*** doc */" font-lock-comment-face)))

(ert-deftest font-lock-doc-block-in-string ()
  (rust-test-font-lock
   "\"/** doc */\""
   '("\"/** doc */\"" font-lock-string-face))
  (rust-test-font-lock
   "\"/*! doc */\""
   '("\"/*! doc */\"" font-lock-string-face)))


(ert-deftest indent-method-chains-no-align ()
  (let ((rust-indent-method-chain nil)) (test-indent
   "
fn main() {
    let x = thing.do_it()
        .aligned()
        .more_alignment();
}
"
   )))

(ert-deftest indent-method-chains-with-align ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    let x = thing.do_it()
                 .aligned()
                 .more_alignment();
}
"
   )))

(ert-deftest indent-method-chains-with-align-and-second-stmt ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    let x = thing.do_it()
                 .aligned()
                 .more_alignment();
    foo.bar();
}
"
   )))

(ert-deftest indent-method-chains-field ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    let x = thing.do_it
                 .aligned
                 .more_alignment();
}
"
   )))

(ert-deftest indent-method-chains-double-field-on-first-line ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    let x = thing.a.do_it
                   .aligned
                   .more_alignment();
}
"
   )))

(ert-deftest indent-method-chains-no-let ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    thing.a.do_it
           .aligned
           .more_alignment();
}
"
   )))

(ert-deftest indent-method-chains-comment ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    // thing.do_it()
    // .aligned()
}
"
   )))

(ert-deftest indent-method-chains-close-block ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() {
    foo.bar()
}
"
   )))

(ert-deftest indent-method-chains-after-comment ()
  (let ((rust-indent-method-chain t)) (test-indent
   "
fn main() { // comment here should not push next line out
    foo.bar()
}
"
   )))

(ert-deftest test-for-issue-36-syntax-corrupted-state ()
  "This is a test for a issue #36, which involved emacs's
internal state getting corrupted when actions were done in a
specific sequence.  The test seems arbitrary, and is, but it was
not clear how to narrow it down further.

The cause of the bug was code that used to set
`syntax-begin-function' to `beginning-of-defun', which doesn't
actually fulfill the expectations--`syntax-begin-function' is
supposed to back out of all parens, but `beginning-of-defun'
could leave it inside parens if a fn appears inside them.

Having said that, as I write this I don't understand fully what
internal state was corrupted and how.  There wasn't an obvious
pattern to what did and did not trip it."
  
  ;; When bug #36 was present, the following test would pass, but running it
  ;; caused some unknown emacs state to be corrupted such that the following
  ;; test failed.  Both the "blank_line" and "indented_closing_brace" functions
  ;; were needed to expose the error, for instance--deleting either of them
  ;; would make the failure go away.
  (with-temp-buffer
    (rust-mode)
    (insert "fn blank_line(arg:int) -> bool {

}

fn indenting_closing_brace() {
    if(true) {
}
}

fn indented_already() {
    \n    // The previous line already has its spaces
}
")
    (font-lock-fontify-buffer)
    (goto-line 11)
    (move-to-column 0)
    (indent-for-tab-command)
    (should (equal (current-column) 4))
    )

  ;; This is the test that would fail only after running the previous one.  The
  ;; code is extracted from src/libstd/collections/table.rs in the rust tree.
  ;; It was not clear how to reduce it further--removing various bits of it
  ;; would make it no longer fail.  In particular, changing only the comment at
  ;; the top of the "next" function was sufficient to make it no longer fail.
  (test-indent
   "
impl Foo for Bar {
    
    /// Modifies the bucket pointer in place to make it point to the next slot.
    pub fn next(&mut self) {
        // Branchless bucket
        // As we reach the end of the table...
        // We take the current idx:          0111111b
        // Xor it by its increment:        ^ 1000000b
        //                               ------------
        //                                   1111111b
        // Then AND with the capacity:     & 1000000b
        //                               ------------
        // to get the backwards offset:      1000000b
        let maybe_wraparound_dist = (self.idx ^ (self.idx + 1)) & self.table.capacity();
        // Finally, we obtain the offset 1 or the offset -cap + 1.
        let dist = 1 - (maybe_wraparound_dist as isize);
        
        self.idx += 1;
        
        unsafe {
            self.raw = self.raw.offset(dist);
        }
    }
    
    /// Reads a bucket at a given index, returning an enum indicating whether
    /// the appropriate types to call most of the other functions in
    /// this module.
    pub fn peek(self) {
        match foo {
            EMPTY_BUCKET =>
                Empty(EmptyBucket {
                    raw: self.raw,
                    idx: self.idx,
                    table: self.table
                }),
            _ =>
                Full(FullBucket {
                    raw: self.raw,
                    idx: self.idx,
                    table: self.table
                })
        }
    }    
}
"
   ))

(ert-deftest test-indent-string-with-eol-backslash ()
  (test-indent
   "
pub fn foo() {
    format!(\"abc \\
             def\")
}
"
   ))

(ert-deftest test-indent-string-with-eol-backslash-at-start ()
  (test-indent
   "
pub fn foo() {
    format!(\"\\
        abc \\
        def\")
}
"
   ))

(ert-deftest test-indent-string-without-eol-backslash-indent-is-not-touched ()
  (test-indent
   "
pub fn foo() {
    format!(\"
abc
def\");
}

pub fn foo() {
    format!(\"la la la
la
la la\");
}
"
   ;; Should still indent the code parts but leave the string internals alone:
   "
         pub fn foo() {
    format!(\"
abc
def\");
}

pub fn foo() {
    format!(\"la la la
la
la la\");
   }
"
   ))

(ert-deftest test-indent-string-eol-backslash-mixed-with-literal-eol ()
  (test-indent
   "
fn foo() {
    println!(\"
Here is the beginning of the string
            and here is a line that is arbitrarily indented \\
            and a continuation of that indented line
     and another arbitrary indentation
  still another
        yet another \\
        with a line continuing it
And another line not indented
\")
}
"
   "
fn foo() {
    println!(\"
Here is the beginning of the string
            and here is a line that is arbitrarily indented \\
            and a continuation of that indented line
     and another arbitrary indentation
  still another
        yet another \\
with a line continuing it
And another line not indented
\")
}
"))

(ert-deftest test-indent-string-eol-backslash-dont-touch-raw-strings ()
  (test-indent
   "
pub fn foo() {
    format!(r\"\
abc\
         def\");
}

pub fn foo() {
    format!(r\"la la la
    la\
la la\");
}
"
   ;; Should still indent the code parts but leave the string internals alone:
   "
    pub fn foo() {
    format!(r\"\
abc\
         def\");
}

pub fn foo() {
          format!(r\"la la la
    la\
la la\");
}
"
   ))

(ert-deftest indent-inside-string-first-line ()
  (test-indent
   ;; Needs to leave 1 space before "world"
   "\"hello \\\n world\""))

(ert-deftest indent-multi-line-type-param-list ()
  (test-indent
   "
pub fn foo<T,
           V>() {
    hello();
}"))

(defun rust-test-matching-parens (content pairs &optional nonparen-positions)
  "Assert that in rust-mode, given a buffer with the given `content',
  emacs's paren matching will find all of the pairs of positions
  as matching braces.  The list of nonparen-positions asserts
  specific positions that should NOT be considered to be
  parens/braces of any kind.

  This does not assert that the `pairs' list is
  comprehensive--there can be additional pairs that don't appear
  in the list and the test still passes (as long as none of their
  positions appear in `nonparen-positions'.)"
  (with-temp-buffer
    (rust-mode)
    (insert content)
    (font-lock-fontify-buffer)
    (dolist (pair pairs)
      (let* ((open-pos (nth 0 pair))
             (close-pos (nth 1 pair)))
        (should (equal 4 (syntax-class (syntax-after open-pos))))
        (should (equal 5 (syntax-class (syntax-after close-pos))))
        (should (equal (scan-sexps open-pos 1) (+ 1 close-pos)))
        (should (equal (scan-sexps (+ 1 close-pos) -1) open-pos))))
    (dolist (nonpar-pos nonparen-positions)
      (let ((nonpar-syntax-class (syntax-class (syntax-after nonpar-pos))))
        (should (not (equal 4 nonpar-syntax-class)))
        (should (not (equal 5 nonpar-syntax-class)))))))

(ert-deftest rust-test-unmatched-single-quote-in-comment-paren-matching ()
  ;; This was a bug from the char quote handling that affected the paren
  ;; matching.  An unmatched quote char in a comment caused the problems.
  (rust-test-matching-parens
   "// If this appeared first in the file...
\"\\
{\";

// And the { was not the on the first column:
 {
    // This then messed up the paren matching: '\\'
}

"
   '((97 150) ;; The { and } at the bottom
     )))

(ert-deftest rust-test-two-character-quotes-in-a-row ()
  (with-temp-buffer
    (rust-mode)
    (font-lock-fontify-buffer)
    (insert "'\\n','a', fn")
    (font-lock-after-change-function 1 12 0)

    (should (equal 'font-lock-string-face (get-text-property 3 'face)))
    (should (equal nil (get-text-property 5 'face)))
    (should (equal 'font-lock-string-face (get-text-property 7 'face)))
    (should (equal nil (get-text-property 9 'face)))
    (should (equal 'font-lock-keyword-face (get-text-property 12 'face)))
    )  
  )

(ert-deftest single-quote-null-char ()
  (rust-test-font-lock
   "'\\0' 'a' fn"
   '("'\\0'" font-lock-string-face
     "'a'" font-lock-string-face
     "fn" font-lock-keyword-face)))

(ert-deftest r-in-string-after-single-quoted-double-quote ()
  (rust-test-font-lock
   "'\"';\n\"r\";\n\"oops\";"
   '("'\"'" font-lock-string-face
     "\"r\"" font-lock-string-face
     "\"oops\"" font-lock-string-face
     )))

(ert-deftest char-literal-after-quote-in-raw-string ()
  (rust-test-font-lock
   "r#\"\"\"#;\n'q'"
   '("r#\"\"\"#" font-lock-string-face
     "'q'" font-lock-string-face)))

(ert-deftest rust-test-basic-paren-matching ()
  (rust-test-matching-parens
   "
fn foo() {
    let a = [1, 2, 3];
}"
   '((8 9) ;; Parens of foo()
     (11 36) ;; Curly braces
     (25 33) ;; Square brackets
   )))

(ert-deftest rust-test-paren-matching-generic-fn ()
  (rust-test-matching-parens
   "
fn foo<A>() {
}"
   '((8 10) ;; Angle brackets <A>
     (11 12) ;; Parens
     (14 16) ;; Curly braces
     )))

(ert-deftest rust-test-paren-matching-generic-fn-with-return-value ()
  (rust-test-matching-parens
   "
fn foo<A>() -> bool {
    false
}"
   '((8 10) ;; Angle brackets <A>
     (11 12) ;; Parens
     (22 34) ;; Curly braces
     )
   
   '(15 ;; The ">" in "->" is not an angle bracket
     )))

(ert-deftest rust-test-paren-matching-match-stmt ()
  (rust-test-matching-parens
   "
fn foo() {
    something_str(match <Type as Trait>::method() {
        Some(_) => \"Got some\",
        None => \"Nada\"
    });
}"
   '((8 9) ;; parens of fn foo
     (11 127) ;; curly braces of foo
     (30 124) ;; parens of something_str
     (37 51) ;; angle brackets of <Type as Trait>
     (60 61) ;; parens of method()
     (63 123) ;; curly braces of match
     (77 79) ;; parens of Some(_)
     )
   
   '(82 ;; > in first =>
     112 ;; > in second =>
     )))

(ert-deftest rust-test-paren-matching-bitshift-operators ()
  (rust-test-matching-parens
  "
fn foo(z:i32) {
    let a:Option<Result<i32,i32>> = Some(Ok(4 >> 1));
    let b = a.map(|x| x.map(|y| y << 3));
    let trick_question = z<<<Type as Trait>::method();  // First two <s are not brackets, third is
}"
    '((34 50) ;; angle brackets of Option
      (41 49) ;; angle brackets of Result
      (142 156) ;; angle brackets of <Type as Trait>
      )
    '(64 ;; The >> inside Some(Ok()) are not angle brackets
      65 ;; The >> inside Some(Ok()) are not angle brackets
      106 ;; The << inside map() are not angle brackets
      107 ;; The << inside map() are not angle brackets
      140 ;; The << before <Type as Trait> are not angle brackets
      141 ;; The << before <Type as Trait> are not angle brackets
      183 ;; The < inside the comment
      )))

(ert-deftest rust-test-paren-matching-angle-bracket-after-colon-ident ()
  (rust-test-matching-parens
   "
struct Bla<T> {
    a:Option<(i32,Option<bool>)>,
    b:Option<T>,
    c:bool
}

fn f(x:i32,y:Option<i32>) {
    let z:Option<i32> = None;
    let b:Bla<i8> = Bla{
        a:None,
        b:None,
        c:x<y.unwrap();
    }
}"
   '((12 14) ;; Angle brackets of Bla<T>
     (30 49) ;; Outer angle brackets of a:Option<...>
     (42 47) ;; Inner angle brackets of Option<bool>
     (64 66) ;; Angle brackets of Option<T>
     (102 106) ;; Angle brackets of y:Option<i32>
     (127 131) ;; Angle brackets of z:Option<i32>
     (154 157) ;; Angle brackets of b:Bla<i8>
     )
   '(209 ;; less than operator in c:x<y.unwrap...
     )))

(ert-deftest rust-test-paren-matching-struct-literals ()
  (rust-test-matching-parens
   "
fn foo(x:i32) -> Bar {
    Bar {
        b:x<3
    }
}"
  '()
  '(17 ;; the -> is not a brace
    46 ;; x<3 the < is a less than sign
    ))
  )

(ert-deftest rust-test-paren-matching-nested-struct-literals ()
  (rust-test-matching-parens
   "
fn f(x:i32,y:i32) -> Foo<Bar> {
    Foo{
        bar:Bar{
            a:3,
            b:x<y
        }
    }
}
"
   '((26 30)) ;; Angle brackets of Foo<Bar>
   )
  '(92 ;; less than operator x<y
    ))

(ert-deftest rust-test-paren-matching-fn-types-in-type-params ()
  (rust-test-matching-parens
   "
fn foo<T:Fn() -> X<Y>>() -> Z {
}
"
   '((8 23) ;; The angle brackets of foo<T...>
     (20 22 ;; The angle brackets of X<Y>
         ))
   '(17 ;; The first ->
     28 ;; The second ->
     )
   ))

(ert-deftest rust-test-paren-matching-lt-ops-in-fn-params ()
  (rust-test-matching-parens
   "
fn foo(x:i32) {
    f(x < 3);
}
"
   '()
   '(26 ;; The < inside f is a less than operator
     )
   ))

(ert-deftest rust-test-paren-matching-lt-ops-in-fn-params ()
  (rust-test-matching-parens
   "
fn foo(x:i32) -> bool {
    return x < 3;
}
"
   '()
   '(17 ;; The ->
     39 ;; The < after return is a less than operator
     )
   ))

(ert-deftest rust-test-type-paren-matching-angle-brackets-in-type-items ()
  (rust-test-matching-parens
   "
type Foo = Blah<Z,Y>;
type Bar<X> = (Foo, Bletch<X>);
type ThisThing<Z,A,D,F> = HereYouGo<Z,Y<Fn(A) -> B<C<D>>,E<F>>>;"
   '((17 21) ;; Angle brackets of Blah<Z,Y>
     (32 34) ;; Angle brackets of Bar<X>
     (50 52) ;; Angle brackets of Bletch<X>
     (70 78) ;; Angle brackets of ThisThing<Z,A,D,F>
     (91 118) ;; Angle brackets of HereYouGo<...>
     (95 117) ;; Angle brackets of Y<Fn...>
     (106 111) ;; Angle brackets of B<C<D>>
     (108 110) ;; Angle brackets of C<D>
     (114 116) ;; Angle brackets of E<F>
     )))

(ert-deftest rust-test-paren-matching-tuple-like-struct ()
  (rust-test-matching-parens
   "
struct A(Option<B>);
struct C<Q>(Result<Q,i32>);"
   '((17 19) ;; The angle brackets <B>
     (10 20) ;; The parens of A();
     (31 33) ;; The angle brackets of C<Q>
     (41 47) ;; The angle brackets of Result<Q,i32>
     )
   '()))

(ert-deftest rust-test-paren-matching-in-enum ()
  (rust-test-matching-parens
   "
enum Boo<A> {
    TupleLike(Option<A>),
    StructLike{foo: Result<A,i32>}
}"
   '((10 12) ;; Angle brackets of Boo<A>
     (36 38) ;; Angle brackets of Option<A>
     (68 74) ;; Angle brackets of Result<A,i32>
     )))

(ert-deftest rust-test-paren-matching-assoc-type-bounds ()
  (rust-test-matching-parens
   "impl <A:B<AssocType = C<A> >> Thing<A> {}"
   '((6 29) ;; Outer angle brackets of impl
     (10 28) ;; Outer angle brackets of B<AssocType = C<A>>
     (24 26) ;; Inner angle brackets of C<A>
     (36 38) ;; Angle brackets of Thing<A>
     )
   ))

(ert-deftest rust-test-paren-matching-plus-signs-in-expressions-and-bounds ()
  ;; Note that as I write this, the function "bluh" below does not compile, but
  ;; it warns that the equality constraint in a where clause is "not yet
  ;; supported."  It seems that the compiler will support this eventually, so
  ;; the emacs mode needs to support it.
  (rust-test-matching-parens
   "fn foo<A:Trait1+Trait2<i32>,B>(a:A,b:B) -> bool where B:Trait3<Foo>+Trait4<Bar> {
    2 + a < 3 && 3 + b > 11
}

fn bluh<A>() where A:Fn()+MyTrait<i32>, MyTrait<A>::AssocType = Option<bool> {
}

fn fluh<C>() where C:Fn(i32) -> (i32, i32) + SomeTrait<i32>, C::AssocType = OtherThing<bool> {
}"
   '((7 30) ;; Angle brackets of foo<...>
     (23 27) ;; Angle brackets of Trait2<i32>
     (63 67) ;; Angle brackets of Trait3<Foo>
     (75 79) ;; Angle brackets of Trait4<Bar>

     (121 123) ;; Angle brackets of bluh<A>
     (147 151) ;; Angle brackets of MyTrait<i32>

     (161 163) ;; Angle brackets of MyTrait<A>
     (184 189) ;; Angle brackets of Option<bool>

     (203 205) ;; Angle brackets of <C>
     (250 254) ;; Angle brackets of SomeTrait<i32>
     (282 287) ;; Angle brackets of Option<bool>
     )
   '(93 ;; Less-than sign of a < 3
     106 ;; Greater than sign of b > 11
     )))

(ert-deftest rust-test-paren-matching-generic-type-in-tuple-return-type ()
  (rust-test-matching-parens
   "pub fn take(mut self) -> (EmptyBucket<K, V, M>, K, V) {}"
   '((38 46))
   ))

(ert-deftest rust-test-paren-matching-references-and-logical-and ()
  (rust-test-matching-parens
   "
fn ampersand_check(a: &Option<i32>, b:bool) -> &Option<u32> {
    a.map(|x| {
        b && x < 32
    })
}"
   '((31 35) ;; Option<i32>
     (56 60) ;; Option<u32>
     )
   '(95 ;; x < 32
     )
   )
  )

(ert-deftest rust-test-paren-matching-lt-sign-in-if-statement ()
  (rust-test-matching-parens
   "
fn if_check(a:i32,b:i32,c:i32) {
    if a + b < c {
        
    }
    if a < b {
        
    }
    if (c < a) {
        
    }
}

fn while_check(x:i32,y:i32) -> bool {
    while x < y {
    }
    for x in y < x {
    }
    match y < z {
        true => (), _ => ()
    }
    return z < y;
}"
   '()
   '(48 ;; b < c
     78 ;; a < b
     109 ;; (c < a)

     184 ;; x < y
     211 ;; y < x
     235 ;; y < z
     288 ;; z < y
     )))

(ert-deftest rust-test-paren-matching-lt-expr-with-field ()
  (rust-test-matching-parens
   "fn foo() { x.y < 3 }"
   '()
   '(16 ;; x.y < 3
     )))

(ert-deftest rust-test-paren-matching-lt-expr-with-quote ()
  (rust-test-matching-parens
   "
fn quote_check() {
    'x' < y;
     \"y\" < x;
    r##\"z\"## < q;
    a <= 3 && b < '2'
}"
   '()
   '(29 ;; 'x' < y
     42 ;; "y" < x
     60 ;; r##"z"## < q
     71 ;; a <= '3'
     81 ;; b < '2'
     )))

(ert-deftest rust-test-paren-matching-keywords-capitalized-are-ok-type-names ()
  (rust-test-matching-parens
   "
fn foo() -> Box<i32> {
    let z:If<bool> = If(a < 3);
}"
   '((17 21) ;; Box<i32>
     (37 42) ;; If<bool>
     )
   '(51 ;; If(a < 3)
     )))

(ert-deftest rust-test-paren-matching-lt-expression-inside-macro ()
  (rust-test-matching-parens
   "fn bla() { assert!(x < y); }"
   '()
   '(22 ;; x < y
     )))

(ert-deftest rust-test-paren-matching-array-types-with-generics ()
  (rust-test-matching-parens
   "fn boo () -> [Option<i32>] {}"
   '((21 25))))

(ert-deftest rust-test-paren-matching-angle-bracket-inner-reference ()
  (rust-test-matching-parens
   "fn x() -> Option<&Node<T>> {}"
   '((17 26) ;; Option
     (23 25) ;; Node
     )))

(ert-deftest rust-test-paren-matching-lt-operator-after-semicolon ()
  (rust-test-matching-parens
   "fn f(x:i32) -> bool { (); x < 3 }"
   '()
   '(29
     )))

(ert-deftest rust-test-paren-matching-lt-operator-after-comma ()
  (rust-test-matching-parens
   "fn foo() {
    (e, a < b)
}"
   '((16 25) ;; The parens ()
     )
   '(22 ;; The < operator
     )))

(ert-deftest rust-test-paren-matching-lt-operator-after-let ()
  (rust-test-matching-parens
   "fn main() {
    let x = a < b;
}"
   '((11 32) ;; The { }
     )
   '(27 ;; The < operator
     )))

(ert-deftest rust-test-paren-matching-two-lt-ops-in-a-row ()
  (rust-test-matching-parens
   "fn next(&mut self) -> Option<<I as Iterator>::Item>"
   '((29 51) ;; Outer Option<>
     (30 44) ;; Inner <I as Iterator>
     )
   '(21
     )))

(ert-deftest rust-test-paren-matching-lt-after-caret ()
  (rust-test-matching-parens
   "fn foo() { x^2 < 3 }"
   '((10 20) ;; The { }
     )
   '(16 ;; The < operator
     )))

(ert-deftest rust-test-paren-matching-lt-operator-after-special-type ()
  (rust-test-matching-parens
   "fn foo() { low as uint <= c }"
   '((10 29))
   '(24)))

(ert-deftest rust-test-paren-matching-lt-operator-after-closing-curly-brace ()
  (rust-test-matching-parens
   "fn main() { if true {} a < 3 }"
   '((11 30)
     )
   '(26)))

(ert-deftest rust-test-paren-matching-const ()
  (rust-test-matching-parens
   "
const BLA = 1 << 3;
const BLUB = 2 < 4;"
   '()
   '(16
     17 ;; Both chars of the << in 1 << 3
     37 ;; The < in 2 < 4
     )))

(ert-deftest rust-test-paren-matching-c-like-enum ()
  (rust-test-matching-parens
   "
enum CLikeEnum {
    Two = 1 << 1,
    Four = 1 << 2 
}"
   '((17 56 ;; The { } of the enum
         ))
   '(31
     32 ;; The first <<
     50
     51 ;; The second <<
     )))

(ert-deftest rust-test-paren-matching-no-angle-brackets-in-macros ()
  (rust-test-matching-parens
   "
fn foo<A>(a:A) {
    macro_a!( foo::<ignore the bracets> );
    macro_b![ foo as Option<B> ];
}

macro_c!{
    struct Boo<D> {}
}"
   '((8 10))
   ;; Inside macros, it should not find any angle brackets, even if it normally
   ;; would
   '(38 ;; macro_a <
     57 ;; macro_a >
     89 ;; macro_b <
     91 ;; macro_b >
     123 ;; macro_c <
     125 ;; macro_d >
     )))

(ert-deftest rust-test-paren-matching-type-with-module-name ()
  (rust-test-matching-parens
   "
const X: libc::c_int = 1 << 2;
fn main() {
    let z: libc::c_uint = 1 << 4;
}
"
   '((43 79)) ;; The curly braces
   '(27
     28 ;; The first <<
     73
     74 ;; The second <<
     )))

(ert-deftest rust-test-paren-matching-qualififed-struct-literal ()
  (rust-test-matching-parens
   "
fn foo() -> Fn(asd) -> F<V> {
    let z = foo::Struct{ b: 1 << 4, c: 2 < 4  }
}"
   '((30 80) ;; Outer curly brackets
     )
   '(62
     63 ;; The shift operator
     73 ;; The less than operator
     )))

(ert-deftest rust-test-paren-matching-let-mut ()
  (rust-test-matching-parens
   "
fn f() {
    let mut b = 1 < 3;
    let mut i = 1 << 3;
}
"
   '()
   '(28 ;; 1 < 3
     51
     52 ;; 1 << 3
     )))

(ert-deftest rust-test-paren-matching-as-ref-type ()
  (rust-test-matching-parens
   "fn f() {
    let a = b as &Foo<Bar>;
}"
   '((31 35) ;; Angle brackets Foo<Bar>
     )))

(ert-deftest rust-test-paren-matching-type-ascription ()
  (rust-test-matching-parens
   "
fn rfc803() {
    let z = a < b:FunnkyThing<int>;
    let s = Foo {
        a: b < 3,
        b: d:CrazyStuff<int> < 3,
        c: 2 < x:CrazyStuff<uint>
    }
}"
   '((45 49) ;; FunkyThing<int>
     (111 115) ;; CrazyStuff<int>
     (149 154) ;; CrazyStuff<uint>
     )
   '(30 ;; a < b
     83 ;; b < 3
     117 ;; d... < 3
     135 ;; 2 < x
     )))

(ert-deftest rust-test-paren-matching-angle-brackets-in-enum-with-where-claause ()
  (rust-test-matching-parens
   "
enum MyEnum<T> where T:std::fmt::Debug {
    Thing(Option<T>)
}"
   '((13 15) ;; MyEnum<T>
     (59 61) ;; Option<T>
     )))

(ert-deftest rust-test-paren-matching-where-clauses-with-closure-types ()
  (rust-test-matching-parens
   "
enum Boo<'a,T> where T:Fn() -> Option<&'a str> + 'a {
   Thingy(Option<&'a T>)
}

fn foo<'a>() -> Result<B,C> where C::X: D<A>, B:FnMut() -> Option<Q>+'a {
    Foo(a < b)
}

type Foo<T> where T: Copy = Box<T>;
"
   '((10 15) ;; Boo<'a,T>
     (39 47) ;; Option<&'a str>
     (72 78) ;; Option<&'a T>

     (106 110) ;; Result<B,C>
     (125 127) ;; D<A>
     (149 151) ;; Option<Q>
     (184 186) ;; Foo<T>
     (207 209) ;; Box<T>
     )

   '(168 ;; Foo(a < b)
     )
   ))

(ert-deftest rust-test-angle-bracket-matching-turned-off ()
  (let ((rust-match-angle-brackets nil))
    (rust-test-matching-parens
     "fn foo<a>() {}"
     '((10 11))
     '(7 9))))


(ert-deftest font-lock-extend-region-in-string ()
  
  (with-temp-buffer
    (rust-mode)
    (insert "
fn foo() {
    let x = r\"
Fontification needs to include this whole string or none of it.
             \"
}")
    (font-lock-fontify-buffer)
    (let ((font-lock-beg 13)
          (font-lock-end 42))
      (rust-font-lock-extend-region)
      (should (<= font-lock-beg 13))
      (should (>= font-lock-end 106))
      )
    (let ((font-lock-beg 42)
          (font-lock-end 108))
      (rust-font-lock-extend-region)
      (should (<= font-lock-beg 25))
      (should (>= font-lock-end 108)))
    (let ((font-lock-beg 1)
          (font-lock-end 12))
      (rust-font-lock-extend-region)
      (should (<= font-lock-beg 1))
      (should (>= font-lock-end 12)))))

(ert-deftest redo-syntax-after-change-far-from-point ()  
  (let*
      ((tmp-file-name (make-temp-file "rust-mdoe-test-issue104"))
       (base-contents (apply 'concat (append '("fn foo() {\n\n}\n") (make-list 500 "// More stuff...\n") '("fn bar() {\n\n}\n")))))
    ;; Create the temp file...
    (with-temp-file tmp-file-name
      (insert base-contents))
    (with-temp-buffer
      (insert-file-contents tmp-file-name 'VISIT nil nil 'REPLACE)
      (rust-mode)
      (goto-char (point-max))
      (should (= 0 (rust-paren-level)))
      (with-temp-file tmp-file-name
        (insert base-contents)
        (goto-char 12) ;; On the blank line in the middle of fn foo
        (insert "    let z = 1 < 3;")
        )
      (revert-buffer 'IGNORE-AUTO 'NOCONFIRM 'PRESERVE-MODES)
      (should (= 0 (rust-paren-level)))
      )
    )
  )

;; If electric-pair-mode is available, load it and run the tests that use it.  If not,
;; no error--the tests will be skipped.
(require 'elec-pair nil t)

;; The emacs 23 version of ERT does not have test skipping functionality.  So
;; don't even define these tests if elec-pair is not available.
(when (featurep 'elec-pair)
  (defun test-electric-pair-insert (original point-pos char closer)
    (let ((old-electric-pair-mode electric-pair-mode))
      (electric-pair-mode 1)
      (unwind-protect
          (with-temp-buffer
            (rust-mode)
            (insert original)
            (font-lock-fontify-buffer)

            (goto-char point-pos)
            (deactivate-mark)
            (let ((last-command-event char)) (self-insert-command 1))
            (should (equal (char-after)
                           (or closer (aref original point-pos)))))
        (electric-pair-mode (or old-electric-pair-mode 1)))))

  (ert-deftest rust-test-electric-pair-generic-fn ()
    (test-electric-pair-insert "fn foo() { }" 7 ?< ?>))

  (ert-deftest rust-test-electric-pair-impl-param ()
    (test-electric-pair-insert "impl Foo<T> for Bar<T>" 5 ?< ?>))

  (ert-deftest rust-test-electric-pair-impl-for-type-param ()
    (test-electric-pair-insert "impl<T> Foo<T> for Bar" 22 ?< ?>))

  (ert-deftest rust-test-electric-pair-lt-expression ()
    (test-electric-pair-insert "fn foo(bar:i32) -> bool { bar  }" 30 ?< nil))

  (ert-deftest rust-test-electric-pair-lt-expression-in-struct-literal ()
    (test-electric-pair-insert "fn foo(x:i32) -> Bar { Bar { a:(bleh() + whatever::<X>()), b:x   }  }" 63 ?< nil))

  (ert-deftest rust-test-electric-pair-lt-expression-capitalized-keyword ()
    (test-electric-pair-insert "fn foo() -> Box" 16 ?< ?>))
  )
