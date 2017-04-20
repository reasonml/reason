;;; reason-mode-tests.el --- ERT tests for reason-mode.el
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

(message "Running tests on Emacs %s" emacs-version)

(require 'ert-x)
(require 'reason-mode)
(require 'cl)

(setq reason-test-fill-column 32)

(defun reason-compare-code-after-manip (original point-pos manip-func expected got)
  (equal expected got))

(defun reason-test-explain-bad-manip (original point-pos manip-func expected got)
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
(put 'reason-compare-code-after-manip 'ert-explainer
     'reason-test-explain-bad-manip)

(defun reason-test-manip-code (original point-pos manip-func expected)
  (with-temp-buffer
    (reason-mode)
    (insert original)
    (goto-char point-pos)
    (funcall manip-func)
    (should (reason-compare-code-after-manip
             original point-pos manip-func expected (buffer-string)))))

(defun test-indent (indented &optional deindented)
  (let ((deindented (or deindented (replace-regexp-in-string "^[[:blank:]]*" "      " indented))))
    (reason-test-manip-code
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
type Foo { bar: int,
           baz: int};

type Blah {x:int,
           y:int,
           z:String};"))

;; Reason will also eventually support line comments, which are not supported in OCaml.
;; (ert-deftest indent-inside-braces ()
;;   (test-indent
;;    "
;; // struct fields out one level:
;; struct foo {
;;     a:int,
;;     // comments too
;;     b:char
;; }

;; fun bar(x:Box<int>) {   // comment here should not affect the next indent
;;     bla();
;;     bla();
;; }"))

(ert-deftest indent-top-level ()
  (test-indent
   "
/* Everything here is at the top level and should not be indented*/
let greeting = \"hello!\";
let score = 10;
let newScore = 10 + score;
"))

(ert-deftest font-lock-multi-raw-strings-in-a-row ()
  (reason-test-font-lock
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
  (reason-test-font-lock
   "\"bar\" r\"foo\""
   '("\"bar\"" font-lock-string-face "r\"foo\"" font-lock-string-face)))

;; TODO how to align these
(ert-deftest indent-params-no-align ()
  (test-indent
   "
/* Indent out one level because no params appear on the first line */
fun xyzzy(
  a:int,
  b:char) { };

fun abcdef(
  a:int,
  b:char)
  -> char
  { };
"))

(ert-deftest indent-params-align ()
  (test-indent
   "
/* Align the second line of params to the first */
fun foo(a:int,
        b:char) { };

fun bar(   a:int,
           b:char)
           -> int
  { };

fun baz(   a:int,  /* should work with a comment here */
           b:char)
           -> int
  { };
"))

(ert-deftest indent-open-after-arrow ()
  (test-indent
   "
/* Indent function body only one level after `=> {` */
fun foo1(a:int, b:char) : int => {
  let body;
};

fun foo2(a:int,
         b:char) => int {
  let body;
};

fun foo3(a:int,
         b:char)
  => int {
    let body;
  };
"))

(ert-deftest indent-square-bracket-alignment ()
  (test-indent
   "
fun args_on_the_next_line( /* with a comment */
                              a:int,
                              b:String) => {
  let aaaaaa = [
    1,
    2,
    3];
  let bbbbbbb = [1, 2, 3,
                 4, 5, 6];
  let ccc = [   10, 9, 8,
                7, 6, 5];
};
"))

;; TODO fix alignment of curly braces when down a line
(ert-deftest indent-nested-funs ()
  (test-indent
   "
fun nexted_funs(a: fun(b:int,
                       c:char)
                => int,
                d: int)
  => uint
  {
    0
  };
"
   ))

;; TODO fix alignment of curly braces when down a line
(ert-deftest indent-multi-line-expr ()
  (test-indent
   "
fun foo() =>
  {
    x();
    let a =
      b()
  };
"))

(ert-deftest indent-switch ()
  (test-indent
   "
fun foo() => {
  switch blah {
    | Pattern => stuff(),
    | _ => whatever
  }
};
"
   ))

(ert-deftest indent-switch-multiline-pattern ()
  (test-indent
   "
fun foo() => {
  switch blah {
    | Pattern => \"dada\"
    | Pattern2 => {
        hello()
      }
    | _ => \"whatever\"
  }
};
"))

;; TODO maybe fix the indentation of y(); below
(ert-deftest indent-indented-switch ()
  (test-indent
   "
fun foo() => {
  let x = {
    switch blah {
      | Pattern => \"dada\"
      | Pattern2 => {
          hello()
        }
      | _ => \"whatever\"
    }
  }
    y();
};
"))

;; Make sure that in effort to cover switch patterns we don't mistreat || or expressions
(ert-deftest indent-nonswitch-or-expression ()
  (test-indent
   "
fun foo() => {
  let x = foo() ||
    bar();
};
"))

;; Closing braces in single char literals and strings should not confuse the indentation
;; TODO In Reason it does confuse indentation
(ert-deftest indent-closing-braces-in-char-literals ()
  (test-indent
   "
fun foo() => {
  { bar('}'); }
    { bar(']'); }
    { bar(')'); }
};
"))

(defun reason-get-buffer-pos (pos-symbol)
  "Get buffer position from POS-SYMBOL.

POS-SYMBOL is a symbol found in `reason-test-positions-alist'.
Convert the line-column information from that list into a buffer position value."
  (interactive "P")
  (let* (
         (line-and-column (cadr (assoc pos-symbol reason-test-positions-alist)))
         (line (nth 0 line-and-column))
         (column (nth 1 line-and-column)))
    (save-excursion
      (goto-line line)
      (move-to-column column)
      (point))))

(defun reason-test-fontify-string (str)
  (with-temp-buffer
    (reason-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun reason-test-group-str-by-face (str)
  "Fontify `STR' in reason-mode and group it by face, returning a
list of substrings of `STR' each followed by its face."
  (loop with fontified = (reason-test-fontify-string str)
        for start = 0 then end
        while start
        for end   = (next-single-property-change start 'face fontified)
        for prop  = (get-text-property start 'face fontified)
        for text  = (substring-no-properties fontified start end)
        if prop
        append (list text prop)))

(defun reason-test-font-lock (source face-groups)
  "Test that `SOURCE' fontifies to the expected `FACE-GROUPS'"
  (should (equal (reason-test-group-str-by-face source)
                 face-groups)))

(ert-deftest font-lock-attribute-inside-string ()
  (reason-test-font-lock
   "\"#[foo]\""
   '("\"#[foo]\"" font-lock-string-face)))

(ert-deftest font-lock-attribute-inside-comment ()
  (reason-test-font-lock
   "/* #[foo] */"
   '("/* " font-lock-comment-delimiter-face
     "#[foo] " font-lock-comment-face
     "*/" font-lock-comment-delimiter-face)))

(ert-deftest font-lock-double-quote-character-literal ()
  (reason-test-font-lock
   "'\"'; let"
   '("'\"'" font-lock-string-face
     "let" font-lock-keyword-face)))

(ert-deftest font-lock-fun-contains-capital ()
  (reason-test-font-lock
   "fun foo_Bar() => {}"
   '("fun" font-lock-keyword-face)))

(ert-deftest font-lock-single-quote-character-literal ()
  (reason-test-font-lock
   "fun main() => { let ch = '\\''; }"
   '("fun" font-lock-keyword-face
     "let" font-lock-keyword-face
     "'\\''" font-lock-string-face)))

(ert-deftest font-lock-escaped-double-quote-character-literal ()
  (reason-test-font-lock
   "fun main() => { let ch = '\\\"'; }"
   '("fun" font-lock-keyword-face
     "let" font-lock-keyword-face
     "'\\\"'" font-lock-string-face)))

(ert-deftest font-lock-escaped-backslash-character-literal ()
  (reason-test-font-lock
   "fun main() => { let ch = '\\\\'; }"
   '("fun" font-lock-keyword-face
     "let" font-lock-keyword-face
     "'\\\\'" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-no-hashes ()
  (reason-test-font-lock
   "r\"No hashes\";"
   '("r\"No hashes\"" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-double-quote ()
  (reason-test-font-lock
   "fun main() => {
    r#\"With a double quote (\")\"#;
}
"
   '("fun" font-lock-keyword-face
     "r#\"With a double quote (\")\"#" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-two-hashes ()
  (reason-test-font-lock
   "r##\"With two hashes\"##;"
   '("r##\"With two hashes\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-backslash-at-end ()
  (reason-test-font-lock
   "r\"With a backslash at the end\\\";"
   '("r\"With a backslash at the end\\\"" font-lock-string-face)))

(ert-deftest font-lock-two-raw-strings ()
  (reason-test-font-lock
   "fun main() => {
    r\"With a backslash at the end\\\";
    r##\"With two hashes\"##;
}"
   '("fun" font-lock-keyword-face
     "r\"With a backslash at the end\\\"" font-lock-string-face
     "r##\"With two hashes\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-with-inner-hash ()
  (reason-test-font-lock
   "r##\"I've got an octothorpe (#)\"##; foo()"
   '("r##\"I've got an octothorpe (#)\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-with-inner-quote-and-hash ()
  (reason-test-font-lock
   "not_the_string(); r##\"string \"# still same string\"##; not_the_string()"
   '("r##\"string \"# still same string\"##" font-lock-string-face)))

(ert-deftest font-lock-string-ending-with-r-not-raw-string ()
  (reason-test-font-lock
   "fun f() => {
    \"Er\";
};

fun g() {
    \"xs\";
};"
   '("fun" font-lock-keyword-face
     "\"Er\"" font-lock-string-face
     "fun" font-lock-keyword-face
     "\"xs\"" font-lock-string-face)))

(ert-deftest font-lock-raw-string-trick-ending-followed-by-string-with-quote ()
  (reason-test-font-lock
   "r\"With what looks like the start of a raw string at the end r#\";
not_a_string();
r##\"With \"embedded\" quote \"##;"
   '("r\"With what looks like the start of a raw string at the end r#\"" font-lock-string-face
     "r##\"With \"embedded\" quote \"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-starter-inside-raw-string ()
  ;; Check that it won't look for a raw string beginning inside another raw string.
  (reason-test-font-lock
   "r#\"In the first string r\" in the first string \"#;
not_in_a_string();
r##\"In the second string\"##;"
   '("r#\"In the first string r\" in the first string \"#" font-lock-string-face
     "r##\"In the second string\"##" font-lock-string-face)))

(ert-deftest font-lock-runaway-raw-string ()
  (reason-test-font-lock
   "let Z = r#\"my raw string\";\n// oops this is still in the string"
   '("let" font-lock-keyword-face
     "Z" font-lock-type-face
     "r#\"my raw string\";\n// oops this is still in the string" font-lock-string-face))
  )

(ert-deftest font-lock-recognize-closing-raw-string ()
  (with-temp-buffer
    (reason-mode)
    (insert "let foo = r##\"
1...............................................50
1...............................................50
1...............................................50
1...............195-=>\"; let ...................50
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

(ert-deftest reason-test-two-character-quotes-in-a-row ()
  (with-temp-buffer
    (reason-mode)
    (font-lock-fontify-buffer)
    (insert "'\\n','a', fun")
    (font-lock-after-change-function 1 12 0)

    (should (equal 'font-lock-string-face (get-text-property 3 'face)))
    (should (equal nil (get-text-property 5 'face)))
    (should (equal 'font-lock-string-face (get-text-property 7 'face)))
    (should (equal nil (get-text-property 9 'face)))
    (should (equal 'font-lock-keyword-face (get-text-property 12 'face)))
    )
  )

(ert-deftest single-quote-null-char ()
  (reason-test-font-lock
   "'\\0' 'a' fun"
   '("'\\0'" font-lock-string-face
     "'a'" font-lock-string-face
     "fun" font-lock-keyword-face)))

(ert-deftest r-in-string-after-single-quoted-double-quote ()
  (reason-test-font-lock
   "'\"';\n\"r\";\n\"oops\";"
   '("'\"'" font-lock-string-face
     "\"r\"" font-lock-string-face
     "\"oops\"" font-lock-string-face
     )))

(ert-deftest char-literal-after-quote-in-raw-string ()
  (reason-test-font-lock
   "r#\"\"\"#;\n'q'"
   '("r#\"\"\"#" font-lock-string-face
     "'q'" font-lock-string-face)))
