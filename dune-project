(lang dune 3.8)

(name reason)

(using menhir 2.0)

(cram enable)

(version 3.16.0)

(generate_opam_files true)

(source
 (github reasonml/reason))

(authors "Jordan Walke <jordojw@gmail.com>")

(maintainers "Jordan Walke <jordojw@gmail.com>"
 "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(homepage "https://reasonml.github.io/")

(bug_reports "https://github.com/reasonml/reason/issues")

(license "MIT")

(package
 (name reason)
 (synopsis "Reason: Syntax & Toolchain for OCaml")
 (description
  "Reason gives OCaml a new syntax that is remniscient of languages like\nJavaScript. It's also the umbrella project for a set of tools for the OCaml &\nJavaScript ecosystem.")
 (depends
  (ocaml
   (and
    (>= "4.08")
    (< "5.5")))
  (ocamlfind :build)
  cmdliner
  (dune-build-info
   (>= 2.9.3))
  (menhir
   (>= "20180523"))
  (merlin-extend
   (>= "0.6.2"))
  fix
  cppo
  (ppxlib :dev)))

(package
 (name rtop)
 (synopsis "Reason toplevel")
 (description
  "rtop is the toplevel (or REPL) for Reason, based on utop (https://github.com/ocaml-community/utop).")
 (depends
  (ocaml
   (and
    (>= "4.08")
    (< "5.5")))
  (reason
   (= :version))
  (utop
   (>= "2.0"))
  cppo))
