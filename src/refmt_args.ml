open Cmdliner

let interface =
  let doc = "parse AST as an interface" in
  Arg.(value & opt (bool) false & info ["i"; "interface"] ~doc)

let recoverable =
  let doc = "enable recoverable parser" in
  Arg.(value & flag & info ["r"; "recoverable"] ~doc)

let explicit_arity =
  let doc =
    "if a constructor's argument is a tuple, always interpret it as \
     multiple arguments"
  in
  Arg.(value & flag & info ["e"; "assume-explicit-arity"] ~doc)

let parse_ast =
  let docv = "FORM" in
  let doc = "parse AST in FORM, which is one of: (ml | re | \
             binary (for compiler input) | \
             binary_reason (for interchange between Reason versions))"
  in
  let opts = Arg.enum ["ml", `ML; "re", `Reason;
                       "binary_reason", `BinaryReason; "auto", `Auto]
  in
  Arg.(value & opt (some opts) None & info ["parse"] ~docv ~doc)

let print =
  let docv = "FORM" in
  let doc = "print AST in FORM, which is one of: (ml | re (default) | \
             binary (for compiler input) | \
             binary_reason (for interchange between Reason versions) | \
             ast (print human readable AST directly) | none)"
  in
  let opts = Arg.enum ["ml", `ML; "re", `Reason; "binary", `Binary;
                       "binary_reason", `BinaryReason; "ast", `AST;
                       "none", `None]
  in
  Arg.(value & opt opts `Reason & info ["p"; "print"] ~docv ~doc)

let print_width =
  let docv = "COLS" in
  let doc = "wrapping width for printing the AST" in
  Arg.(value & opt (int) (90) & info ["w"; "print-width"] ~docv ~doc)

let heuristics_file =
  let doc =
    "load path as a heuristics file to specify which constructors carry a tuple \
     rather than multiple arguments. Mostly used in removing [@implicit_arity] introduced from \
     OCaml conversion.\n\t\texample.txt:\n\t\tConstructor1\n\t\tConstructor2"
  in
  Arg.(value & opt (some file) None & info ["h"; "heuristics-file"] ~doc)

let in_place =
  let doc = "reformat a file in-place" in
  Arg.(value & flag & info ["in-place"] ~doc)

let add_printers =
  let doc = "add auto-printers to user-defined types" in
  Arg.(value & flag & info ["add-printers"] ~doc)

let add_runtime =
  let doc = "add runtime for auto-printers" in
  Arg.(value & flag & info ["add-runtime"] ~doc)

let input =
  let docv = "FILENAMES" in
  let doc = "input files; if empty, assume stdin" in
  Arg.(value & (pos_all non_dir_file []) & info [] ~docv ~doc)

(* DEPRECATED *)

let is_interface_pp =
  let doc = "is-interface-pp is DEPRECATED; use -i or --interface instead" in
  Arg.(value & flag & info ["is-interface-pp"] ~doc)

let use_stdin =
  let doc = "use-stdin is DEPRECATED; usage is assumed if not specifying a \
             filename"
  in
  Arg.(value & flag & info ["use-stdin"] ~doc)
