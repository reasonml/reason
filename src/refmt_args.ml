open Cmdliner

(* let version =
    let doc = "print version of Reason" in
    Arg.(value & flag & info ["v"; "version"] ~doc)
*)

let is_interface_pp =
    let doc = "parse AST as interface" in
    Arg.(value & opt (some bool) None & info ["i"; "is-interface-pp"] ~doc)

let interface =
    let doc =
        "parse AST as an interface (either true or faulse; default false)"
    in
    Arg.(value & opt (some bool) (Some false) & info ["-i"; "interface"] ~doc)

let recoverable =
    let doc = "enable recoverable parser" in
    Arg.(value & flag & info ["r"; "recoverable"] ~doc)

let explicit_arity =
    let doc =
        "If a constructor's argument is a tuple, always interpret \
     it as multiple arguments"
    in
    Arg.(value & flag & info ["e"; "assume-explicit-arity"] ~doc)

let parse_ast =
    let doc =
        "parse AST as 'ml', 're', 'binary_reason(for \
     interchange between Reason versions')"
    in
    Arg.(value & opt (some string) None & info ["parse"] ~doc)

let print =
    let docv =
        "(ml | re | binary (default - for compiler input) | binary_reason \
        (for interchange between Reason versions) | ast (print human readable \
        directly) | none)"
    in
    let doc = "print AST in given form" in
    Arg.(value & opt (some string) None & info ["p"; "print"] ~docv ~doc)

let print_width =
    let docv = "COLS" in
    let doc = "wrapping width for printing the AST" in
    Arg.(value & opt (int) (90) & info ["w"; "print-width"] ~docv ~doc)

let heuristics_file =
    let doc =
        "load path as a heuristics file to specify which constructors are defined with \
     multi-arguments. Mostly used in removing [@implicit_arity] introduced from \
     OCaml conversion.\n\t\texample.txt:\n\t\tConstructor1\n\t\tConstructor2"
    in
    Arg.(value & opt (some file) None & info ["h"; "heuristics-file"] ~doc)

let output =
    let docv = "FILENAME" in
    let doc = "target file for output; default [stdout]" in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv ~doc)

let in_place =
    let doc = "reformat a file in-place (defaults to not in place)" in
    Arg.(value & flag & info ["in-place"] ~doc)

let input =
    let docv = "FILENAME" in
    let doc = "input file" in
    Arg.(value & pos ~rev:true 0 (some file) None & info [] ~docv ~doc)
