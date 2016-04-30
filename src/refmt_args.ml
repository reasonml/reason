open Cmdliner

let is_interface_pp =
  let doc = "parse AST as interface" in
  Arg.(value & flag & info ["i"; "interface"] ~doc)

let use_stdin =
  let doc = "parse AST from stdin" in
  Arg.(value & flag & info ["s"; "stdin"] ~doc)

let recoverable =
  let doc = "enable recoverable parser" in
  Arg.(value & flag & info ["r"; "recoverable"] ~doc)

let explicit_arity =
  let doc =
    "If a constructor's argument is a tuple, always interpret \
     it as multiple arguments"
  in
  Arg.(value & flag & info ["e"; "explicit_arity"] ~doc)

let parse_ast =
  let doc =
    "parse AST as 'ml', 're', 'binary_reason(for \
     interchange between Reason versions')"
  in
  Arg.(value  & opt (some string) None & info ["parse"] ~doc)

let print =
  let doc =
    "print AST in <print> (either 'ml', 're', 'binary(default - \
     for compiler input)', \
     'binary_reason(for interchange between Reason versions)', \
     'ast (print human readable directly)', 'none')"
  in
  Arg.(value & opt (some string) None & info ["p"; "print"] ~doc)

let print_width =
  let doc = "wrapping width for printing the AST" in
  Arg.(value & opt (some int) (Some 90) & info ["w"; "print_width"] ~doc)

let heuristics_file =
  let doc =
    "load path as a heuristics file to specify which constructors are defined with \
     multi-arguments. Mostly used in removing [@implicit_arity] introduced from \
     OCaml conversion.\n\t\texample.txt:\n\t\tConstructor1\n\t\tConstructor2"
  in
  Arg.(value & opt (some file) None & info ["h"; "heuristic"] ~doc)

let input =
  let doc = "input files" in
  Arg.(required & pos ~rev:true 0 (some file) None & info [] ~doc)
