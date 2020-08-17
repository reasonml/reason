module Cmdliner = Vendored_cmdliner
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
  let opts = Arg.enum ["ml", `ML; "re", `Reason; "binary", `Binary;
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
  Arg.(value & opt (int) (80) & info ["w"; "print-width"] ~docv ~doc)

let _version_options =
  List.map
  (fun fv ->
    let major, minor = string_of_int fv.Reason_version.major, string_of_int fv.minor in
    (major ^ "." ^ minor), fv)
  Reason_version.all_supported_file_versions

let unspecified_version = Reason_version.unspecified ()
let version_options = ("default", unspecified_version) :: _version_options

let parse_version =
  let docv = "INT.INT" in
  let doc =
    "Sets the default assumed print of Reason Syntax to parse. \
     Usually refmt will assume 3.7, until it sees otherwise such as [@reason.version 3.8]. \
     Passing x.y for this option causes refmt to assume x.y until it an attribute requesting \
     otherwise." in
  let opts = Arg.enum version_options in
  Arg.(value & opt opts unspecified_version & info ["parse-version"] ~docv ~doc)

let promote_version =
  let docv = "INT.INT" in
  let doc =
    "Forces the parser to rewrite the Reason Syntax version attribute at \
    parse time, causing the printer to print it in the promoted version. \
    If no existing attribute was present, one will be injected at parse time \
    as usual." in
  let opts = Arg.enum version_options in
  Arg.(value & opt opts unspecified_version & info ["promote-version"] ~docv ~doc)


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

let input =
  let docv = "FILENAMES" in
  let doc = "input files; if empty, assume stdin" in
  Arg.(value & (pos_all non_dir_file []) & info [] ~docv ~doc)
