(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

let () = Reason_pprint_ast.configure
  (* This will likely be pluggable in the future. In the meantime, it's more
  convenient to have the output on a single line *)
  ~width:80
  ~assumeExplicitArity:false
  ~constructorLists:[]

let reasonFormatter = Reason_pprint_ast.createFormatter ()

let convert str =
  try
    (* we'll first assume the input is a type, e.g. "int list" *)
    Lexing.from_string str
    |> Reason_toolchain.ML.canonical_core_type
    |> reasonFormatter#core_type Format.str_formatter;
    Format.flush_str_formatter ()
  with Syntaxerr.Error a ->
    (* if the above parsing failed, we re-try, assuming that we're being
    passed a syntaxically valid input, e.g. type a = A | B *)
    Lexing.from_string str
    |> Reason_toolchain.ML.canonical_implementation
    |> reasonFormatter#structure [] Format.str_formatter;
    Format.flush_str_formatter ()
