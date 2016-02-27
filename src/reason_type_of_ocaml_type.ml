(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

let reasonFormatter = Reason_pprint_ast.createFormatter ()

let convert str =
  let lexbuf = Lexing.from_string str in
  let parsedTree = Reason_toolchain.ML.canonical_core_type lexbuf in
  (* will print the resulting string to the string formatter buffer *)
  reasonFormatter#core_type Format.str_formatter parsedTree;
  (* will return the formatted string *)
  Format.flush_str_formatter ()
