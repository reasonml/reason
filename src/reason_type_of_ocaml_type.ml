(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

let () = Reason_pprint_ast.configure
  (* This will likely be pluggable in the future. In the meantime, it's more
  convenient to have the output on a single line *)
  ~width:80
  ~assumeExplicitArity:false
  ~constructorLists:[]

let reasonFormatter = Reason_pprint_ast.createFormatter ()

(* "Why would you ever pass in some of these to print into Reason?"
"Because Merlin returns these as type signature sometimes" *)

(* int list *)
let parseAsCoreType str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.canonical_core_type
  |> reasonFormatter#core_type formatter

(* type a = int list *)
let parseAsImplementation str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.canonical_implementation
  |> reasonFormatter#structure [] formatter

(* val a: int list *)
let parseAsInterface str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.canonical_interface
  |> reasonFormatter#signature [] formatter

(* sig val a: int list end *)
(* This one is a hack; we should have our own parser entry point to module_type.
But that'd require modifying compiler-libs, which we'll refrain from doing. *)
let parseAsCoreModuleType str formatter =
  Lexing.from_string ("module X: " ^ str)
  |> Reason_toolchain.ML.canonical_interface
  |> reasonFormatter#signature [] formatter

let convert str =
  let formatter = Format.str_formatter in
  try (parseAsCoreType str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error _ ->
  try (parseAsImplementation str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error _ ->
  try (parseAsInterface str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error _ ->
  (parseAsCoreModuleType str formatter; Format.flush_str_formatter ())
