(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let reasonFormatter = Reason_pprint_ast.createFormatter ()

(* "Why would you ever pass in some of these to print into Reason?"
"Because Merlin returns these as type signature sometimes" *)

(* int list *)
let parseAsCoreType str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.core_type
  |> reasonFormatter#core_type formatter

(* type a = int list *)
let parseAsImplementation str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.implementation
  |> reasonFormatter#structure [] formatter

(* val a: int list *)
let parseAsInterface str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.interface
  |> reasonFormatter#signature [] formatter

(* sig val a: int list end *)
(* This one is a hack; we should have our own parser entry point to module_type.
But that'd require modifying compiler-libs, which we'll refrain from doing. *)
let parseAsCoreModuleType str formatter =
  Lexing.from_string ("module X: " ^ str)
  |> Reason_toolchain.ML.interface
  |> reasonFormatter#signature [] formatter

(* Quirky merlin/ocaml output that doesn't really parse. *)
let parseAsWeirdListSyntax str a =
  if str = "type 'a list = [] | :: of 'a * 'a list" then "type list 'a = [] | :: of list 'a 'a"
  (* Manually creating an error is tedious, so we'll put a hack here to throw the previous error. *)
  else raise (Syntaxerr.Error a)

let convert str =
  let formatter = Format.str_formatter in
  try (parseAsCoreType str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error _ ->
  try (parseAsImplementation str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error _ ->
  try (parseAsInterface str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error _ ->
  try (parseAsCoreModuleType str formatter; Format.flush_str_formatter ())
  with Syntaxerr.Error a ->
  (parseAsWeirdListSyntax str a)
