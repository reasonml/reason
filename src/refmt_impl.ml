(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Lexing
open Reason_interface_printer
open Reason_implementation_printer

module P = Printf

exception Invalid_config of string

let default_print_width = 100

let read_lines file =
  let list = ref [] in
  let chan = open_in file in
  try
    while true do
      list := input_line chan :: !list
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !list


let version_string = "Reason " ^ Package.version ^ " @ " ^ Package.git_short_version

let usage = {|Reason: Meta Language Utility

[Usage]: refmt [options] some-file.[re|ml|rei|mli]

   // translate ocaml to reason on stdin
   echo 'let _ = ()' | refmt --print re --parse ml

   // print the AST of a file
   refmt --parse re --print ast some-file.re

   // reformat a file
   refmt --parse re --print re some-file.re

[Options]:
|}


let begin_reason
    is_interface_pp
    use_stdin
    is_recoverable
    explicit_arity
    parse_ast
    print
    print_width
    h_file
    input_file
  =
  Reason_config.configure ~r:!recoverable;
  Location.input_name := filename;
  let constructorLists = match h_file with
    | None -> []
    | Some f_name -> read_lines f_name
  in
  let intf = match !intf with
    | None when (Filename.check_suffix filename ".rei" || Filename.check_suffix filename ".mli") -> true
    | None -> false
    | Some b -> b
  in
  let () =
      if !in_place then
          if use_stdin then
              raise (Invalid_config "Cannot write in place to stdin.")
          else if !writing_to_file then
              raise (Invalid_config "Cannot specify --output and --in-place.")
          else
              output_file := Some filename
      else ()
  in
  let (module Printer : Printer_maker.PRINTER) =
    if intf then (module Reason_interface_printer)
    else (module Reason_implementation_printer)
  in
  let _ = Reason_pprint_ast.configure
      ~width: print_width
      ~assumeExplicitArity: !assumeExplicitArity
      ~constructorLists
  in
  try
    let (ast, parsedAsML) = Printer.parse !prse use_stdin filename in
    let output_chan = Printer_maker.prepare_output_file !output_file in
    (* If you run into trouble with this (or need to use std_formatter by
       itself at the same time for some reason), try breaking this out so that
       it's not possible to call Format.formatter_of_out_channel on stdout. *)
    let output_formatter = Format.formatter_of_out_channel output_chan in
    let thePrinter = Printer.makePrinter !prnt filename parsedAsML output_chan output_formatter in
    (
        thePrinter ast;
        (* Also closes all open boxes. *)
        Format.pp_print_flush output_formatter ();
        flush output_chan;
        Printer_maker.close_output_file !output_file output_chan
    )
  with
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1


let entry_point =
  Refmt_args.(Cmdliner.Term.(pure
                               begin_reason
                             $ is_interface_pp
                             $ use_stdin
                             $ recoverable
                             $ explicit_arity
                             $ parse_ast
                             $ print
                             $ print_width
                             $ heuristics_file
                             $ input
                            ))

let top_level_info =
  let doc = "Meta language utility" in
  let man = [`S "DESCRIPTION";
             `P "Something something"]
  in
  Cmdliner.Term.info "refmt" ~version:"0.0.2" ~doc ~man

let () =
  match Cmdliner.Term.eval (entry_point, top_level_info) with
  | `Error `Exn ->
    "This is unexpected, please report to http://github.com/facebook/Reason"
    |> prerr_endline
  | _ -> ()
