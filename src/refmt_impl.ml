(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Lexing
open Reason_interface_printer
open Reason_implementation_printer

module P = Printf

exception Invalid_config of string

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


let refmt
      is_interface_pp
      interface
      is_recoverable
      explicit_arity
      parse_ast
      print
      print_width
      h_file
      output_file
      in_place
      input_file
  =
  let (use_stdin, input_file) = match input_file with
    | Some name -> (false, name)
    | None -> (true, "")
 in
 let () =
    let has_print = match print with
      | None -> false
      | Some x -> true
    in
    let has_parse = match parse_ast with
      | None -> false
      | Some x -> true
    in
    if input_file = "" && not (has_parse && has_print) then
        raise (Invalid_config "Need an input file, parse mode, and print mode.")
  in
  Reason_config.configure ~r:is_recoverable;
  Location.input_name := input_file;
  let constructorLists = match h_file with
    | None -> []
    | Some f_name -> read_lines f_name
  in
  let intf = match interface with
    | None when (Filename.check_suffix input_file ".rei" || Filename.check_suffix input_file ".mli") -> true
    | None -> false
    | Some b -> b
  in
  let writing_to_file = match output_file with
    | Some _ -> true
    | None -> false
  in
  let output_file =
      if in_place then
          if use_stdin then
              raise (Invalid_config "Cannot write in place to stdin.")
          else if writing_to_file then
              raise (Invalid_config "Cannot specify --output and --in-place.")
          else Some input_file
      else output_file
  in
  let (module Printer : Printer_maker.PRINTER) =
    if intf then (module Reason_interface_printer)
    else (module Reason_implementation_printer)
  in
  let _ = Reason_pprint_ast.configure
      ~width: print_width
      ~assumeExplicitArity: explicit_arity
      ~constructorLists
  in
  try
    let (ast, parsedAsML) = Printer.parse parse_ast use_stdin input_file in
    let output_chan = Printer_maker.prepare_output_file output_file in
    (* If you run into trouble with this (or need to use std_formatter by
       itself at the same time for some reason), try breaking this out so that
       it's not possible to call Format.formatter_of_out_channel on stdout. *)
    let output_formatter = Format.formatter_of_out_channel output_chan in
    let thePrinter = Printer.makePrinter print input_file parsedAsML output_chan output_formatter in
    (
        thePrinter ast;
        (* Also closes all open boxes. *)
        Format.pp_print_flush output_formatter ();
        flush output_chan;
        Printer_maker.close_output_file output_file output_chan
    )
  with
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1


let top_level_info =
  let doc = "Meta language utility" in
  let man = [`S "DESCRIPTION";
             `P "Something something"]
  in
  Cmdliner.Term.info "refmt" ~version:version_string ~doc ~man

let refmt_t =
  Refmt_args.(Cmdliner.Term.(const refmt
                             $ is_interface_pp
                             $ interface
                             $ recoverable
                             $ explicit_arity
                             $ parse_ast
                             $ print
                             $ print_width
                             $ heuristics_file
                             $ output
                             $ in_place
                             $ input
                            ))

let () =
  match Cmdliner.Term.eval (refmt_t, top_level_info) with
  | `Error `Exn -> exit 1
  | _ -> exit 0
