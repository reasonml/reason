(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Lexing
open Cmdliner

exception Invalid_config = Printer_maker.Invalid_config

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

let version = "Reason " ^ Package.version ^ " @ " ^ Package.git_short_version


let refmt
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
    is_interface_pp
    use_stdin
  =
  let () =
    if is_interface_pp then
      raise (Invalid_config "--is-interface-pp is deprecated.")
    else if use_stdin then
      raise (Invalid_config "--use-stdin is deprecated.")
  in
  let (use_stdin, input_file) = match input_file with
    | Some name -> (false, name)
    | None -> (true, "")
  in
  let () =
    let has_print = match print with
      | Some _ -> true
      | None -> false
    in
    let has_parse = match parse_ast with
      | Some _ -> true
      | None -> false
    in
    if input_file = "" && not (has_parse && has_print) then
      raise (Invalid_config "Need an input file, parse mode, and print \
                             mode. Modes can be auto-detected based on filename.")
  in
  Reason_config.configure ~r:is_recoverable;
  Location.input_name := input_file;
  let constructorLists = match h_file with
    | Some f_name -> read_lines f_name
    | None -> []
  in
  let interface = match interface with
    | true -> true
    | false -> (Filename.check_suffix input_file ".rei" || Filename.check_suffix input_file ".mli")
  in
  let writing_to_file = match output_file with
    | Some _ -> true
    | None -> false
  in
  let output_file =
    match in_place, use_stdin, writing_to_file with
    | (true, true, _) -> raise (Invalid_config "Cannot write in place to stdin.")
    | (true, _, true) -> raise (Invalid_config "Cannot specify --output and --in-place.")
    | (true, _, _) -> Some input_file
    (* Writing to a file vs stdout handled by Printer_maker below. *)
    | (false, _, _) -> output_file
  in
  let (module Printer : Printer_maker.PRINTER) =
    if interface then (module Reason_interface_printer.Reason_interface_printer)
    else (module Reason_implementation_printer.Reason_implementation_printer)
  in
  let _ = Reason_pprint_ast.configure
      ~width: print_width
      ~assumeExplicitArity: explicit_arity
      ~constructorLists
  in
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
    Printer_maker.close_output_file output_file output_chan;
    `Ok ()
  )

let top_level_info =
  let doc = "Meta language utility" in
  let man = [`S "DESCRIPTION"; `P "refmt is a parser and pretty-printer"] in
  Term.info "refmt" ~version ~doc ~man

let refmt_t =
  let open Term in
  let open Refmt_args in
  let open Cmdliner_shim in
  let handler = function
    | Invalid_config msg -> msg
    | exn ->
            Location.report_exception Format.err_formatter exn;
            exit 1
  in
  let refmt = wrap refmt handler in
  load (const refmt $ interface
                    $ recoverable
                    $ explicit_arity
                    $ parse_ast
                    $ print
                    $ print_width
                    $ heuristics_file
                    $ output
                    $ in_place
                    $ input
                    $ is_interface_pp
                    $ use_stdin)

let () =
  match Term.eval ((Term.ret refmt_t), top_level_info) with
  | `Error s -> exit 1
  | _ -> exit 0
