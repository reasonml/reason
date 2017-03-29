(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Lexing
open Cmdliner

exception Invalid_config = Printer_maker.Invalid_config
let err = Printer_maker.err

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

let warn s =
  let red s = "\027[31m" ^ s ^ "\x1b[m" in
  prerr_endline (red "WARNING:" ^ " " ^ s)


let refmt
    interface
    is_recoverable
    explicit_arity
    parse_ast
    print
    print_width
    h_file
    in_place
    input_files
    add_printers
    add_runtime
    is_interface_pp
    use_stdin
  =
  let refmt_single input_file =
    let () =
      if is_interface_pp then err "--is-interface-pp is deprecated."
      else if use_stdin then err "--use-stdin is deprecated."
    in
    let (use_stdin, input_file) = match input_file with
      | Some name -> (false, name)
      | None -> (true, "")
    in
    let parse_ast = match parse_ast, use_stdin with
      | (Some x, _) -> x
      | (None, false) -> `Auto
      | (None, true) -> `Reason (* default *)
    in
    let constructorLists = match h_file with
      | Some f_name -> read_lines f_name
      | None -> []
    in
    let interface = match interface with
      | true -> true
      | false -> (Filename.check_suffix input_file ".rei" || Filename.check_suffix input_file ".mli")
    in
    let output_file =
      match in_place, use_stdin with
      | (true, true) -> err "Cannot write in place to stdin."
      | (true,    _) -> Some input_file
      | (false,   _) -> None
    in
    let (module Printer : Printer_maker.PRINTER) =
      if interface then (module Reason_interface_printer.Reason_interface_printer)
      else (module Reason_implementation_printer.Reason_implementation_printer)
    in
    Reason_config.configure ~r:is_recoverable ~ap:add_printers;
    Location.input_name := input_file;
    let _ = Reason_pprint_ast.configure
        ~width: print_width
        ~assumeExplicitArity: explicit_arity
        ~constructorLists
    in
    let (ast, parsedAsML) =
      Printer.parse ~add_runtime ~use_stdin parse_ast input_file
    in
    let output_chan = Printer_maker.prepare_output_file output_file in
    (* If you run into trouble with this (or need to use std_formatter by
       itself at the same time for some reason), try breaking this out so that
       it's not possible to call Format.formatter_of_out_channel on stdout. *)
    let output_formatter = Format.formatter_of_out_channel output_chan in
    (
      Printer.print print input_file parsedAsML output_chan output_formatter ast;
      (* Also closes all open boxes. *)
      Format.pp_print_flush output_formatter ();
      flush output_chan;
      Printer_maker.close_output_file output_file output_chan;
    )
  in
  try
    match input_files with
    | [] -> `Ok (refmt_single None)
    | ls -> `Ok (List.iter (fun file -> refmt_single (Some file)) input_files)
  with
  | Invalid_config msg -> `Error (true, msg)
  | exn ->
          Location.report_exception Format.err_formatter exn;
          exit 1

let top_level_info =
  let doc = "Meta language utility" in
  let man = [`S "DESCRIPTION"; `P "refmt is a parser and pretty-printer"] in
  let version = "Reason " ^ Package.version ^ " @ " ^ Package.git_short_version
  in
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
                    $ in_place
                    $ input
                    $ add_printers
                    $ add_runtime
                    $ is_interface_pp
                    $ use_stdin)

let () =
  match Term.eval ((Term.ret refmt_t), top_level_info) with
  | `Error s -> exit 1
  | _ -> exit 0
