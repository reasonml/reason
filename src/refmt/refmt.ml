(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Cmdliner = Vendored_cmdliner
open Cmdliner

let read_text_lines file =
  let list = ref [] in
  let chan = open_in file in
  try
    while true do
      list := input_line chan :: !list
    done;
    []
  with
  | End_of_file ->
    close_in chan;
    List.rev !list

let refmt
    interface
    is_recoverable
    explicit_arity
    parse_ast
    print
    print_width
    heuristics_file
    in_place
    input_files
  =
  let refmt_single input_file =
    let use_stdin, input_file =
      match input_file with Some name -> false, name | None -> true, ""
    in
    let eol =
      match use_stdin, input_file with
      | true, _ -> Eol_detect.default_eol
      | false, name -> Eol_detect.get_eol_for_file name
    in
    let parse_ast =
      match parse_ast, use_stdin with
      | Some x, _ -> x
      | None, false -> `Auto
      | None, true -> `Reason (* default *)
    in
    let constructorLists =
      match heuristics_file with
      | Some f_name -> read_text_lines f_name
      | None -> []
    in
    let interface =
      match interface with
      | true -> true
      | false ->
        Filename.check_suffix input_file ".rei"
        || Filename.check_suffix input_file ".mli"
    in
    let output_file =
      match in_place, use_stdin with
      | true, true -> Printer_maker.err "Cannot write in place to stdin."
      | true, _ -> Some input_file
      | false, _ -> None
    in
    let (module Printer : Printer_maker.PRINTER) =
      if interface
      then (module Reason_interface_printer)
      else (module Reason_implementation_printer)
    in
    Reason_config.configure ~r:is_recoverable;
    Location.input_name := input_file;
    let _ =
      Reason_pprint_ast.configure
        ~width:print_width
        ~assumeExplicitArity:explicit_arity
        ~constructorLists
    in
    let ast, parsedAsML = Printer.parse ~use_stdin parse_ast input_file in
    let output_chan = Printer_maker.prepare_output_file output_file in
    (* If you run into trouble with this (or need to use std_formatter by itself
       at the same time for some reason), try breaking this out so that it's not
       possible to call Format.formatter_of_out_channel on stdout. *)
    let output_formatter = Eol_convert.get_formatter output_chan eol in
    Printer.print print input_file parsedAsML output_chan output_formatter ast;
    (* Also closes all open boxes. *)
    Format.pp_print_flush output_formatter ();
    flush output_chan;
    Printer_maker.close_output_file output_file output_chan
  in
  try
    match input_files with
    | [] -> `Ok (refmt_single None)
    | _ -> `Ok (List.iter (fun file -> refmt_single (Some file)) input_files)
  with
  | Printer_maker.Invalid_config msg -> `Error (true, msg)
  | Reason_errors.Reason_error (error, loc) ->
    Reason_errors.report_error Format.err_formatter ~loc error;
    exit 1
  | exn ->
    prerr_endline (Printexc.to_string exn);
    (* FIXME: Reason_syntax_util.report_error Format.err_formatter exn; *)
    exit 1

let split_lines s =
  let rec loop ~last_is_cr ~acc i j =
    if j = String.length s
    then
      let acc =
        if j = i || (j = i + 1 && last_is_cr)
        then acc
        else String.sub s i (j - i) :: acc
      in
      List.rev acc
    else
      match s.[j] with
      | '\r' -> loop ~last_is_cr:true ~acc i (j + 1)
      | '\n' ->
        let line =
          let len = if last_is_cr then j - i - 1 else j - i in
          String.sub s i len
        in
        loop ~acc:(line :: acc) (j + 1) (j + 1) ~last_is_cr:false
      | _ -> loop ~acc i (j + 1) ~last_is_cr:false
  in
  loop ~acc:[] 0 0 ~last_is_cr:false

let[@tail_mod_cons] rec concat_map f = function
  | [] -> []
  | x :: xs -> prepend_concat_map (f x) f xs

and[@tail_mod_cons] prepend_concat_map ys f xs =
  match ys with
  | [] -> concat_map f xs
  | y :: ys -> y :: prepend_concat_map ys f xs

let examples = function
  | [] -> `Blocks []
  | _ :: _ as examples ->
    let block_of_example index (intro, ex) =
      let prose = `I (string_of_int (index + 1) ^ ".", String.trim intro ^ ":")
      and code_lines =
        ex
        |> String.trim
        |> split_lines
        |> concat_map (fun codeline -> [ `Noblank; `Pre ("      " ^ codeline) ])
        (* suppress initial blank *)
        |> List.tl
      in
      `Blocks (prose :: code_lines)
    in
    let example_blocks = examples |> List.mapi block_of_example in
    `Blocks (`S "EXAMPLES" :: example_blocks)

let top_level_info =
  let doc = "Reason's Parser & Pretty-printer" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "refmt lets you format Reason files, parse them, and convert them \
         between OCaml syntax and Reason syntax."
    ; examples
        [ "Initialise a new project named `foo'", "dune init project foo"
        ; "Format a Reason implementation file", "refmt file.re"
        ; "Format a Reason interface file", "refmt file.rei"
        ; ( "Format interface code from the command line"
          , "echo 'let x: int' | refmt --interface=true" )
        ; "Convert an OCaml file to Reason", "refmt file.ml"
        ; "Convert a Reason file to OCaml", "refmt file.re --print ml"
        ; ( "Convert OCaml from the command line to Reason"
          , "echo 'let x = 1' | refmt --parse ml" )
        ]
    ]
  in
  let version =
    "Reason " ^ Package.version ^ " @ " ^ Package.git_short_version
  in
  Cmd.info "refmt" ~version ~doc ~man

let refmt_t =
  let open Term in
  let open Refmt_args in
  let term =
    const refmt
    $ interface
    $ recoverable
    $ explicit_arity
    $ parse_ast
    $ print
    $ print_width
    $ heuristics_file
    $ in_place
    $ input
  in
  Cmd.v top_level_info (Term.ret term)

let () =
  match Cmd.eval_value' refmt_t with
  | `Exit 0 -> exit 0
  | `Exit _ -> exit 1
  | _ -> exit 0
