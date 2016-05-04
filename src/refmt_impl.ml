(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Lexing

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

(* Note: filename should only be used with .ml files. See reason_toolchain. *)
let defaultImplementationParserFor use_stdin filename = Reason_toolchain.(
    if Filename.check_suffix filename ".re"
    then (JS.canonical_implementation_with_comments (setup_lexbuf use_stdin filename), false, false)
    else if Filename.check_suffix filename ".ml"
    then (ML.canonical_implementation_with_comments (setup_lexbuf use_stdin filename), true, false)
    else
      (Invalid_config ("Cannot determine default implementation parser for filename '" ^ filename ^ "'."))
      |> raise
  )

(* Note: filename should only be used with .mli files. See reason_toolchain. *)
let defaultInterfaceParserFor use_stdin filename = Reason_toolchain.(
    if Filename.check_suffix filename ".rei"
    then (JS.canonical_interface_with_comments (setup_lexbuf use_stdin filename) , false, true)
    else if Filename.check_suffix filename ".mli"
    then (ML.canonical_interface_with_comments (setup_lexbuf use_stdin filename), true, true)
    else
      (Invalid_config ("Cannot determine default interface parser for filename '" ^ filename ^ "'."))
      |> raise
  )

let reasonBinaryParser use_stdin filename =
  let chan =
    if use_stdin then stdin
    else
      let file_chan = open_in filename in
      seek_in file_chan 0;
      file_chan
  in
  let (magic_number, filename, ast, comments, parsedAsML, parsedAsInterface) = input_value chan in
  ((ast, comments), parsedAsML, parsedAsInterface)

(*
 * As soon as m17n vends comments, this should be replaced with what is
 * effectively m17n's parser.
 *)

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
  Reason_config.configure is_recoverable;
  Location.input_name := input_file;
  let constructorLists = match h_file with
      None -> [] | Some f_name -> read_lines f_name
  in
  let intf =
    let is_suffix = String.lowercase input_file |> Filename.check_suffix in
    match is_interface_pp with
    | None when (is_suffix ".rei" || is_suffix ".mli") -> true
    | None -> if use_stdin then
        Invalid_config "Unable to determine if stdin input is an \
                        interface file. Invalid --is-interface-pp setting."
        |> raise
      else false
    | Some b -> b
  in
  try
    if intf then (
      let ((ast, comments), parsedAsML, parsedAsInterface) = Reason_toolchain.(match parse_ast with
          | None -> (defaultInterfaceParserFor use_stdin input_file)
          | Some "binary_reason" -> reasonBinaryParser use_stdin input_file
          | Some "ml" -> ((ML.canonical_interface_with_comments (setup_lexbuf use_stdin input_file)), true, true)
          | Some "re" -> ((JS.canonical_interface_with_comments (setup_lexbuf use_stdin input_file)), false, true)
          | Some s -> (
              Invalid_config (P.sprintf "Invalid --parse setting for interface ' %s'." s)
              |> raise
            ))
      in
      let _ =
        if not parsedAsInterface then
          raise (Invalid_config ("The file parsed does not appear to be an interface file.")) in
      Reason_pprint_ast.configure
        ~width: (match print_width with None -> 90 | Some i -> i)
        ~assumeExplicitArity:explicit_arity
        ~constructorLists;
      let thePrinter = match print with
        | Some "binary_reason" -> fun (ast, comments) ->
          (* Our special format for interchange between reason should keep the
           * comments separate.  This is not compatible for input into the
           * ocaml compiler - only for input into another version of Reason. We
           * also store whether or not the binary was originally *parsed* as an
           * interface file.
          *)
          (Config.ast_intf_magic_number, input_file, ast, comments, parsedAsML, true)
          |> output_value stdout
        | Some "binary"
        | None -> fun (ast, comments) -> (
            output_string stdout Config.ast_intf_magic_number;
            output_value  stdout input_file;
            output_value  stdout ast
          )
        | Some "ast" -> fun (ast, comments) -> Printast.interface Format.std_formatter ast
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun (ast, comments) -> ())
        | Some "ml" -> Reason_toolchain.ML.print_canonical_interface_with_comments
        | Some "re" -> Reason_toolchain.JS.print_canonical_interface_with_comments
        | Some s ->
          Invalid_config (P.sprintf "Invalid --print setting for interface '%s'." s)
          |> raise
      in
      thePrinter (ast, comments)
    ) else (
      (*    Printf.fprintf stderr "syntax error: %s\n" (Reason_error_report.report checkpoint); [] in *)
      let ((ast, comments), parsedAsML, parsedAsInterface) = Reason_toolchain.(match parse_ast with
          | None -> (defaultImplementationParserFor use_stdin input_file)
          | Some "binary_reason" -> reasonBinaryParser use_stdin input_file
          | Some "ml" -> (ML.canonical_implementation_with_comments (setup_lexbuf use_stdin input_file), true, false)
          | Some "re" -> (JS.canonical_implementation_with_comments (setup_lexbuf use_stdin input_file), false, false)
          | Some s ->
            Invalid_config (P.sprintf "Invalid --parse setting for interface '%s'." s)
            |> raise
        )
      in

      let _ = if parsedAsInterface then
          raise (Invalid_config ("The file parsed does not appear to be an implementation file.")) in
      Reason_pprint_ast.configure
        ~width: (match print_width with None -> 90 | Some i -> i)
        ~assumeExplicitArity: explicit_arity
        ~constructorLists;
      let thePrinter = match print with
        | Some "binary_reason" -> fun (ast, comments) ->
          (* Our special format for interchange between reason should keep the
           * comments separate.  This is not compatible for input into the
           * ocaml compiler - only for input into another version of Reason. We
           * also store whether or not the binary was originally *parsed* as an
           * interface file.
          *)
          (Config.ast_impl_magic_number, input_file, ast, comments, parsedAsML, false)
          |> output_value stdout
        | Some "binary"
        | None -> fun (ast, comments) -> (
            output_string stdout Config.ast_impl_magic_number;
            output_value stdout input_file;
            output_value stdout ast
          )
        | Some "ast" -> fun (ast, comments) ->
          Printast.implementation Format.std_formatter ast
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun (ast, comments) -> ())
        | Some "ml" -> Reason_toolchain.ML.print_canonical_implementation_with_comments
        | Some "re" -> Reason_toolchain.JS.print_canonical_implementation_with_comments
        | Some s ->
          Invalid_config (P.sprintf "Invalid --print setting for implementation '%s'." s)
          |> raise
      in
      thePrinter (ast, comments)
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
  | `Error _ ->
    "This is unexpected, please report to github.com/facebook/Reason"
    |> prerr_endline
  | _ -> ()
