(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)


exception Invalid_config of string


let default_print_width = 90

let defaultImplementationParserFor filename lexbuf =
  if Filename.check_suffix filename ".re" then (Reason_toolchain.JS.canonical_implementation_with_comments lexbuf, false, false)
  else if Filename.check_suffix filename ".ml" then (Reason_toolchain.ML.canonical_implementation_with_comments lexbuf, true, false)
  else (
    raise (Invalid_config ("Cannot determine default implementation parser for filename '" ^ filename ^ "'."))
  )

let defaultInterfaceParserFor filename lexbuf =
  if Filename.check_suffix filename ".rei" then (Reason_toolchain.JS.canonical_interface_with_comments lexbuf, false, true)
  else if Filename.check_suffix filename ".mli" then (Reason_toolchain.ML.canonical_interface_with_comments lexbuf, true, true)
  else (
    raise (Invalid_config ("Cannot determine default interface parser for filename '" ^ filename ^ "'."))
  )

let reasonBinaryParser chan =
  let (magic_number, filename, ast, comments, parsedAsML, parsedAsInterface) = input_value chan in
  ((ast, comments), parsedAsML, parsedAsInterface)

(*
 * As soon as m17n vends comments, this should be replaced with what is
 * effectively m17n's parser.
 *)
let () =
  let (filename, load_path, prnt, prse, intf, print_width, use_stdin, recoverable, assume_explicit_arity) =
    let filename = ref "" in
    let prnt = ref None in
    let prse = ref None in
    let use_stdin = ref false in
    let intf = ref None in
    let recoverable = ref false in
    let assume_explicit_arity = ref false in
    let print_width = ref None in
    let load_path = ref [] in
    Arg.parse [
        "-I", Arg.String (fun x -> load_path := x :: !load_path), "<path> add <path> to load path";
        "-ignore", Arg.Unit ignore, "ignored";
        "-is-interface-pp", Arg.Bool (fun x -> intf := Some x), "<interface> parse AST as <interface> (either true or false)";
        "-use-stdin", Arg.Bool (fun x -> use_stdin := x), "<use_stdin> parse AST from <use_stdin> (either true, false). You still must provide a file name even if using stdin for errors to be reported";
        "-recoverable", Arg.Bool (fun x -> recoverable := x), "Enable recoverable parser";
        "-assume-explicit-arity", Arg.Unit (fun () -> assume_explicit_arity := true), "If a constructor's argument is a tuple, always interpret it as multiple arguments";
        "-parse", Arg.String (fun x -> prse := Some x), "<parse> parse AST as <parse> (either 'ml', 're', 'binary_reason(for interchange between Reason versions')";
        (* Use a print option of "none" to simply perform a parsing validation -
         * useful for IDE error messages etc.*)
        "-print", Arg.String (fun x -> prnt := Some x), "<print> print AST in <print> (either 'ml', 're', 'binary(default - for compiler input)', 'binary_reason(for interchange between Reason versions)', 'none')";
        "-print-width", Arg.Int (fun x -> print_width := Some x), "<print-width> wrapping width for printing the AST";
      ]
      (fun arg -> filename := arg)
      "Reason: Meta Language Utility";
    (!filename, load_path, !prnt, !prse, !intf, !print_width, !use_stdin, !recoverable, !assume_explicit_arity)
  in
  let print_width = match print_width with
      | None -> default_print_width
      | Some x -> x
  in
  let chan =
    match use_stdin with
      | true -> stdin
      | false ->
          let file_chan = open_in filename in
          seek_in file_chan 0;
          file_chan
  in
  let _ = if recoverable then
    Reason_config.configure ~r:true
  in
  Location.input_name := filename;
  let lexbuf = Lexing.from_channel chan in
  Location.init lexbuf filename;
  let intf = match intf with
    | None when (Filename.check_suffix filename ".rei" || Filename.check_suffix filename ".mli") -> true
    | None -> false
    | Some b -> b
  in
  try
    if intf then (
      let ((ast, comments), parsedAsML, parsedAsInterface) = match prse with
        | None -> (defaultInterfaceParserFor filename) lexbuf
        | Some "binary_reason" -> reasonBinaryParser chan
        | Some "ml" -> ((Reason_toolchain.ML.canonical_interface_with_comments lexbuf), true, true)
        | Some "re" -> ((Reason_toolchain.JS.canonical_interface_with_comments lexbuf), false, true)
        | Some s -> (
          raise (Invalid_config ("Invalid -parse setting for interface '" ^ s ^ "'."))
        )
      in
      let _ =
        if not parsedAsInterface then
          raise (Invalid_config ("The file parsed does not appear to be an interface file.")) in
      let _ = Reason_pprint_ast.configure
          ~width: print_width
          ~assumeExplicitArity
      in
      let thePrinter = match prnt with
        | Some "binary_reason" -> fun comments ast -> (
          (* Our special format for interchange between reason should keep the
           * comments separate.  This is not compatible for input into the
           * ocaml compiler - only for input into another version of Reason. We
           * also store whether or not the binary was originally *parsed* as an
           * interface file.
           *)
          output_value stdout (
            Config.ast_intf_magic_number, filename, ast, comments, parsedAsML, true
          );
        )
        | Some "binary"
        | None -> fun comments ast -> (
          output_string stdout Config.ast_intf_magic_number;
          output_value  stdout filename;
          output_value  stdout ast
        )
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun comments ast -> ())
        | Some "ml" -> Reason_toolchain.ML.print_canonical_interface_with_comments
        | Some "re" -> Reason_toolchain.JS.print_canonical_interface_with_comments
        | Some s -> (
          raise (Invalid_config ("Invalid -print setting for interface '" ^ s ^ "'."))
        )
      in
      thePrinter comments ast
    ) else (
      let ((ast, comments), parsedAsML, parsedAsInterface) = match prse with
        | None -> (defaultImplementationParserFor filename) lexbuf
        | Some "binary_reason" -> reasonBinaryParser chan
        | Some "ml" -> ((Reason_toolchain.ML.canonical_implementation_with_comments lexbuf), true, false)
        | Some "re" -> ((Reason_toolchain.JS.canonical_implementation_with_comments lexbuf), false, false)
        | Some s -> (
          raise (Invalid_config ("Invalid -parse setting for interface '" ^ s ^ "'."))
        )
      in
      let _ = if parsedAsInterface then
          raise (Invalid_config ("The file parsed does not appear to be an implementation file.")) in
      let _ = Reason_pprint_ast.configure
          ~width: print_width
          ~assumeExplicitArity:assume_explicit_arity
      in
      let thePrinter = match prnt with
        | Some "binary_reason" -> fun comments ast -> (
          (* Our special format for interchange between reason should keep the
           * comments separate.  This is not compatible for input into the
           * ocaml compiler - only for input into another version of Reason. We
           * also store whether or not the binary was originally *parsed* as an
           * interface file.
           *)
          output_value stdout (
            Config.ast_impl_magic_number, filename, ast, comments, parsedAsML, false
          );
        )
        | Some "binary"
        | None -> fun comments ast -> (
          output_string stdout Config.ast_impl_magic_number;
          output_value stdout filename;
          output_value stdout ast
        )
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun comments ast -> ())
        | Some "ml" -> Reason_toolchain.ML.print_canonical_implementation_with_comments
        | Some "re" -> Reason_toolchain.JS.print_canonical_implementation_with_comments
        | Some s -> (
          raise (Invalid_config ("Invalid -print setting for implementation '" ^ s ^ "'."))
        )
      in
      thePrinter comments ast
    )
  with
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
    let location = Location.{
      loc_ghost = false;
      loc_start = lexbuf.Lexing.lex_start_p;
      loc_end   = lexbuf.Lexing.lex_curr_p;
    } in
    let exn = Syntaxerr.Error (Syntaxerr.Other location) in
    Location.report_exception Format.err_formatter exn;
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
