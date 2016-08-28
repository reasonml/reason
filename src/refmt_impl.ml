(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

open Lexing

exception Invalid_config of string


let default_print_width = 100

(* Note: filename should only be used with .ml files. See reason_toolchain. *)
let defaultImplementationParserFor use_stdin filename =
  if Filename.check_suffix filename ".re" then (Reason_toolchain.JS.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), false, false)
  else if Filename.check_suffix filename ".ml" then (Reason_toolchain.ML.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, false)
  else (
    raise (Invalid_config ("Cannot determine default implementation parser for filename '" ^ filename ^ "'."))
  )

(* Note: filename should only be used with .mli files. See reason_toolchain. *)
let defaultInterfaceParserFor use_stdin filename =
  if Filename.check_suffix filename ".rei" then (Reason_toolchain.JS.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename) , false, true)
  else if Filename.check_suffix filename ".mli" then (Reason_toolchain.ML.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, true)
  else (
    raise (Invalid_config ("Cannot determine default interface parser for filename '" ^ filename ^ "'."))
  )

let reasonBinaryParser use_stdin filename =
  let chan =
    match use_stdin with
      | true -> stdin
      | false ->
          let file_chan = open_in filename in
          seek_in file_chan 0;
          file_chan
  in
  let (magic_number, filename, ast, comments, parsedAsML, parsedAsInterface) = input_value chan in
  ((ast, comments), parsedAsML, parsedAsInterface)

let ocamlBinaryParser use_stdin filename parsedAsInterface =
  let chan =
    match use_stdin with
      | true -> stdin
      | false ->
          let file_chan = open_in filename in
          seek_in file_chan 0;
          file_chan
  in
  let _ = really_input_string chan (String.length Config.ast_impl_magic_number) in
  let _ = input_value chan in
  let ast = input_value chan in
  ((ast, []), true, parsedAsInterface)

let usage = {|Reason: Meta Language Utility

[Usage]: refmt [options] some-file.[re|ml]

   // translate ocaml to reason on stdin
   echo 'let _ = ()' | refmt -print re -parse ml -use-stdin true -is-interface-pp false

   // print the AST of a file
   refmt -parse re -print ast some-file.re

   // reformat a file
   refmt -parse re -print re some-file.re

[Options]:
|}

(*
 * As soon as m17n vends comments, this should be replaced with what is
 * effectively m17n's parser.
 *)
let () =
  let filename = ref "" in
  let prnt = ref None in
  let prse = ref None in
  let use_stdin = ref false in
  let intf = ref None in
  let recoverable = ref false in
  let assumeExplicitArity = ref false in
  let heuristics_file = ref None in
  let print_width = ref None in
  let print_help = ref false in
  let options = [
    "-ignore", Arg.Unit ignore, "ignored";
    "-is-interface-pp", Arg.Bool (fun x -> intf := Some x), "<interface>, parse AST as <interface> (either true or false)";
    "-use-stdin", Arg.Bool (fun x -> use_stdin := x), "<use_stdin>, parse AST from <use_stdin> (either true, false). You still must provide a file name even if using stdin for errors to be reported";
    "-recoverable", Arg.Bool (fun x -> recoverable := x), "Enable recoverable parser";
    "-assume-explicit-arity", Arg.Unit (fun () -> assumeExplicitArity := true), "If a constructor's argument is a tuple, always interpret it as multiple arguments";
    "-parse", Arg.String (fun x -> prse := Some x), "<parse>, parse AST as <parse> (either 'ml', 're', 'binary_reason(for interchange between Reason versions)', 'binary (from the ocaml compiler)')";
    (* Use a print option of "none" to simply perform a parsing validation -
     * useful for IDE error messages etc.*)
    "-print", Arg.String (fun x -> prnt := Some x), "<print>, print AST in <print> (either 'ml', 're', 'binary(default - for compiler input)', 'binary_reason(for interchange between Reason versions)', 'ast (print human readable directly)', 'none')";
    "-print-width", Arg.Int (fun x -> print_width := Some x), "<print-width>, wrapping width for printing the AST";
    "-heuristics-file", Arg.String (fun x -> heuristics_file := Some x),
    "<path>, load path as a heuristics file to specify which constructors are defined with multi-arguments. Mostly used in removing [@implicit_arity] introduced from OCaml conversion.\n\t\texample.txt:\n\t\tConstructor1\n\t\tConstructor2";
    "-h", Arg.Unit (fun () -> print_help := true), " Display this list of options";
  ] in
  let () = Arg.parse options (fun arg -> filename := arg) usage in
  let print_help = !print_help in
  let filename = !filename in
  let use_stdin = !use_stdin in
  let _ =
    if (filename = "" && not use_stdin) || print_help then
      let () = Arg.usage options usage in
        exit 1;
  in
  let print_width = match !print_width with
    | None -> default_print_width
    | Some x -> x
  in
  let constructorLists = match !heuristics_file with
    | None -> []
    | Some file ->
      let list = ref [] in
      let chan = open_in file in
      try
        while true; do
          list := input_line chan :: !list
        done; []
      with End_of_file ->
        close_in chan;
        List.rev !list
  in
  let _ = if !recoverable then
    Reason_config.configure ~r:true
  in
  Location.input_name := filename;
  let intf = match !intf with
    | None when (Filename.check_suffix filename ".rei" || Filename.check_suffix filename ".mli") -> true
    | None ->
      if use_stdin then
        raise (Invalid_config ("Unable to determine if stdin input is an interface file. Invalid -is-interface-pp setting."))
      else
        false
    | Some b -> b
  in
  try
    if intf then (
      let ((ast, comments), parsedAsML, parsedAsInterface) = match !prse with
        | None -> (defaultInterfaceParserFor use_stdin filename)
        | Some "binary_reason" -> reasonBinaryParser use_stdin filename
        | Some "binary" -> ocamlBinaryParser use_stdin filename true
        | Some "ml" -> ((Reason_toolchain.ML.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename)), true, true)
        | Some "re" -> ((Reason_toolchain.JS.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename)), false, true)
        | Some s -> (
          raise (Invalid_config ("Invalid -parse setting for interface '" ^ s ^ "'."))
        )
      in
      let _ =
        if not parsedAsInterface then
          raise (Invalid_config ("The file parsed does not appear to be an interface file.")) in
      let _ = Reason_pprint_ast.configure
          ~width: print_width
          ~assumeExplicitArity: !assumeExplicitArity
          ~constructorLists
      in
      let thePrinter = match !prnt with
        | Some "binary_reason" -> fun (ast, comments) -> (
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
        | None -> fun (ast, comments) -> (
          output_string stdout Config.ast_intf_magic_number;
          output_value  stdout filename;
          output_value  stdout ast
        )
        | Some "ast" -> fun (ast, comments) -> (
          Printast.interface Format.std_formatter ast
        )
        | Some "json" -> fun (ast, comments) -> (
          Reason_astjson.print_intf ast
          )
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun (ast, comments) -> ())
        | Some "ml" -> Reason_toolchain.ML.print_canonical_interface_with_comments
        | Some "re" -> Reason_toolchain.JS.print_canonical_interface_with_comments
        | Some s -> (
          raise (Invalid_config ("Invalid -print setting for interface '" ^ s ^ "'."))
        )
      in
      thePrinter (ast, comments)
    ) else (
      let ((ast, comments), parsedAsML, parsedAsInterface) = match !prse with
        | None -> (defaultImplementationParserFor use_stdin filename)
        | Some "binary_reason" -> reasonBinaryParser use_stdin filename
        | Some "binary" -> ocamlBinaryParser use_stdin filename false
        | Some "ml" -> (Reason_toolchain.ML.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, false)
        | Some "re" -> (Reason_toolchain.JS.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), false, false)
        | Some s -> (
          raise (Invalid_config ("Invalid -parse setting for interface '" ^ s ^ "'."))
        )
      in
      let _ = if parsedAsInterface then
          raise (Invalid_config ("The file parsed does not appear to be an implementation file.")) in
      let _ = Reason_pprint_ast.configure
          ~width: print_width
          ~assumeExplicitArity: !assumeExplicitArity
          ~constructorLists
      in
      let thePrinter = match !prnt with
        | Some "binary_reason" -> fun (ast, comments) -> (
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
        | None -> fun (ast, comments) -> (
          output_string stdout Config.ast_impl_magic_number;
          output_value stdout filename;
          output_value stdout ast
        )
        | Some "ast" -> fun (ast, comments) -> (
          Printast.implementation Format.std_formatter ast
        )
        | Some "json" -> fun (ast, comments) -> (
          Reason_astjson.print_ast ast
          )
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun (ast, comments) -> ())
        | Some "ml" -> Reason_toolchain.ML.print_canonical_implementation_with_comments
        | Some "re" -> Reason_toolchain.JS.print_canonical_implementation_with_comments
        | Some s -> (
          raise (Invalid_config ("Invalid -print setting for implementation '" ^ s ^ "'."))
        )
      in
      thePrinter (ast, comments)
    )
  with
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
