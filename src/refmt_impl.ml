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

[Usage]: refmt [options] some-file.[re|ml|rei|mli]

   // translate ocaml to reason on stdin
   echo 'let _ = ()' | refmt --print re --parse ml

   // print the AST of a file
   refmt --parse re --print ast some-file.re

   // reformat a file
   refmt --parse re --print re some-file.re

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
  let intf = ref None in
  let recoverable = ref false in
  let assumeExplicitArity = ref false in
  let heuristics_file = ref None in
  let print_width = ref None in
  let print_help = ref false in
  let in_place = ref false in
  let output_file = ref None in
  let options = [
    "-is-interface-pp", Arg.Bool (fun x -> prerr_endline "-is-interface-pp is deprecated; use -i or --interface instead"; intf := Some x), "";
    "--is-interface-pp", Arg.Bool (fun x -> prerr_endline "--is-interface-pp is deprecated; use -i or --interface instead"; intf := Some x), "";
    "--interface", Arg.Bool (fun x -> intf := Some x), "<interface>, -i <interface>; parse AST as an interface (either true or false; default false)";
    "-i", Arg.Bool (fun x -> intf := Some x), "<interface>, --interface <interface>; parse AST as an interface (either true or false; default false)";
    "-use-stdin", Arg.Bool (fun x -> prerr_endline "-use-stdin is deprecated; usage is assumed if not specifying a filename"), "";
    "--use-stdin", Arg.Bool (fun x -> prerr_endline "--use-stdin is deprecated; usage is assumed if not specifying a filename"), "";
    "-recoverable", Arg.Bool (fun x -> prerr_endline "-recoverable is deprecated; use --recoverable instead"; recoverable := x), "";
    "--recoverable", Arg.Bool (fun x -> recoverable := x), "<recoverable>; enable or disable recoverable parser (either true or false; default false)";
    "-assume-explicit-arity", Arg.Unit (fun () -> prerr_endline "-assume-explicit-arity is deprecated; use --assume-explicit-arity instead" ; assumeExplicitArity := true), "";
    "--assume-explicit-arity", Arg.Unit (fun () -> assumeExplicitArity := true), "If a constructor's argument is a tuple, always interpret it as multiple arguments";
    "-parse", Arg.String (fun x -> prerr_endline "-parse is deprecated; use --parse instead"; prse := Some x), "";
    "--parse", Arg.String (fun x -> prse := Some x), "<parse>, parse AST as <parse> (either 'ml', 're', 'binary_reason(for interchange between Reason versions)', 'binary (from the ocaml compiler)')";
    (* Use a print option of "none" to simply perform a parsing validation -
     * useful for IDE error messages etc.*)
    "-print", Arg.String (fun x -> prerr_endline "-print is deprecated; use --print instead"; prnt := Some x), "";
    "--print", Arg.String (fun x -> prnt := Some x), "<print>, print AST in <print> (either 'ml', 're', 'binary(default - for compiler input)', 'binary_reason(for interchange between Reason versions)', 'ast (print human readable directly)', 'none')";
    "-print-width", Arg.Int (fun x -> prerr_endline "-print-width is deprecated; use --print-width instead"; print_width := Some x), "";
    "--print-width", Arg.Int (fun x -> print_width := Some x), "<print-width>, wrapping width for printing the AST";
    "--output", Arg.String (fun x -> output_file := Some x), "<output-file>, -o <output-file>, target file for output; default [stdout]";
    "-o", Arg.String (fun x -> output_file := Some x), "<output-file>, --output <output-file>, target file for output; default [stdout]";
    "--in-place", Arg.Unit (fun () -> in_place := true), "Reformat a file in-place (defaults to not in place).";
    "-heuristics-file", Arg.String (fun x -> prerr_endline "-heuristics-file is deprecated; use --heuristics-file instead"; heuristics_file := Some x), "";
    "--heuristics-file", Arg.String (fun x -> heuristics_file := Some x),
    "<path>, load path as a heuristics file to specify which constructors are defined with multi-arguments. Mostly used in removing [@implicit_arity] introduced from OCaml conversion.\n\t\texample.txt:\n\t\tConstructor1\n\t\tConstructor2";
    "-h", Arg.Unit (fun () -> print_help := true), " Display this list of options";
  ] in
  let () = Arg.parse options (fun arg -> filename := arg) usage in
  let print_help = !print_help in
  let filename = !filename in
  let use_stdin = (filename = "") in
  let () =
    let has_print = match !prnt with
      | None -> false
      | Some x -> true
    in
    let has_parse = match !prse with
      | None -> false
      | Some x -> true
    in

    if (filename = "" && not (has_parse && has_print)) || print_help then
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
  let () =
      if use_stdin && !in_place then
          raise (Invalid_config "Cannot write in place to stdin.")
  in
  Reason_config.configure ~r:!recoverable;
  Location.input_name := filename;
  let intf = match !intf with
    | None when (Filename.check_suffix filename ".rei" || Filename.check_suffix filename ".mli") -> true
    | None -> false
    | Some b -> b
  in
  try
    if intf then (
      let ((ast, comments), parsedAsML, parsedAsInterface) = match !prse with
        | None -> (defaultInterfaceParserFor use_stdin filename)
        | Some "binary_reason" -> reasonBinaryParser use_stdin filename
        | Some "binary" -> ocamlBinaryParser use_stdin filename true
        | Some "ml" -> (Reason_toolchain.ML.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, true)
        | Some "re" -> (Reason_toolchain.JS.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), false, true)
        | Some s -> (
          raise (Invalid_config ("Invalid --parse setting for interface '" ^ s ^ "'."))
        )
      in
      let _ =
          if not parsedAsInterface then
              raise (Invalid_config ("The file parsed does not appear to be an interface file."))
      in
      let _ = Reason_pprint_ast.configure
          ~width: print_width
          ~assumeExplicitArity: !assumeExplicitArity
          ~constructorLists
      in
      let output_chan = match !output_file with
                        | Some name ->
                                if !in_place then
                                    raise (Invalid_config "Cannot specify --output and --in-place.")
                                else open_out name
                        | None ->
                                if !in_place then
                                    open_out filename
                                else stdout
      in
      let formatter = Format.formatter_of_out_channel output_chan
      in
      let thePrinter = match !prnt with
        | Some "binary_reason" -> fun (ast, comments) -> (
          (* Our special format for interchange between reason should keep the
           * comments separate.  This is not compatible for input into the
           * ocaml compiler - only for input into another version of Reason. We
           * also store whether or not the binary was originally *parsed* as an
           * interface file.
           *)
          output_value output_chan (
            Config.ast_intf_magic_number, filename, ast, comments, parsedAsML, true
          );
        )
        | Some "binary"
        | None -> fun (ast, comments) -> (
          output_string output_chan Config.ast_intf_magic_number;
          output_value  output_chan filename;
          output_value  output_chan ast
        )
        | Some "ast" -> fun (ast, comments) -> (
          Printast.interface formatter ast
        )
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun (ast, comments) -> ())
        | Some "ml" ->
                Reason_toolchain.ML.print_canonical_interface_with_comments formatter
        | Some "re" ->
                Reason_toolchain.JS.print_canonical_interface_with_comments formatter
        | Some s -> (
          raise (Invalid_config ("Invalid --print setting for interface '" ^ s ^ "'."))
        )
      in
      thePrinter (ast, comments);
      match !output_file with
      | Some _ -> close_out output_chan
      | None -> ()
    ) else (
      let ((ast, comments), parsedAsML, parsedAsInterface) = match !prse with
        | None -> (defaultImplementationParserFor use_stdin filename)
        | Some "binary_reason" -> reasonBinaryParser use_stdin filename
        | Some "binary" -> ocamlBinaryParser use_stdin filename false
        | Some "ml" -> (Reason_toolchain.ML.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, false)
        | Some "re" -> (Reason_toolchain.JS.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), false, false)
        | Some s -> (
          raise (Invalid_config ("Invalid --parse setting for interface '" ^ s ^ "'."))
        )
      in
      let _ =
          if parsedAsInterface then
              raise (Invalid_config ("The file parsed does not appear to be an implementation file."))
      in
      let _ = Reason_pprint_ast.configure
          ~width: print_width
          ~assumeExplicitArity: !assumeExplicitArity
          ~constructorLists
      in
      let output_chan = match !output_file with
                        | Some name ->
                                if !in_place then
                                    raise (Invalid_config "Cannot specify --output and --in-place.")
                                else open_out name
                        | None ->
                                if !in_place then
                                    open_out filename
                                else stdout
      in
      let formatter = Format.formatter_of_out_channel output_chan
      in
      let thePrinter = match !prnt with
        | Some "binary_reason" -> fun (ast, comments) -> (
          (* Our special format for interchange between reason should keep the
           * comments separate.  This is not compatible for input into the
           * ocaml compiler - only for input into another version of Reason. We
           * also store whether or not the binary was originally *parsed* as an
           * interface file.
           *)
          output_value output_chan (
            Config.ast_impl_magic_number, filename, ast, comments, parsedAsML, false
          );
        )
        | Some "binary"
        | None -> fun (ast, comments) -> (
          output_string output_chan Config.ast_impl_magic_number;
          output_value  output_chan filename;
          output_value  output_chan ast
        )
        | Some "ast" -> fun (ast, comments) -> (
          Printast.implementation formatter ast
        )
        (* If you don't wrap the function in parens, it's a totally different
         * meaning #thanksOCaml *)
        | Some "none" -> (fun (ast, comments) -> ())
        | Some "ml" ->
                Reason_toolchain.ML.print_canonical_implementation_with_comments formatter
        | Some "re" ->
                Reason_toolchain.JS.print_canonical_implementation_with_comments formatter
        | Some s -> (
          raise (Invalid_config ("Invalid --print setting for implementation '" ^ s ^ "'."))
        )
      in
      thePrinter (ast, comments);
      match !output_file with
      | Some _ -> close_out output_chan
      | None -> ()
    )
  with
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
