open Ppxlib

type t = Parsetree.signature

let err = Printer_maker.err

(* Note: filename should only be used with .mli files. See reason_toolchain. *)
let defaultInterfaceParserFor use_stdin filename =
  let open Reason_toolchain in
  let theParser, parsedAsML =
    if Filename.check_suffix filename ".rei"
    then RE.interface_with_comments, false
    else if Filename.check_suffix filename ".mli"
    then ML.interface_with_comments, true
    else
      err
        ("Cannot determine default interface parser for filename '"
        ^ filename
        ^ "'.")
  in
  theParser (setup_lexbuf use_stdin filename), parsedAsML, true

let parse ~use_stdin filetype filename =
  let (ast, comments), parsedAsML, parsedAsInterface =
    match filetype with
    | `Auto -> defaultInterfaceParserFor use_stdin filename
    | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
    | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename
    | `ML ->
      let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
      let intf = Reason_toolchain.ML.interface_with_comments in
      intf lexbuf, true, true
    | `Reason ->
      let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
      let intf = Reason_toolchain.RE.interface_with_comments in
      intf lexbuf, false, true
  in
  if not parsedAsInterface
  then err "The file parsed does not appear to be an interface file."
  else (ast, comments), parsedAsML

let print printtype filename parsedAsML output_chan output_formatter =
  match printtype with
  | `BinaryReason ->
    fun (ast, comments) ->
      (* Our special format for interchange between reason should keep the
       * comments separate.  This is not compatible for input into the
       * ocaml compiler - only for input into another version of Reason. We
       * also store whether or not the binary was originally *parsed* as an
       * interface file.
       *)
      output_value
        output_chan
        ( Ocaml_common.Config.ast_intf_magic_number
        , filename
        , ast
        , comments
        , parsedAsML
        , true )
  | `Binary ->
    fun (ast, _) ->
      let ast =
        ast
        |> Reason_syntax_util.(
             apply_mapper_to_signature remove_stylistic_attrs_mapper)
        |> Reason_syntax_util.(apply_mapper_to_signature backport_letopt_mapper)
      in
      Ppxlib__.Utils.Ast_io.write
        output_chan
        { Ppxlib__.Utils.Ast_io.input_name = filename
        ; input_version =
            Obj.magic
              (module Ppxlib_ast.Compiler_version : Ppxlib_ast.OCaml_version)
        ; ast = Intf ast
        }
        ~add_ppx_context:false
  | `AST ->
    fun (ast, _) ->
      Ocaml_common.Printast.interface
        output_formatter
        (Reason_toolchain.To_current.copy_signature ast)
  | `None -> fun _ -> ()
  | `ML -> Reason_toolchain.ML.print_interface_with_comments output_formatter
  | `Reason ->
    Reason_toolchain.RE.print_interface_with_comments output_formatter
