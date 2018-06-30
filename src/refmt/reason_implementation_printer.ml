open Migrate_parsetree
open Ast_406

type t = Parsetree.structure
let err = Printer_maker.err

(* Note: filename should only be used with .ml files. See reason_toolchain. *)
let defaultImplementationParserFor use_stdin filename =
  let open Reason_toolchain in
  let (theParser, parsedAsML) =
    if Filename.check_suffix filename ".re"
    then (RE.implementation_with_comments, false)
    else if Filename.check_suffix filename ".ml"
    then (ML.implementation_with_comments, true)
    else err ("Cannot determine default implementation parser for filename '" ^ filename ^ "'.")
  in
  theParser (setup_lexbuf use_stdin filename), parsedAsML, false

let parse ~use_stdin filetype filename =
  let ((ast, comments), parsedAsML, parsedAsInterface) =
  (match filetype with
  | `Auto -> defaultImplementationParserFor use_stdin filename
  | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
  | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename
  | `ML ->
          let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
          let impl = Reason_toolchain.ML.implementation_with_comments in
          (impl lexbuf, true, false)
  | `Reason ->
          let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
          let impl = Reason_toolchain.RE.implementation_with_comments in
          (impl lexbuf, false, false))
  in
  if parsedAsInterface then
    err "The file parsed does not appear to be an implementation file."
  else
    ((ast, comments), parsedAsML)

let print printtype filename parsedAsML output_chan output_formatter =
  match printtype with
  | `BinaryReason -> fun (ast, comments) -> (
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
  | `Binary -> fun (ast, _) ->
    let ast =
      Reason_syntax_util.(apply_mapper_to_structure ast remove_stylistic_attrs_mapper)
    in
    Ast_io.to_channel output_chan filename
      (Ast_io.Impl ((module OCaml_current),
                    Reason_toolchain.To_current.copy_structure ast))
  | `AST -> fun (ast, _) -> (
    Printast.implementation output_formatter
      (Reason_toolchain.To_current.copy_structure ast)
  )
  (* If you don't wrap the function in parens, it's a totally different
   * meaning #thanksOCaml *)
  | `None -> (fun _ -> ())
  | `ML -> Reason_toolchain.ML.print_implementation_with_comments output_formatter
  | `Reason -> Reason_toolchain.RE.print_implementation_with_comments output_formatter
