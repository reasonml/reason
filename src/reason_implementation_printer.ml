open Migrate_parsetree
open Ast_404

module Reason_implementation_printer : Printer_maker.PRINTER =
    struct
        type q = Parsetree.structure_item
        type t = Parsetree.structure
        let err = Printer_maker.err

        (* Note: filename should only be used with .ml files. See reason_toolchain. *)
        let defaultImplementationParserFor use_stdin filename =
          let open Reason_toolchain in
          let (theParser, parsedAsML) =
            if Filename.check_suffix filename ".re"
            then (JS.canonical_implementation_with_comments, false)
            else if Filename.check_suffix filename ".ml"
            then (ML.canonical_implementation_with_comments, true)
            else err ("Cannot determine default implementation parser for filename '" ^ filename ^ "'.")
          in
          theParser (setup_lexbuf use_stdin filename), parsedAsML, false

        let ppx_deriving_runtime =
          let open Asttypes in
          let open Parsetree in
          let open Longident in
          let open Ast_helper in
          let open Location in
          let mktypealias (name, params, types) =
            let manifest = Typ.constr (mknoloc (Lident name)) types in
            Str.type_ Nonrecursive [Type.mk ~params ~kind:Ptype_abstract ~manifest (mknoloc name)]
          in
          let mkmodulealias name =
            Str.module_ (Mb.mk (mknoloc name) (Mod.ident (mknoloc (Lident name))))
          in
          let mkmoduleinclude name =
            Str.include_ (Incl.mk (Mod.ident (mknoloc (Lident name))))
          in
          let type_aliases =
            let n s = (s, [], []) in
            let a s = (s, [(Typ.var "a"), Invariant], [Typ.var "a"]) in
            List.map mktypealias [n "int"; n "char"; n "string"; n "float"; n "bool";
                                  n "unit"; n "exn"; a "array"; a "list"; a "option";
                                  n "nativeint"; n "int32"; n "int64"; a "lazy_t";
                                  n "bytes"]
          in
          let module_aliases = List.map mkmodulealias ["Pervasives"; "Char"; "String";
            "Printexc"; "Array"; "List"; "Nativeint"; "Int32"; "Int64"; "Lazy";
            "Bytes"; "Hashtbl"; "Queue"; "Stack"; "Set"; "Weak"; "Printf"; "Format";
            "Buffer"]
          in
          let module_includes = List.map mkmoduleinclude ["Pervasives"]
          in
          let structure_items = type_aliases @ module_aliases @ module_includes in
          Str.module_ (Mb.mk (mknoloc "Ppx_deriving_runtime")
                             (Mod.structure structure_items))

        let parse filetype use_stdin filename =
            let ((ast, comments), parsedAsML, parsedAsInterface) =
            (match filetype with
            | `Auto -> defaultImplementationParserFor use_stdin filename
            | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
            | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename
            | `ML ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let impl = Reason_toolchain.ML.canonical_implementation_with_comments in
                    (impl lexbuf, true, false)
            | `Reason ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let impl = Reason_toolchain.JS.canonical_implementation_with_comments in
                    (impl lexbuf, false, false))
            in
            if parsedAsInterface then
              err "The file parsed does not appear to be an implementation file."
            else if !Reason_config.add_printers then
              (* NB: Not idempotent. *)
              ((ppx_deriving_runtime::ast, comments), parsedAsML)
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
            | `Binary -> fun (ast, comments) -> (
               Ast_io.to_channel output_chan filename
                 (Ast_io.Impl ((module OCaml_current),
                               Reason_toolchain.To_current.copy_structure ast))
            )
            | `AST -> fun (ast, comments) -> (
              Printast.implementation output_formatter
                (Reason_toolchain.To_current.copy_structure ast)
            )
            (* If you don't wrap the function in parens, it's a totally different
             * meaning #thanksOCaml *)
            | `None -> (fun (ast, comments) -> ())
            | `ML -> Reason_toolchain.ML.print_canonical_implementation_with_comments output_formatter
            | `Reason -> Reason_toolchain.JS.print_canonical_implementation_with_comments output_formatter
    end;;
