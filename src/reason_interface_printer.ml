open Migrate_parsetree
open Ast_404

module Reason_interface_printer : Printer_maker.PRINTER =
    struct
        type q = Parsetree.signature_item
        type t = Parsetree.signature
        let err = Printer_maker.err

        (* Note: filename should only be used with .mli files. See reason_toolchain. *)
        let defaultInterfaceParserFor use_stdin filename =
          let open Reason_toolchain in
          let (_parser, thing) =
            if Filename.check_suffix filename ".rei"
            then (JS.canonical_interface_with_comments, false)
            else if Filename.check_suffix filename ".mli"
            then (ML.canonical_interface_with_comments, true)
            else err ("Cannot determine default interface parser for filename '" ^ filename ^ "'.")
          in
          _parser (setup_lexbuf use_stdin filename), thing, false

        let ppx_deriving_runtime =
          let open Asttypes in
          let open Parsetree in
          let open Longident in
          let open Ast_helper in
          let open Location in
          let (@@) f g x = f (g x) in
          let mkstr = mknoloc in
          let mklid = mknoloc @@ Longident.parse in
          let mktypealias (name, params, types) =
            let manifest = Typ.constr (mklid name) types in
            Sig.type_ Nonrecursive [Type.mk ~params ~kind:Ptype_abstract ~manifest (mknoloc name)]
          in
          let type_aliases =
            let n s = (s, [], []) in
            let a s = (s, [(Typ.var "a"), Invariant], [Typ.var "a"]) in
            List.map mktypealias [n "int"; n "char"; n "string"; n "float"; n "bool";
                                  n "unit"; n "exn"; a "array"; a "list"; a "option";
                                  n "nativeint"; n "int32"; n "int64"; a "lazy_t";
                                  n "bytes"]
          in
          let module_aliases =
            let mktysubst (mod_, ty) =
              let combined = mklid (mod_ ^ "." ^ ty) in
              Pwith_typesubst (Type.mk (mkstr ty) ~manifest:(Typ.constr combined []))
            in
            let printf =
              let n = "Printf" in
              Md.mk (mkstr n) Mty.typeof_ (Mod.ident (mklid n))
            in
            let fmt =
              let n = "Format" in
              let mty = Mty.with_
                          (Mty.typeof_ (Mod.ident (mklid n)))
                          (List.map (mktysubst @@ (fun s -> n, s))
                                    [ "formatter_out_functions";
                                      "formatter_tag_functions";
                                      "formatter" ])
              in Md.mk (mkstr n) mty
            in
            [Sig.module_ printf; Sig.module_ fmt]
          in
          let structure_items = type_aliases @ module_aliases in
          Sig.module_ (Md.mk (mknoloc "Ppx_deriving_runtime")
                             (Mty.signature structure_items))

        let parse filetype use_stdin filename =
            let ((ast, comments), parsedAsML, parsedAsInterface) =
            (match filetype with
            | `Auto -> defaultInterfaceParserFor use_stdin filename
            | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
            | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename
            | `ML ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let intf = Reason_toolchain.ML.canonical_interface_with_comments in
                    ((intf lexbuf), true, true)
            | `Reason ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let intf = Reason_toolchain.JS.canonical_interface_with_comments in
                    ((intf lexbuf), false, true))
            in
            if not parsedAsInterface then
              err "The file parsed does not appear to be an interface file."
            else if !Reason_config.add_printers then
              (* NB: Not idempotent. *)
              ((ppx_deriving_runtime::ast, comments), parsedAsML)
            else ((ast, comments), parsedAsML)

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
                        Config.ast_intf_magic_number, filename, ast, comments, parsedAsML, true
                      );
                    )
                    | `Binary -> fun (ast, comments) -> (
                        Ast_io.to_channel output_chan filename
                          (Ast_io.Intf ((module OCaml_current),
                                        Reason_toolchain.To_current.copy_signature ast))
                    )
                    | `AST -> fun (ast, comments) -> (
                        Printast.interface output_formatter
                          (Reason_toolchain.To_current.copy_signature ast)
                    )
                    (* If you don't wrap the function in parens, it's a totally different
                     * meaning #thanksOCaml *)
                    | `None -> (fun (ast, comments) -> ())
                    | `ML -> Reason_toolchain.ML.print_canonical_interface_with_comments output_formatter
                    | `Reason -> Reason_toolchain.JS.print_canonical_interface_with_comments output_formatter
    end;;
