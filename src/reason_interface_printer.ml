module Reason_interface_printer : Printer_maker.PRINTER =
    struct
        type t = Parsetree.signature
        exception Invalid_config = Printer_maker.Invalid_config

        (* Note: filename should only be used with .mli files. See reason_toolchain. *)
        let defaultInterfaceParserFor use_stdin filename =
            if Filename.check_suffix filename ".rei"
            then (Reason_toolchain.JS.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename) , false, true)
            else if Filename.check_suffix filename ".mli"
            then (Reason_toolchain.ML.canonical_interface_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, true)
            else (
                raise (Invalid_config ("Cannot determine default interface parser for filename '" ^ filename ^ "'."))
                )

        let parse filetype use_stdin filename =
            let ((ast, comments), parsedAsML, parsedAsInterface) =
            (match filetype with
            | None -> defaultInterfaceParserFor use_stdin filename
            | Some `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
            | Some `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename true
            | Some `ML ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let intf = Reason_toolchain.ML.canonical_interface_with_comments in
                    ((intf lexbuf), true, true)
            | Some `Reason ->
                    let lexbuf = Reason_toolchain.setup_lexbuf use_stdin filename in
                    let intf = Reason_toolchain.JS.canonical_interface_with_comments in
                    ((intf lexbuf), false, true))
            in
            if not parsedAsInterface then
                raise (Invalid_config ("The file parsed does not appear to be an interface file."))
            else ((ast, comments), parsedAsML)

        let makePrinter printtype filename parsedAsML output_chan output_formatter =
            match printtype with
                    | Some `BinaryReason -> fun (ast, comments) -> (
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
                    | Some `Binary
                    | None -> fun (ast, comments) -> (
                      output_string output_chan Config.ast_intf_magic_number;
                      output_value  output_chan filename;
                      output_value  output_chan ast
                    )
                    | Some `AST -> fun (ast, comments) -> (
                      Printast.interface output_formatter ast
                    )
                    (* If you don't wrap the function in parens, it's a totally different
                     * meaning #thanksOCaml *)
                    | Some `None -> (fun (ast, comments) -> ())
                    | Some `ML -> Reason_toolchain.ML.print_canonical_interface_with_comments output_formatter
                    | Some `Reason -> Reason_toolchain.JS.print_canonical_interface_with_comments output_formatter
    end;;
