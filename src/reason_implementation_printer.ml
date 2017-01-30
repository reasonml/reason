module Reason_implementation_printer : Printer_maker.PRINTER =
    struct
        type t = Parsetree.structure
        exception Invalid_config = Printer_maker.Invalid_config

        (* Note: filename should only be used with .ml files. See reason_toolchain. *)
        let defaultImplementationParserFor use_stdin filename =
          if Filename.check_suffix filename ".re" then (Reason_toolchain.JS.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), false, false)
          else if Filename.check_suffix filename ".ml" then (Reason_toolchain.ML.canonical_implementation_with_comments (Reason_toolchain.setup_lexbuf use_stdin filename), true, false)
          else (
            raise (Invalid_config ("Cannot determine default implementation parser for filename '" ^ filename ^ "'."))
          )

        let parse filetype use_stdin filename =
            let ((ast, comments), parsedAsML, parsedAsInterface) =
            (match filetype with
            | `Auto -> defaultImplementationParserFor use_stdin filename
            | `BinaryReason -> Printer_maker.reasonBinaryParser use_stdin filename
            | `Binary -> Printer_maker.ocamlBinaryParser use_stdin filename false
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
              raise (Invalid_config ("The file parsed does not appear to be an implementation file."))
            else ((ast, comments), parsedAsML)

        let makePrinter printtype filename parsedAsML output_chan output_formatter =
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
              output_string output_chan Config.ast_impl_magic_number;
              output_value  output_chan filename;
              output_value  output_chan ast
            )
            | `AST -> fun (ast, comments) -> (
              Printast.implementation output_formatter ast
            )
            (* If you don't wrap the function in parens, it's a totally different
             * meaning #thanksOCaml *)
            | `None -> (fun (ast, comments) -> ())
            | `ML -> Reason_toolchain.ML.print_canonical_implementation_with_comments output_formatter
            | `Reason -> Reason_toolchain.JS.print_canonical_implementation_with_comments output_formatter
    end;;
