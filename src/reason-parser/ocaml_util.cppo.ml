#if OCAML_VERSION >= (4,6,0)
let warn_latin1 lexbuf =
  Location.deprecated (Location.curr lexbuf) "ISO-Latin1 characters in identifiers"
#else
let warn_latin1 lexbuf =
  Location.prerr_warning (Location.curr lexbuf)
    (Warnings.Deprecated "ISO-Latin1 characters in identifiers")
#endif

let print_loc ppf loc =
  Location.print_loc ppf loc


let print_error loc f ppf x =
#if OCAML_VERSION >= (5,3,0)
  let error =
    let f (fmt: Format_doc.formatter) err =
      let doc_f =
        Format_doc.deprecated_printer (fun fmt -> Format.fprintf fmt "%a" f err)
      in
      doc_f fmt
    in
    Location.error_of_printer ~loc f x in
  Location.print_report ppf error
#elif OCAML_VERSION >= (4,8,0)
  let error = Location.error_of_printer ~loc f x in
  Location.print_report ppf error
#else
  let error = Location.error_of_printer loc f x in
  Location.report_error ppf error
#endif
