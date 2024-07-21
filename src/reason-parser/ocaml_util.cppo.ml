#if OCAML_VERSION >= (4,0,6)
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
#if OCAML_VERSION >= (4,0,8)
  let error = Location.error_of_printer ~loc f x in
  Location.print_report ppf error
#else
  let error = Location.error_of_printer loc f x in
  Location.report_error ppf error
#endif
