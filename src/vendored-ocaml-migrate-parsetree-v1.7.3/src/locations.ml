type old_location_error (*IF_NOT_AT_LEAST 408 = Location.error *) = {
    loc: Location.t;
    msg: string;
    sub: old_location_error list;
    if_highlight: string;
  }

type location_msg = (Format.formatter -> unit) Location.loc

type location_report_kind (*IF_AT_LEAST 408 = Location.report_kind *) =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

type location_report (*IF_AT_LEAST 408 = Location.report *) = {
  kind : location_report_kind;
  main : location_msg;
  sub : location_msg list;
}

type location_error (*IF_AT_LEAST 408 = Location.error *) (*IF_NOT_AT_LEAST 408 = old_location_error *)

type error_type = [`Report of location_report | `Old_error of old_location_error]

let error_type_of_location_error : location_error -> error_type = fun x ->
  (*IF_AT_LEAST 408 `Report x *)
  (*IF_NOT_AT_LEAST 408 `Old_error x *)

let location_error_of_exn : exn -> location_error = fun exn ->
  (*IF_AT_LEAST 408 match Location.error_of_exn exn with None | Some `Already_displayed -> raise exn | Some (`Ok e) -> e *)
  (*IF_NOT_AT_LEAST 408 match Migrate_parsetree_compiler_functions.error_of_exn exn with None -> raise exn | Some e -> e*)

let extension_of_error ~mk_pstr ~mk_extension ~mk_string_constant (error : location_error) =
  match error_type_of_location_error error with
  | `Old_error old_error ->
    let rec extension_of_old_error ({loc; msg; if_highlight = _; sub} : old_location_error) =
      { Location.loc; txt = "ocaml.error" },
      mk_pstr ((mk_string_constant msg) ::
               (List.map (fun ext -> mk_extension (extension_of_old_error ext)) sub)) in
    extension_of_old_error old_error
  | `Report report ->
    let extension_of_report ({kind; main; sub} : location_report) =
      if kind <> Report_error then
        raise (Invalid_argument "extension_of_error: expected kind Report_error");
      let str_of_pp pp_msg = Format.asprintf "%t" pp_msg in
      let extension_of_sub (sub : location_msg) =
        { Location.loc = sub.loc; txt = "ocaml.error" },
        mk_pstr ([mk_string_constant (str_of_pp sub.txt)])
      in
      { Location.loc = main.loc; txt = "ocaml.error" },
      mk_pstr (mk_string_constant (str_of_pp main.txt) ::
               List.map (fun msg -> mk_extension (extension_of_sub msg)) sub) in
    extension_of_report report

let error_of_exn exn =
  try Some (location_error_of_exn exn) with _ -> None

let register_error_of_exn f = Location.register_error_of_exn f

let report_exception ppf exn = Location.report_exception ppf exn

let errorf ~loc fmt = Location.errorf ~loc ~sub:[] fmt

let raise_errorf ?(loc = Location.none) fmt = Location.raise_errorf ~loc ~sub:[] fmt

let _get_error_message_old location_error =
  location_error.msg

let _get_error_message_new location_error =
  let buff = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buff in
  location_error.main.txt ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buff

let get_error_message location_error =
  (*IF_NOT_AT_LEAST 408 _get_error_message_old location_error*)
  (*IF_AT_LEAST 408 _get_error_message_new location_error*)

let _set_error_message_old location_error msg =
  { location_error with msg; }

let _set_error_message_new location_error msg =
  let txt ppf = Format.pp_print_string ppf msg in
  let main = { location_error.main with txt; } in
  { location_error with main }

let set_error_message location_error msg =
  (*IF_NOT_AT_LEAST 408 _set_error_message_old location_error msg*)
  (*IF_AT_LEAST 408 _set_error_message_new location_error msg*)

let make_error_of_message_old ~loc msg ~sub =
  let sub = List.map (fun (loc, msg) -> { loc; msg; sub = []; if_highlight = msg; }) sub in
  { loc; msg; sub; if_highlight = msg; }

let make_error_of_message_new ~loc msg ~sub =
  let mk_txt x ppf = Format.pp_print_string ppf x in
  let mk loc x = { Location.loc; txt = mk_txt x; } in
  { kind = Report_error;
    main = mk loc msg;
    sub = List.map (fun (loc, msg) -> mk loc msg) sub; }

let make_error_of_message ~loc msg ~sub =
  (*IF_NOT_AT_LEAST 408 make_error_of_message_old ~loc msg ~sub*)
  (*IF_AT_LEAST 408 make_error_of_message_new ~loc msg ~sub*)

let print_error ppf err =
  (*IF_NOT_AT_LEAST 408 Location.report_error ppf err*)
  (*IF_AT_LEAST 408 Location.print_report ppf err*)

module type Helpers_intf = sig
  type nonrec location_error = location_error
  val error_of_exn : exn -> location_error option
  val register_error_of_exn : (exn -> location_error option) -> unit
  val report_exception : Format.formatter -> exn -> unit
  val get_error_message : location_error -> string
  val set_error_message : location_error -> string -> location_error
  val make_error_of_message : loc:Location.t -> string -> sub:(Location.t * string) list -> location_error
  val print_error : Format.formatter -> location_error -> unit
  val raise_error : location_error -> 'a
end

module Helpers_impl = struct
  type nonrec location_error = location_error
  let error_of_exn = error_of_exn
  let register_error_of_exn = register_error_of_exn
  let report_exception = report_exception
  let get_error_message = get_error_message
  let set_error_message = set_error_message
  let make_error_of_message = make_error_of_message
  let print_error = print_error
  let raise_error err = raise (Location.Error err)
end
