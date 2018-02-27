open Extend_protocol.Reader

let () =
  Reason_config.recoverable := true

module Reason_reader = struct
  open Migrate_parsetree

  type t = buffer

  let load buffer = buffer

  let structure str =
    Structure (Reason_toolchain.To_current.copy_structure str)

  let signature sg =
    Signature (Reason_toolchain.To_current.copy_signature sg)

  let parse {text; path} =
    let l = String.length path in
    let buf = Lexing.from_string text in
    Location.init buf (Filename.basename path);
    if l > 0 && path.[l - 1] = 'i' then
      signature (Reason_toolchain.RE.interface buf)
    else
      structure (Reason_toolchain.RE.implementation buf)

  let for_completion t pos =
    let pos' = !Reason_toolchain.insert_completion_ident in
    Reason_toolchain.insert_completion_ident := Some pos;
    Misc.try_finally
      (fun () -> ({complete_labels = true}, parse t))
      (fun () -> Reason_toolchain.insert_completion_ident := pos')

  let parse_line t pos line =
    let buf = Lexing.from_string line in
    structure (Reason_toolchain.RE.implementation buf)

  let ident_at t _ = []

  let formatter =
     let fmt = lazy (Reason_pprint_ast.createFormatter ()) in
     fun () -> Lazy.force fmt

  let pretty_print ppf =
    let open Reason_toolchain in
    let open Reason_pprint_ast in
    function
    | Pretty_core_type x ->
      (formatter ())#core_type ppf (From_current.copy_core_type x)
    | Pretty_case_list x ->
      (formatter ())#case_list ppf (List.map From_current.copy_case x)
    | Pretty_expression x ->
      (formatter ())#expression ppf (From_current.copy_expression x)
    | Pretty_pattern x ->
      (formatter ())#pattern ppf (From_current.copy_pattern x)
    | Pretty_signature x ->
      (formatter ())#signature [] ppf (From_current.copy_signature x)
    | Pretty_structure x ->
      (formatter ())#structure [] ppf (From_current.copy_structure x)
    | Pretty_toplevel_phrase x ->
      (formatter ())#toplevel_phrase ppf (From_current.copy_toplevel_phrase x)

  let print_outcome ppf =
    let open Reason_toolchain in
    let open Reason_oprint in
    function
    | Out_value x ->
      print_out_value ppf (From_current.copy_out_value x)
    | Out_type x ->
      print_out_type ppf (From_current.copy_out_type x)
    | Out_class_type x ->
      print_out_class_type ppf (From_current.copy_out_class_type x)
    | Out_module_type x ->
      print_out_module_type ppf (From_current.copy_out_module_type x)
    | Out_sig_item x ->
      print_out_sig_item ppf (From_current.copy_out_sig_item x)
    | Out_signature x ->
      print_out_signature ppf (List.map From_current.copy_out_sig_item x)
    | Out_type_extension x ->
      print_out_type_extension ppf (From_current.copy_out_type_extension x)
    | Out_phrase x ->
      print_out_phrase ppf (From_current.copy_out_phrase x)
end



let () =
  let open Extend_main in
  extension_main
    ~reader:(Reader.make_v0 (module Reason_reader : V0))
    (Description.make_v0 ~name:"reason" ~version:"0.1")
