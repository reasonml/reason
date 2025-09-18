open Reason

let () = Reason_config.recoverable := true

module Reason_reader = struct
  open Extend_protocol.Reader

  type t = buffer

  let load buffer = buffer

  let structure str =
    let str =
      str
      |> Reason_syntax_util.(
           apply_mapper_to_structure remove_stylistic_attrs_mapper)
      |> Reason_syntax_util.(apply_mapper_to_structure backport_letopt_mapper)
    in
    Structure (Reason_toolchain.To_current.copy_structure str)

  let signature sg =
    let sg =
      let open Reason_syntax_util in
      sg
      |> apply_mapper_to_signature remove_stylistic_attrs_mapper
      |> apply_mapper_to_signature backport_letopt_mapper
    in
    Signature (Reason_toolchain.To_current.copy_signature sg)

  let parse { text; path; _ } =
    let buf = Lexing.from_string text in
    Location.init buf (Filename.basename path);
    let l = String.length path in
    if l > 0 && String.unsafe_get path (l - 1) = 'i'
    then signature (Reason_toolchain.RE.interface buf)
    else structure (Reason_toolchain.RE.implementation buf)

  let for_completion t pos =
    let pos' = !Reason_toolchain_conf.insert_completion_ident in
    Reason_toolchain_conf.insert_completion_ident := Some pos;
    Misc.try_finally
      (fun () -> { complete_labels = true }, parse t)
      ~always:(fun () -> Reason_toolchain_conf.insert_completion_ident := pos')

  let parse_line _ _ line =
    let buf = Lexing.from_string line in
    structure (Reason_toolchain.RE.implementation buf)

  let ident_at _ _ = []

  let formatter =
    let fmt = lazy (Reason_pprint_ast.createFormatter ()) in
    fun () -> Lazy.force fmt

  let pretty_print =
    let module From_current = Reason_toolchain.From_current in
    fun ppf ppt ->
      let fmt = formatter () in
      match ppt with
      | Pretty_core_type x -> fmt#core_type ppf (From_current.copy_core_type x)
      | Pretty_case_list x ->
        fmt#case_list ppf (List.map ~f:From_current.copy_case x)
      | Pretty_expression x ->
        fmt#expression ppf (From_current.copy_expression x)
      | Pretty_pattern x -> fmt#pattern ppf (From_current.copy_pattern x)
      | Pretty_signature x ->
        fmt#signature [] ppf (From_current.copy_signature x)
      | Pretty_structure x ->
        fmt#structure [] ppf (From_current.copy_structure x)
      | Pretty_toplevel_phrase x ->
        fmt#toplevel_phrase ppf (From_current.copy_toplevel_phrase x)

  let print_outcome =
    let module From_current = Reason_toolchain.From_current in
    fun ppf otree ->
      match otree with
      | Out_value x ->
        Reason_oprint.print_out_value ppf (From_current.copy_out_value x)
      | Out_type x ->
        Reason_oprint.print_out_type ppf (From_current.copy_out_type x)
      | Out_class_type x ->
        Reason_oprint.print_out_class_type
          ppf
          (From_current.copy_out_class_type x)
      | Out_module_type x ->
        Reason_oprint.print_out_module_type
          ppf
          (From_current.copy_out_module_type x)
      | Out_sig_item x ->
        Reason_oprint.print_out_sig_item ppf (From_current.copy_out_sig_item x)
      | Out_signature x ->
        Reason_oprint.print_out_signature
          ppf
          (List.map ~f:From_current.copy_out_sig_item x)
      | Out_type_extension x ->
        Reason_oprint.print_out_type_extension
          ppf
          (From_current.copy_out_type_extension x)
      | Out_phrase x ->
        Reason_oprint.print_out_phrase ppf (From_current.copy_out_phrase x)
end

let () =
  if Sys.win32
  then (
    set_binary_mode_in stdin true;
    set_binary_mode_out stdout true);
  Extend_main.extension_main
    ~reader:
      (Extend_main.Reader.make_v0
         (module Reason_reader : Extend_protocol.Reader.V0))
    (Extend_main.Description.make_v0 ~name:"reason" ~version:"0.1")
