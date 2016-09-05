open Extend_protocol.Reader

let () =
  Reason_config.recoverable := true

module Reason_reader = struct
  type t = buffer

  let load buffer = buffer

  let parse {text; path} =
    let l = String.length path in
    let buf = Lexing.from_string text in
    Location.init buf (Filename.basename path);
    if l > 0 && path.[l - 1] = 'i' then
      Signature (Reason_toolchain.JS.canonical_interface buf)
    else
      Structure (Reason_toolchain.JS.canonical_implementation buf)

  let for_completion t _ =
    ({complete_labels = true}, parse t)

  let parse_line t pos line =
    let buf = Lexing.from_string line in
    Structure (Reason_toolchain.JS.canonical_implementation buf)

  let ident_at t _ = []

  let formatter =
     let fmt = lazy (Reason_pprint_ast.createFormatter ()) in
     fun () -> Lazy.force fmt

  let pretty_print ppf =
    let open Reason_pprint_ast in function
    | Pretty_core_type       x -> (formatter ())#core_type       ppf x
    | Pretty_case_list       x -> (formatter ())#case_list       ppf x
    | Pretty_expression      x -> (formatter ())#expression      ppf x
    | Pretty_pattern         x -> (formatter ())#pattern         ppf x
    | Pretty_signature       x -> (formatter ())#signature    [] ppf x
    | Pretty_structure       x -> (formatter ())#structure    [] ppf x
    | Pretty_toplevel_phrase x -> (formatter ())#toplevel_phrase ppf x

  let print_outcome ppf =
    let open Reason_oprint in function
    | Out_value          x -> print_out_value          ppf x
    | Out_type           x -> print_out_type           ppf x
    | Out_class_type     x -> print_out_class_type     ppf x
    | Out_module_type    x -> print_out_module_type    ppf x
    | Out_sig_item       x -> print_out_sig_item       ppf x
    | Out_signature      x -> print_out_signature      ppf x
    | Out_type_extension x -> print_out_type_extension ppf x
    | Out_phrase         x -> print_out_phrase         ppf x
end



let () =
  let open Extend_main in
  extension_main
    ~reader:(Reader.make_v0 (module Reason_reader : V0))
    (Description.make_v0 ~name:"reason" ~version:"0.1")
