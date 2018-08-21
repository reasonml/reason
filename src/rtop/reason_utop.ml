(**
 * Some of this was coppied from @whitequark's m17n project.
 *)
(*
 * Portions Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ToploopBackup = struct
  let print_out_value = !Toploop.print_out_value
  let print_out_type = !Toploop.print_out_type
  let print_out_class_type = !Toploop.print_out_class_type
  let print_out_module_type = !Toploop.print_out_module_type
  let print_out_type_extension = !Toploop.print_out_type_extension
  let print_out_sig_item = !Toploop.print_out_sig_item
  let print_out_signature = !Toploop.print_out_signature
  let print_out_phrase = !Toploop.print_out_phrase
  let current_show = Hashtbl.find Toploop.directive_table "show"
end

let rec lident_operator_map mapper li =
  let open Longident in
  match li with
  | Lident s -> Lident (mapper s)
  | Ldot (x, s) -> Ldot (x, mapper s)
  | Lapply (x, y) -> Lapply (lident_operator_map mapper x, lident_operator_map mapper y)

type top_kind = RTop | UTop
let current_top = ref RTop

let init_reason () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Reason is incompatible with camlp4!"
  else begin
    let use_file x =
      List.map Reason_toolchain.To_current.copy_toplevel_phrase
        (Reason_toolchain.RE.use_file x)
    in
    current_top := RTop;
    UTop.set_phrase_terminator ";";
    UTop.prompt := fst (React.S.create LTerm_text.
                     (eval [B_fg (LTerm_style.green); S "Reason # "]));
    UTop.parse_toplevel_phrase := UTop.parse_default (
      Reason_util.correctly_catch_parse_errors
        (fun x -> Reason_toolchain.To_current.copy_toplevel_phrase
            (Reason_toolchain.RE.toplevel_phrase x))
    );
    UTop.parse_use_file := UTop.parse_default (
      Reason_util.correctly_catch_parse_errors use_file
    );
    UTop.history_file_name :=
      Some (Filename.concat LTerm_resources.home ".rtop-history");

    Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors use_file;

    (* Printing in Reason syntax *)
    let open Reason_toolchain.From_current in
    let wrap f g fmt x = g fmt (f x) in
    Toploop.print_out_value :=
      wrap copy_out_value Reason_oprint.print_out_value;
    Toploop.print_out_type :=
      wrap copy_out_type Reason_oprint.print_out_type;
    Toploop.print_out_class_type :=
      wrap copy_out_class_type Reason_oprint.print_out_class_type;
    Toploop.print_out_module_type :=
      wrap copy_out_module_type Reason_oprint.print_out_module_type;
    Toploop.print_out_type_extension :=
      wrap copy_out_type_extension Reason_oprint.print_out_type_extension;
    Toploop.print_out_sig_item :=
      wrap copy_out_sig_item Reason_oprint.print_out_sig_item;
    Toploop.print_out_signature :=
      wrap (List.map copy_out_sig_item) Reason_oprint.print_out_signature;
    Toploop.print_out_phrase :=
      wrap copy_out_phrase Reason_oprint.print_out_phrase;
    let current_show_fn = match ToploopBackup.current_show with
    | Toploop.Directive_ident fn -> fn
    | _ -> assert false
    in
    Hashtbl.replace Toploop.directive_table "show"
      (Toploop.Directive_ident (fun li ->
           let li' = lident_operator_map Reason_syntax_util.reason_to_ml_swap li in
           current_show_fn li'));
  end

let init_ocaml () =
  current_top := UTop;
  UTop.set_phrase_terminator ";;";
  UTop.prompt := fst (React.S.create LTerm_text.
                   (eval[B_fg (LTerm_style.green); S "OCaml # "]));
  UTop.parse_toplevel_phrase := UTop.parse_toplevel_phrase_default;
  UTop.parse_use_file := UTop.parse_use_file_default;
  UTop.history_file_name :=
      Some (Filename.concat LTerm_resources.home ".utop-history");

  Toploop.print_out_value := ToploopBackup.print_out_value;
  Toploop.print_out_type := ToploopBackup.print_out_type;
  Toploop.print_out_class_type := ToploopBackup.print_out_class_type;
  Toploop.print_out_module_type := ToploopBackup.print_out_module_type;
  Toploop.print_out_type_extension := ToploopBackup.print_out_type_extension;
  Toploop.print_out_sig_item := ToploopBackup.print_out_sig_item;
  Toploop.print_out_signature := ToploopBackup.print_out_signature;
  Toploop.print_out_phrase := ToploopBackup.print_out_phrase;
  Hashtbl.replace Toploop.directive_table "show" ToploopBackup.current_show

let toggle_syntax () =
  match !current_top with
  | RTop -> init_ocaml ()
  | UTop -> init_reason ()

let _ =
  Hashtbl.add Toploop.directive_table "toggle_syntax"
    (Toploop.Directive_none toggle_syntax);
  init_reason ()
