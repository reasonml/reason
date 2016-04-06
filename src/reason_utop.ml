(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)

let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Reason is incompatible with camlp4!"
  else begin
    UTop.prompt := fst (React.S.create LTerm_text.(eval [B_fg (LTerm_style.green); S "# "]));
    UTop.parse_toplevel_phrase := UTop.parse_default (
      Reason_util.correctly_catch_parse_errors
      Reason_toolchain.JS.canonical_toplevel_phrase
    );
    UTop.parse_use_file := UTop.parse_default (
      Reason_util.correctly_catch_parse_errors
      Reason_toolchain.JS.canonical_use_file
    );
    Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors
                                Reason_toolchain.JS.canonical_use_file;
    (* Printing in Reason syntax *)
    Toploop.print_out_value := Reason_oprint.print_out_value;
    Toploop.print_out_type := Reason_oprint.print_out_type;
    Toploop.print_out_class_type := Reason_oprint.print_out_class_type;
    Toploop.print_out_module_type := Reason_oprint.print_out_module_type;
    Toploop.print_out_type_extension := Reason_oprint.print_out_type_extension;
    Toploop.print_out_sig_item := Reason_oprint.print_out_sig_item;
    Toploop.print_out_signature := Reason_oprint.print_out_signature;
    Toploop.print_out_phrase := Reason_oprint.print_out_phrase;
  end

let _ = UTop.set_phrase_terminator ";"

(* With the Reason utop extension, single semicolons are sufficient for
   submitting to the top level *)
(* let toggle_semi = *)
(*   let original = !UTop.parse_toplevel_phrase in *)
(*   let no_semi str eos_is_error = *)
(*     let len = String.length str in *)
(*     if len > 1 && str.[len - 1] != ';' then *)
(*       original (str) eos_is_error *)
(*     else if len > 0 && str.[len - 1] = ';' then *)
(*       original (str ^ "; ") eos_is_error *)
(*     else  *)
(*       original str eos_is_error *)
(*   in *)
(*   let semi = ref true in *)
(*   fun () -> *)
(*     UTop.parse_toplevel_phrase := if !semi then no_semi else original; *)
(*     semi := not !semi *)
(* ;; *)
(* (* Include this line to not require ;; by default *) *)
(* toggle_semi ();; *)
