let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Reason is incompatible with camlp4!"
  else begin
    UTop.parse_toplevel_phrase := UTop.parse_default (
      Reason_util.correctly_catch_parse_errors Reason_parser.toplevel_phrase);
    UTop.parse_use_file := UTop.parse_default (
      Reason_util.correctly_catch_parse_errors  Reason_parser.use_file);
    (* Toploop.print_out_sig_item := M17n_util.utf8_print_out_sig_item !Toploop.print_out_sig_item; *)
    (* Toploop.install_printer Predef.path_string Predef.type_string *)
    (*   (fun fmt obj -> M17n_util.utf8_print_string fmt (Obj.magic obj)); *)
    (* As of 1.16, utop does not actually invoke UTop.parse_use_file. *)
    Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors Reason_parser.use_file;
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
