(* Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved. *)


let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Reason is incompatible with camlp4!"
  else begin
    Toploop.parse_toplevel_phrase := Reason_util.correctly_catch_parse_errors Reason_parser.toplevel_phrase;
    Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors Reason_parser.use_file;
    (* Toploop.print_out_sig_item := M17n_util.utf8_print_out_sig_item !Toploop.print_out_sig_item; *)
    (* Toploop.install_printer Predef.path_string Predef.type_string *)
    (*   (fun fmt obj -> M17n_util.utf8_print_string fmt (Obj.magic obj)); *)
    prerr_endline "Reason: Meta Language Utility";
  end
