(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* this file's triggered by utop/rtop *)
let () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Reason is incompatible with camlp4!"
  else begin
    Toploop.parse_toplevel_phrase := Reason_util.correctly_catch_parse_errors
        (fun x -> Reason_toolchain.To_current.copy_toplevel_phrase
            (Reason_toolchain.RE.toplevel_phrase x));
    Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors
        (fun x -> List.map Reason_toolchain.To_current.copy_toplevel_phrase
            (Reason_toolchain.RE.use_file x));
     (* Toploop.print_out_sig_item := M17n_util.utf8_print_out_sig_item !Toploop.print_out_sig_item; *)
    (* Toploop.install_printer Predef.path_string Predef.type_string *)
    (*   (fun fmt obj -> M17n_util.utf8_print_string fmt (Obj.magic obj)); *)
  end
