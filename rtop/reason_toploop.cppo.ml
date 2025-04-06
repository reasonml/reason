(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

let default_parse_toplevel_phrase = !Toploop.parse_toplevel_phrase
let reason_parse_toplevel_phrase =
  Reason_util.correctly_catch_parse_errors
  (fun x ->
    let r =
      Reason_toolchain.To_current.copy_toplevel_phrase
        (Reason_toolchain.RE.toplevel_phrase x)
  in
#if OCAML_VERSION >= (5,2,0)
  (* NOTE(anmonteiro): after https://github.com/ocaml/ocaml/pull/12029, we get a

  Fatal error: exception Invalid_argument("index out of bounds")
  Raised by primitive operation at Toploop.ends_with_lf in file "toplevel/toploop.ml"

  Setting `lex_eof_reached` seems to avoid whatever check upstream is doing. *)
  x.lex_eof_reached <- true;
#endif
  r)


(* this file's triggered by utop/rtop *)
let main () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Reason is incompatible with camlp4!"
  else begin
#if OCAML_VERSION >= (5,3,0)
    if not (Toploop.prepare Format.err_formatter ()) then raise (Compenv.Exit_with_status 2);
#endif
    Toploop.parse_toplevel_phrase := (fun t ->
      if !Reason_utop.current_top = UTop then
        default_parse_toplevel_phrase t
      else
        reason_parse_toplevel_phrase t);
    Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors
        (fun x -> List.map Reason_toolchain.To_current.copy_toplevel_phrase
            (Reason_toolchain.RE.use_file x));
     (* Toploop.print_out_sig_item := M17n_util.utf8_print_out_sig_item !Toploop.print_out_sig_item; *)
    (* Toploop.install_printer Predef.path_string Predef.type_string *)
    (*   (fun fmt obj -> M17n_util.utf8_print_string fmt (Obj.magic obj)); *)
  end
