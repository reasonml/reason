(**
 * Some of this was coppied from @whitequark's m17n project.
 *)
(*
 * Portions Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let transmogrify_exn exn template =
  assert (Obj.tag (Obj.repr exn) = 0);
  Obj.set_field (Obj.repr exn) 0 (Obj.field (Obj.repr template) 0);
  exn

let extract_exn src name =
  try
    ignore (!Toploop.parse_toplevel_phrase (Lexing.from_string src));
    assert false
  with exn ->
    assert (Printexc.exn_slot_name exn = name);
    exn

let exn_Lexer_Error = extract_exn "\128" "Lexer.Error"
let exn_Syntaxerr_Error = extract_exn "fun" "Syntaxerr.Error"

let correctly_catch_parse_errors fn lexbuf =
  (*let kind = if !Toploop.input_name = "//toplevel//" then `Toplevel else `Batch in*)
  fn lexbuf
       (*with exn when kind = `Toplevel ->
    (* In expunged toplevel, we have a split-brain situation where toplevel
       and m17n have different internal IDs for the "same" exceptions.
       Fixup. *)
    raise
     (match exn with
          (* FIXME... Maybe? *)
          (*| Reason_lexer.Error _ -> transmogrify_exn exn exn_Lexer_Error*)
          | Syntaxerr.Error _ -> transmogrify_exn exn exn_Syntaxerr_Error
          | Reason_syntax_util.Error (loc, _) -> transmogrify_exn (Syntaxerr.Error(Syntaxerr.Other loc)) exn_Syntaxerr_Error
          | _ -> exn)
        *)
