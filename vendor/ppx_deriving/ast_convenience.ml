(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(*  It has been trimmed down and adapted to fit the needs of     *)
(*  Facebook by Max Bernstein. Sort of like that little notice   *)
(*  that appears before some films on airplanes.                 *)


open Parsetree
open Asttypes
open Location
open Ast_helper


module Label = struct
  let nolabel = ""
end

let may_tuple ?loc tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc ?attrs:None l)

let lid ?(loc = !default_loc) s = mkloc (Longident.parse s) loc
let constr ?loc ?attrs s args = Exp.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Exp.tuple args)
let nil ?loc ?attrs () = constr ?loc ?attrs "[]" []
let unit ?loc ?attrs () = constr ?loc ?attrs "()" []
let tuple ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | [x] -> x
  | xs -> Exp.tuple ?loc ?attrs xs
let cons ?loc ?attrs hd tl = constr ?loc ?attrs "::" [hd; tl]
let list ?loc ?attrs l = List.fold_right (cons ?loc ?attrs) l (nil ?loc ?attrs ())
let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const_string (s, None))
let int ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let int32 ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let int64 ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let char ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_char x)
let float ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_float x)
