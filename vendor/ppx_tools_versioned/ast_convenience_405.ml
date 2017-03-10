open Ast_405

(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

open Parsetree
open Asttypes
open Location
open Ast_helper


module Label = struct

  type t = Asttypes.arg_label

  type desc = Asttypes.arg_label =
      Nolabel
    | Labelled of string
    | Optional of string

  let explode x = x

  let nolabel = Nolabel
  let labelled x = Labelled x
  let optional x = Optional x

end

module Constant = struct
  type t = Parsetree.constant =
     Pconst_integer of string * char option
   | Pconst_char of char
   | Pconst_string of string * string option
   | Pconst_float of string * char option

  let of_constant x = x

  let to_constant x = x

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
let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Pconst_string (s, None))
let int ?loc ?attrs x = Exp.constant ?loc ?attrs (Pconst_integer (string_of_int x, None))
let int32 ?loc ?attrs x = Exp.constant ?loc ?attrs (Pconst_integer (Int32.to_string x, Some 'l'))
let int64 ?loc ?attrs x = Exp.constant ?loc ?attrs (Pconst_integer (Int64.to_string x, Some 'L'))
let char ?loc ?attrs x = Exp.constant ?loc ?attrs (Pconst_char x)
let float ?loc ?attrs x = Exp.constant ?loc ?attrs (Pconst_float (string_of_float x, None))
let record ?loc ?attrs ?over l =
  Exp.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.pexp_loc s, e)) l) over
let func ?loc ?attrs l = Exp.function_ ?loc ?attrs (List.map (fun (p, e) -> Exp.case p e) l)
let lam ?loc ?attrs ?(label = Label.nolabel) ?default pat exp = Exp.fun_ ?loc ?attrs label default pat exp
let app ?loc ?attrs f l = if l = [] then f else Exp.apply ?loc ?attrs f (List.map (fun a -> Label.nolabel, a) l)
let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lid ?loc s)
let let_in ?loc ?attrs ?(recursive = false) b body =
  Exp.let_ ?loc ?attrs (if recursive then Recursive else Nonrecursive) b body

let sequence ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | hd :: tl -> List.fold_left (fun e1 e2 -> Exp.sequence ?loc ?attrs e1 e2) hd tl

let pvar ?(loc = !default_loc) ?attrs s = Pat.var ~loc ?attrs (mkloc s loc)
let pconstr ?loc ?attrs s args = Pat.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Pat.tuple args)
let precord ?loc ?attrs ?(closed = Open) l =
  Pat.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.ppat_loc s, e)) l) closed
let pnil ?loc ?attrs () = pconstr ?loc ?attrs "[]" []
let pcons ?loc ?attrs hd tl = pconstr ?loc ?attrs "::" [hd; tl]
let punit ?loc ?attrs () = pconstr ?loc ?attrs "()" []
let ptuple ?loc ?attrs = function
  | [] -> punit ?loc ?attrs ()
  | [x] -> x
  | xs -> Pat.tuple ?loc ?attrs xs
let plist ?loc ?attrs l = List.fold_right (pcons ?loc ?attrs) l (pnil ?loc ?attrs ())

let pstr ?loc ?attrs s = Pat.constant ?loc ?attrs (Pconst_string (s, None))
let pint ?loc ?attrs x = Pat.constant ?loc ?attrs (Pconst_integer (string_of_int x, None))
let pchar ?loc ?attrs x = Pat.constant ?loc ?attrs (Pconst_char x)
let pfloat ?loc ?attrs x = Pat.constant ?loc ?attrs (Pconst_float (string_of_float x, None))

let tconstr ?loc ?attrs c l = Typ.constr ?loc ?attrs (lid ?loc c) l

let get_str = function
  | {pexp_desc=Pexp_constant (Pconst_string (s, _)); _} -> Some s
  | _ -> None

let get_str_with_quotation_delimiter = function
  | {pexp_desc=Pexp_constant (Pconst_string (s, d)); _} -> Some (s, d)
  | _ -> None

let get_lid = function
  | {pexp_desc=Pexp_ident{txt=id;_};_} ->
      Some (String.concat "." (Longident.flatten id))
  | _ -> None

let find_attr s attrs =
  try Some (snd (List.find (fun (x, _) -> x.txt = s) attrs))
  with Not_found -> None

let expr_of_payload = function
  | PStr [{pstr_desc=Pstr_eval(e, _); _}] -> Some e
  | _ -> None

let find_attr_expr s attrs =
  match find_attr s attrs with
  | Some e -> expr_of_payload e
  | None -> None

let has_attr s attrs =
  find_attr s attrs <> None
