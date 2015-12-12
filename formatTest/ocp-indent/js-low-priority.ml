(* Relatively low priority Jane Street indentation bugs. *)



(* js-args *)

(* uncommon *)
let x =
  try x with a -> b
           | c -> d
let x =
  try x
  with a -> b
     | c -> d



(* js-comment *)

let mk_cont_parser cont_parse = (); fun _state str ~max_pos ~pos ->
  let len = max_pos - pos + 1 in
  cont_parse ~pos ~len str

(* sexp parser is sensitive to
   absent newlines at the end of files. *)



(* It would be nice if a partially completed ocamldoc code fragment inside a
   comment had the closing delimiter "]}" indented nicely before the comment is
   closed.  (This has to be the last comment in the file, to be partial.) *)
(* Maybe add:
   {[
     val state : t -> [ `Unstarted | `Running | `Stopped ]
   ]}
*)
