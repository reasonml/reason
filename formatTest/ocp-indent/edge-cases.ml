
(* this could be fixed, but we actually want to handle the first case
   differently for when there is only one case (see next examples) *)
let f x = function A -> x;
  2
                 | B -> y;
                   3

(* if we were to fix to the case above, the second >>= would be below the _
   (test taken from js-fun) *)
let _ =
  x
  >>= fun x ->
  try x with _ -> ()
    >>= fun x ->
    x

(* (and also: the some_handling here would be below Not_found) *)
let _ =
  try
    ()
  with Not_found ->
    some_handling

let f = fun x ->
  x

let f = (fun x ->
    x
  )

let f g = g @@ fun x ->
  x

let f g = g @@ (fun x ->
    x
  )


(* the above should probably be consistent with: *)
let f x y = y + match x with A ->
    0

let f x y = y + (match x with A ->
    0
  )

(* wich means we may over-indent even when the block is non-closable *)

let f x y = y + match x with
  | A -> 0

let f x y = y + (match x with
    | A -> 0
  )

let f x y = y + match x with
  | A -> 0

let _ =
  somefun
    (fun x ->
       x);
  somefun
    (if
      bla
     then
       bli);
  somefun
    (if bla then
       bli
     else
       blu)

let _ =
  a
  ;
  b

(* Surprisingly, this is the indentation correpsonding to OCaml's interpretation
   of this code.  Indenting this accordingly may help users notice that they're
   doing something dubious. *)
(* let b = `b *)
(* let d = `d *)
(* ;; *)
(* let a = b *)
(* function (_ : [ `c ]) -> d *)
(* ;; *)
