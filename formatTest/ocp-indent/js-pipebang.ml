let f x =
  x
  >>| fun x ->
  g x
  >>| fun x ->
  h x
;;

let f x =
  x >>| fun x ->
  g x >>| fun x ->
  h x
;;

let f x =
  x
  |! fun x ->
  g x
  |! fun x ->
  h x
;;

let f x =
  x |! fun x ->
  g x |! fun x ->
  h x
;;

let _ =
  (z (fun x -> x)
   |! Validate.of_list)			(* Tuareg indents this line too far. *)

let _ =
  (* Tuareg works correctly on this (if you drop the fun). *)
  (z x
   |! Validate.of_list)

(* jli found this great one.  Tuareg gets confused by the paren before List.map and
   indents |! way too far, under "k ^".  ocp-indent should shine, since it understands the
   syntax better. *)
let _ =
  List.filter_opt [
    format old (fun old -> "removed: "
                           ^ (List.map old ~f:(fun (k, v) -> k ^ "=" ^ acl_to_string v)
                              |! String.concat ~sep:", "))
  ]



(* (|>) = (|!) *)

let f x =
  x
  |> fun x ->
  g x
  |> fun x ->
  h x
;;

let f x =
  x |> fun x ->
  g x |> fun x ->
  h x
;;

let _ =
  (z (fun x -> x)
   |> Validate.of_list)			(* Tuareg indents this line too far. *)

let _ =
  (* Tuareg works correctly on this (if you drop the fun). *)
  (z x
   |> Validate.of_list)

(* jli found this great one.  Tuareg gets confused by the paren before List.map and
   indents |> way too far, under "k ^".  ocp-indent should shine, since it understands the
   syntax better. *)
let _ =
  List.filter_opt [
    format old (fun old -> "removed: "
                           ^ (List.map old ~f:(fun (k, v) -> k ^ "=" ^ acl_to_string v)
                              |> String.concat ~sep:", "))
  ]
