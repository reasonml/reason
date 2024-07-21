(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cmap = Map.Make (Char)                           (* character maps. *)

type 'a value =                         (* type for holding a bound value. *)
| Pre of 'a                    (* value is bound by the prefix of a key. *)
| Key of 'a                          (* value is bound by an entire key. *)
| Amb                     (* no value bound because of ambiguous prefix. *)
| Nil                            (* not bound (only for the empty trie). *)

type 'a t = { v : 'a value; succs : 'a t Cmap.t }
let empty = { v = Nil; succs = Cmap.empty }
let is_empty t = t = empty

(* N.B. If we replace a non-ambiguous key, it becomes ambiguous but it's
   not important for our use. Also the following is not tail recursive but
   the stack is bounded by key length. *)
let add t k d =
  let rec loop t k len i d pre_d = match i = len with
  | true ->
      let t' = { v = Key d; succs = t.succs } in
      begin match t.v with
      | Key old -> `Replaced (old, t')
      | _ -> `New t'
      end
  | false ->
      let v = match t.v with
      | Amb | Pre _ -> Amb | Key _ as v -> v | Nil -> pre_d
      in
      let t' = try Cmap.find k.[i] t.succs with Not_found -> empty in
      match loop t' k len (i + 1) d pre_d with
      | `New n -> `New { v; succs = Cmap.add k.[i] n t.succs }
      | `Replaced (o, n) ->
          `Replaced (o, { v; succs = Cmap.add k.[i] n t.succs })
  in
  loop t k (String.length k) 0 d (Pre d (* allocate less *))

let find_node t k =
  let rec aux t k len i =
    if i = len then t else
    aux (Cmap.find k.[i] t.succs) k len (i + 1)
  in
  aux t k (String.length k) 0

let find t k =
  try match (find_node t k).v with
  | Key v | Pre v -> `Ok v | Amb -> `Ambiguous | Nil -> `Not_found
  with Not_found -> `Not_found

let ambiguities t p =                        (* ambiguities of [p] in [t]. *)
  try
    let t = find_node t p in
    match t.v with
    | Key _ | Pre _ | Nil -> []
    | Amb ->
        let add_char s c = s ^ (String.make 1 c) in
        let rem_char s = String.sub s 0 ((String.length s) - 1) in
        let to_list m = Cmap.fold (fun k t acc -> (k,t) :: acc) m [] in
        let rec aux acc p = function
        | ((c, t) :: succs) :: rest ->
            let p' = add_char p c in
            let acc' = match t.v with
            | Pre _ | Amb -> acc
            | Key _ -> (p' :: acc)
            | Nil -> assert false
            in
            aux acc' p' ((to_list t.succs) :: succs :: rest)
        | [] :: [] -> acc
        | [] :: rest -> aux acc (rem_char p) rest
        | [] -> assert false
        in
        aux [] p (to_list t.succs :: [])
  with Not_found -> []

let of_list l =
  let add t (s, v) = match add t s v with `New t -> t | `Replaced (_, t) -> t in
  List.fold_left add empty l
