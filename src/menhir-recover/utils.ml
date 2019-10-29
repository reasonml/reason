let const c = fun _ -> c

let group_assoc l =
  let cons k v acc = (k, List.rev v) :: acc in
  let rec aux k v vs acc = function
    | [] -> List.rev (cons k (v :: vs) acc)
    | (k', v') :: xs when compare k k' = 0 ->
      if compare v v' = 0 then
        aux k v vs acc xs
      else
        aux k v' (v :: vs) acc xs
    | (k', v') :: xs ->
      aux k' v' [] (cons k (v :: vs) acc) xs
  in
  match List.sort compare l with
  | [] -> []
  | (k, v) :: xs -> aux k v [] [] xs

let rec list_last = function
  | [x] -> x
  | _ :: xs -> list_last xs
  | [] -> invalid_arg "list_last"

let pp_list f ppf = function
  | [] -> Format.fprintf ppf "[]"
  | x :: xs ->
     Format.fprintf ppf "[%a" f x;
     List.iter (Format.fprintf ppf "; %a" f) xs;
     Format.fprintf ppf "]"

let rec list_filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> list_filter_map f xs
    | Some x' -> x' :: list_filter_map f xs

module Cost : sig
  type t = private int
  val zero : t
  val infinite : t
  val compare : t -> t -> int
  val add : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val is_infinite : t -> bool
  val arg_min : ('a -> t) -> 'a -> 'a -> 'a
end = struct
  type t = int
  let zero = 0
  let infinite = max_int
  let compare : t -> t -> int = compare
  let add t1 t2 =
    let result = t1 + t2 in
    if result < 0 then infinite
    else result
  let of_int x =
    if x < 0
    then invalid_arg "Cost.of_int: cost must be positive"
    else x
  let to_int x = x
  let is_infinite x = x = infinite
  let arg_min f a b =
    if compare (f a) (f b) <= 0 then a else b

end
