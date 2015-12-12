type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n    -> n                 (* a = int *)
  | Add      -> (fun x y -> x+y)  (* a = int -> int -> int *)
  | App(f,x) -> (eval f) (eval x)
(* eval called at types (b->a) and b for fresh b *)

let two = eval (App (App (Add, Int 1), Int 1))

let rec sum : type a. a term -> _ = fun x ->
  let y =
    match x with
    | Int n -> n
    | Add   -> 0
    | App(f,x) -> sum f + sum x
  in y + 1

type _ typ =
  | Int : int typ
  | String : string typ
  | Pair : 'a typ * 'b typ -> ('a * 'b) typ

let rec to_string: type t. t typ -> t -> string =
  fun t x ->
    match t with
    | Int -> string_of_int x
    | String -> Printf.sprintf "%S" x
    | Pair(t1,t2) ->
      let (x1, x2) = x in
      Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)

type (_,_) eq = Eq : ('a,'a) eq

let cast : type a b. (a,b) eq -> a -> b = fun Eq x -> x

let rec eq_type : type a b. a typ -> b typ -> (a,b) eq option =
  fun a b ->
    match a, b with
    | Int, Int -> Some Eq
    | String, String -> Some Eq
    | Pair(a1,a2), Pair(b1,b2) ->
      begin match eq_type a1 b1, eq_type a2 b2 with
        | Some Eq, Some Eq -> Some Eq
        | _ -> None
      end
    | _ -> None

type dyn = Dyn : 'a typ * 'a -> dyn

let get_dyn : type a. a typ -> dyn -> a option =
  fun a (Dyn(b,x)) ->
    match eq_type a b with
    | None -> None
    | Some Eq -> Some x

let _ =
  let f: type a. a list -> int =
    fun _x -> 42
  in
  f []

let nth t n =
  if n < 0 then None else
    let rec nth_aux: type b. ('a, b) t -> int -> 'a option = fun t n ->
      match t with
      | Empty -> None
      | Node (a, t) -> if n = 0 then Some a else nth_aux t (n-1)
    in
    nth_aux t n

let rec f : type a b. a = function
  | _ -> assert false
and g : type a. a = function
  | _ -> assert false
