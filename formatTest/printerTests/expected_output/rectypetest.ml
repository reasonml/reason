module Ppx_deriving_runtime =
  struct
    type nonrec int = int
    type nonrec char = char
    type nonrec string = string
    type nonrec float = float
    type nonrec bool = bool
    type nonrec unit = unit
    type nonrec exn = exn
    type nonrec 'a array = 'a array
    type nonrec 'a list = 'a list
    type nonrec 'a option = 'a option
    type nonrec nativeint = nativeint
    type nonrec int32 = int32
    type nonrec int64 = int64
    type nonrec 'a lazy_t = 'a lazy_t
    type nonrec bytes = bytes
    module Pervasives = Pervasives
    module Char = Char
    module String = String
    module Printexc = Printexc
    module Array = Array
    module List = List
    module Nativeint = Nativeint
    module Int32 = Int32
    module Int64 = Int64
    module Lazy = Lazy
    module Bytes = Bytes
    module Hashtbl = Hashtbl
    module Queue = Queue
    module Stack = Stack
    module Set = Set
    module Weak = Weak
    module Printf = Printf
    module Format = Format
    module Buffer = Buffer
    include Pervasives
  end
type mytype =
  | Int of hello 
  | Float of int list 
  | String 
and hello = string
let rec pp_mytype : Format.formatter -> mytype -> Ppx_deriving_runtime.unit =
  let __0 () = pp_hello  in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Int a0 ->
            (Format.fprintf fmt "(@[<2>Int@ ";
             ((__0 ()) fmt) a0;
             Format.fprintf fmt "@])")
        | Float a0 ->
            (Format.fprintf fmt "(@[<2>Float@ ";
             ((fun x  ->
                 Format.fprintf fmt "@[<2>[";
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ";@ ";
                           (Format.fprintf fmt "%d") x;
                           true) false x);
                 Format.fprintf fmt "@,]@]")) a0;
             Format.fprintf fmt "@])")
        | String  -> Format.pp_print_string fmt "String")
    [@ocaml.warning "-A"])

and show_mytype : mytype -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_mytype x

and (pp_hello : Format.formatter -> hello -> Ppx_deriving_runtime.unit) =
  ((let open! Ppx_deriving_runtime in fun fmt  -> Format.fprintf fmt "%S")
  [@ocaml.warning "-A"])

and show_hello : hello -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_hello x

let () = print_endline (show_mytype ((Int ("five"))[@explicit_arity ])) 
;;print_endline (show_hello "aloha")