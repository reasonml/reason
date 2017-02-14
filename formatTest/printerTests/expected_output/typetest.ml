module Ppx_deriving_runtime =
  struct
    type nonrec int = int[@@nonrec ]
    type nonrec char = char[@@nonrec ]
    type nonrec string = string[@@nonrec ]
    type nonrec float = float[@@nonrec ]
    type nonrec bool = bool[@@nonrec ]
    type nonrec unit = unit[@@nonrec ]
    type nonrec exn = exn[@@nonrec ]
    type nonrec 'a array = 'a array[@@nonrec ]
    type nonrec 'a list = 'a list[@@nonrec ]
    type nonrec 'a option = 'a option[@@nonrec ]
    type nonrec nativeint = nativeint[@@nonrec ]
    type nonrec int32 = int32[@@nonrec ]
    type nonrec int64 = int64[@@nonrec ]
    type nonrec 'a lazy_t = 'a lazy_t[@@nonrec ]
    type nonrec bytes = bytes[@@nonrec ]
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
  | Int of string
  | Float of int list
  | String
let rec (pp_mytype : Format.formatter -> mytype -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Int a0 ->
            (Format.fprintf fmt "(@[<2>Int@ ";
             (Format.fprintf fmt "%S") a0;
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
        | String  -> Format.pp_print_string fmt "String")[@ocaml.warning
                                                           "-A"])
and show_mytype : mytype -> Ppx_deriving_runtime.string=
  fun x  -> Format.asprintf "%a" pp_mytype x
let () = print_endline (show_mytype ((Int ("five"))[@explicit_arity ]))