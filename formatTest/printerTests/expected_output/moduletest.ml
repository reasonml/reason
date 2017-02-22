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
module TestModule =
  struct
    type twostrings = (string* string)
    let rec (pp_twostrings :
              Format.formatter -> twostrings -> Ppx_deriving_runtime.unit)
      =
      ((let open! Ppx_deriving_runtime in
          fun fmt  ->
            fun (a0,a1)  ->
              Format.fprintf fmt "(@[";
              ((Format.fprintf fmt "%S") a0;
               Format.fprintf fmt ",@ ";
               (Format.fprintf fmt "%S") a1);
              Format.fprintf fmt "@])")[@ocaml.warning "-A"])
    and show_twostrings : twostrings -> Ppx_deriving_runtime.string=
      fun x  -> Format.asprintf "%a" pp_twostrings x
    let mkPair s = (s, s)
  end
let twoStrings = TestModule.mkPair "hello"
let () = print_endline (TestModule.show_twostrings twoStrings)