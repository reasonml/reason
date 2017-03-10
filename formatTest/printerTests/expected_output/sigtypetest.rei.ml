module Ppx_deriving_runtime :
sig
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
  module Format :
  (module type of Format with type  formatter_out_functions := 
    Format.formatter_out_functions and type  formatter_tag_functions := 
    Format.formatter_tag_functions and type  formatter :=  Format.formatter)
end
type mytype
val show_mytype : mytype -> Ppx_deriving_runtime.string
val pp_mytype : Format.formatter -> mytype -> Ppx_deriving_runtime.unit