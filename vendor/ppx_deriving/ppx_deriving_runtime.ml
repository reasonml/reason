type nonrec int = int
type nonrec char = char
type nonrec string = string
type nonrec float = float
type nonrec bool = bool
type nonrec unit = unit
type nonrec exn = exn
type nonrec 'a array = 'a array
type nonrec 'a list = 'a list
type nonrec 'a option = 'a option = None | Some of 'a
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
module Map = Map
module Weak = Weak

module Printf = Printf
module Format = Format
module Buffer = Buffer

include Pervasives
