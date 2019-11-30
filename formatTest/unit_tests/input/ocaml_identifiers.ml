(* Type names (supported with PR#2342) *)
module T = struct
  type pub = unit
end

(* Value names (already supported) *)
module V = struct
  let pub = ()
end

(* Record fields *)
module R = struct
  type r = { pub : int }

  let foo = { pub = 4 }
end

(* Class names and instance variables *)
module C = struct
  class pub = object end

  class c = object
    val pub = 0
    method pub () = ()
  end

  class c' = object
    inherit pub
    val! pub = 1
  end
end

(* Class types *)
module Ct = struct
  class type pub = object
    val pub: unit -> unit
  end
end

(* Virtual *)
module Cv = struct
  class virtual pub = object end
end

(* Object methods *)
module O = struct
  let o = object method pub = () end
end

(* Function parameter labels *)
module L = struct
  let f ~pub = ignore pub
end

(* Module types *)
module type pub = sig
end

(* Polymorphic variants (probably ok as-is?) *)
module P = struct
  type t = [ `pub ]
end

type method_ = string

type foo = {method_: method_}
