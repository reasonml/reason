module M (S : S) =
  F.Make(struct
    module G = struct
      type t
      include (Foo : Foo with type t := t)
      include (Bar:Bar with type t := t)
    end
  end)

module M =
struct
  type t
end

module Update : sig
  val f : ('a, 'b) t -> 'a -> unit
  val g : ('a, 'b) t -> 'a -> unit
  module M : C with type k = t
  module G : C with type k := f
  type t
end = struct
  type t = int
end

module M : S
  with type t = x
   and type t' = y
   and type t' = y
=
struct
  type t = int
end

module M : S with type t = x
              and type t' = y
              and type t' = y
= struct
  type t = int
end

module Make: functor (M : T) -> sig
  val f : int -> int
  val g : int -> int
end = functor () -> struct end

module Store (K: API.KEY) (V: API.VALUE) :
  API.STORE with module K = K
             and module V = V =
struct
  end

  module K = K
