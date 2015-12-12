(** ocaml module types
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual018.html)
*)

module type T =
  M.T

module type T =
sig
end

module type T = sig
  ;;
end

module type T =
  functor (M : T) ->
  functor (M1 : T1) ->
  sig
  end

module type T =
sig end
with type 'a t = 'b
 and module M = M'.MF(X)
 and type t' = t''

module type T = (
sig
end
)

module type T =
sig
  val v :
    t

  external x : 'a =
    "stub"

  type t =
    int
  and t2 =
    t

  exception Error
    of int

  class virtual ['a] cl :
    object
    end
  and cl2 :
    object
    end

  class type clt =
    object
    end
  and ['a] clt2 =
    object
    end

  module M :
    Sig

  module M (X:X) (Y:Y):
    Sig

  module type Sig

  module type Sig1 =
  sig
  end

  open
    M

  include
    M
end
