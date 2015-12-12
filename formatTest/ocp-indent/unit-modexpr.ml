(** ocaml module expressions
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual019.html)
*)

module M =
struct
end

module M = struct
  ;;
end

module M =
  functor (M1 : T1) -> functor (M2 : T2) ->
  struct
  end

module M = functor (M1 : T1) -> functor (M2 : T2) ->
struct
end

module M =
  functor (M1 : T1) ->
  functor (M2 : T2) ->
  struct
  end

module M =
  functor
    (M1 : T1) ->
  functor
    (M2 : T2) ->
  struct
  end

module M =
  F
    (X)
    (Y)

module M = (
struct
end :
sig
end
)

module M :
  Sig
=
struct
end

module M
    (X1: T1)
    (X2: T2) =
struct end

