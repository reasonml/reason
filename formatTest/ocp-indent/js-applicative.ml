(* applicative_intf.ml *)

let args =
  bar "A"
  @> baz "B"
  @> nil

let args =
  bar "A"
  @> baz_qux
  @@ zap "D"
  @> nil
