module Pre : sig
  type t

  val empty: t
  val process: t -> string -> t
  val finish: t -> Ast.Raw.block list

  val of_channel: in_channel -> Ast.Raw.block list
  val of_string: string -> Ast.Raw.block list
end
