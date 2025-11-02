type category =
  | EndOfLine
  | SingleLine
  | Regular

type t =
  { location : Location.t
  ; category : category
  ; text : string
  }

val category : t -> category
val location : t -> Location.t
val wrap : t -> string
val make : location:Location.t -> category -> string -> t
val isLineComment : t -> bool
