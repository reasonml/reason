type t =
  S.s        (* looks like a constructor to ocp-indent, which indents too far *)
type t =
  s    (* correct, because this doesn't look like a constructor to ocp-indent *)
type t =
    S                         (* correctly indented a little more, because... *)
type t =
  | S                                   (* we leave room for the vertical bar *)



(* analogous value expressions, analogous to lists, some different from now *)
let _ =
  [ x
  ; y
  ]
let _ =
  [ x;
    y
  ]
let _ =
  ( x
  , y
  )
let _ =
  ( x,
    y
  )
let _ =
  (
    x
  , y
  )
let _ =
  [
    x
  ; y
  ]
let _ = (
  x,
  y
)
let _ = [
  x;
  y
]
let _ = (
  x
, y
)
let _ = [
  x
; y
]
