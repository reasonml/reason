type ('a, 'b) t
  = a : 'a
  -> ?b : b
  -> unit

type ('a, 'b) t =
  | A
  | B of ('a, 'b) t * 'k
  | C of 'a * 'b

type t = Foo
       | Bar
       | Baz

type t =
  | Foo
  | Bar
  | Baz

type t =
    Foo
  | Bar
  | Baz

type t = | Foo
         | Bar
         | Baz

type t = Foo | Bar
       | Baz

type t = {
  foo: int -> int;
  bar: 'a;
}

type t = {
  x: int;
}

type t = {
  x: int;
  y: int -> a:string -> ?b:(int -> string) -> unit;
  mutable
    z: int;
  mutable a:
    string -> unit A.t;
}

type t = {
  x: int
; y: int -> a:string -> ?b:(int -> string) -> unit
; mutable
  z: int;
  a: string ->
    unit A.t;
}

type t =
  {
    x: int
  ; y: int -> a:string -> ?b:(int -> string) -> unit
  ; mutable
    z: int;
    a: string ->
      unit A.t;
  }

type t =
  { x: int
  ; y: int -> a:string -> ?b:(int -> string) -> unit
  ; mutable
    z: int
  ; mutable a: string
      -> unit A.t
  }

type t = { x: int
         ; y: int -> a:string -> ?b:(int -> string) -> unit
         ; mutable
           z: int
         ; mutable a: string -> unit A.t
         }

type t = [
  | `a | `b
  | `c
]

type t =
  [
    `a
  | `b
  | `c
  ]

type t =
  [
    | `a
    | `b
    | `c
  ]

type t =
  [ `a
  | `b
  | `c
  ]

type t = [ `a
         | `b
         | `c
         ]

type t = [ `a | `b
         | `c
         ]

module type M = sig
  type t = t0
  and t'
  and t'' = t
  val v: t
end
