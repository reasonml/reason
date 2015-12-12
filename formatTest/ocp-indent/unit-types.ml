(** ocaml type expressions
    (http://caml.inria.fr/pub/docs/manual-ocaml/types.html)
*)

module type MySig = sig

  (* variables *)
  val a :
    'ident

  val a :
    _

  (* parentheses *)
  val a :
    ( t )

  (* functions *)
  val a :
    int ->
    int ->
    t
    -> t
    -> t

  val a :
    lab1: int
    -> lab2 :
         (t)
    -> t

  val a :
    ? lab1:
      ( ?l2 : int -> t )
    -> t

  (* tuples *)
  val a :
    (t1 * t2) * (
      t
    )

  (* constructed *)
  val a :
    int

  val a :
    ('a -> 'b) Array.t


  (* aliased *)
  val a :
    int
    as 'bla

  (* polymorphic variants *)
  val a :
    [ `s | `t' |
      `_00 |
      `Aa of int
    ]

  val a : [
    | `s | `t' |
      `_00 |
      `Aa of int
  ]

  val a : [<
    `s | `t' |
    `_00 |
    `Aa of int
  ]

  val a :
    [
      | `s | `t' |
        `_00 |
        `Aa of int
    ]

  val a : [<
    | `Bb of int
             & string
             & t |
      int >
        `a `_bbb
        `c `d
  ]

  (* objects *)
  val a :
    < >

  val a :
    < .. >

  val a :
    < meth: int option;
      meth2: 'a. 'a option;
      meth3: 'a 'b. ('a,'b) Hashtbl.t >

  val a :
    < meth: int option;
      meth2: 'a. 'a option;
      meth3: 'a 'b. ('a,'b) Hashtbl.t;
      .. >

  (* #-types *)
  val a :
    #M.meth

  val a :
    'a#M.meth

  val a :
    ('a,'b*'c)
    #M.meth

end
