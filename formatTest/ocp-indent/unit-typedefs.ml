(** ocaml type and exception definitions
    (http://caml.inria.fr/pub/docs/manual-ocaml/manual016.html)
*)

type
  t

type
  'a t

type
  +'_a t

type
  -'a t

type
  ('a,
   +'b,
   -'c,-'d)
    t

type t =
  t2

type t =
    A

type t =
    A
  | B of 'a
  | C of 'a * 'b
  | D of ('a) Array.t * 'b list
  | E of _

type t =
  { f1 : t1;
    f2 : 'a;
    mutable f3: t2;
    f4 :
      'a 'b.t2;
  }

type 'a t
  constraint 'a = t
  constraint 'b = 'a

type
  ('a,
   +'b,
   -'c,-'d)
    t
  =
  { f1 : t1;
    f2 : 'a;
    mutable f3: t2;
    f4 :
      t1 * t2;
  }
  constraint 'a = t
  constraint 'b = 'a


exception
  E

exception
  E of
    'a t * string

exception
  E' =
  E
