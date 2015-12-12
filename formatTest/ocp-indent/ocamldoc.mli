(** From http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual029.html#htoc172
    The first special comment of the file is the comment associated
    with the whole module.*)


(** Special comments can be placed between elements and are kept
    by the OCamldoc tool, but are not associated to any element.
    @-tags in these comments are ignored.*)

(*******************************************************************)
(** Comments like the one above, with more than two asterisks,
    are ignored. *)

(** The comment for function f. *)
val f : int -> int -> int
(** The continuation of the comment for function f. *)

(** Comment for exception My_exception, even with a simple comment
    between the special comment and the exception.*)
(* Hello, I'm a simple comment :-) *)
exception My_exception of (int -> int) * int

(** Comment for type weather  *)
type weather =
  | Rain of int (** The comment for construtor Rain *)
  | Sun (** The comment for constructor Sun *)

(** Comment for type weather2  *)
type weather2 =
  | Rain of int (** The comment for construtor Rain *)
  | Sun (** The comment for constructor Sun *)
(** I can continue the comment for type weather2 here
    because there is already a comment associated to the last constructor.*)

(** The comment for type my_record *)
type my_record =
  {
    foo : int ;    (** Comment for field foo *)
    bar : string ; (** Comment for field bar *)
  }
(** Continuation of comment for type my_record *)

(** Comment for foo *)
val foo : string
(** This comment is associated to foo and not to bar. *)
val bar : string
(** This comment is assciated to bar. *)

(** The comment for class my_class *)
class my_class :
  object
    (** A comment to describe inheritance from cl *)
    inherit cl

    (** The comment for attribute tutu *)
    val mutable tutu : string

    (** The comment for attribute toto. *)
    val toto : int

    (** This comment is not attached to titi since
        there is a blank line before titi, but is kept
        as a comment in the class. *)

    val titi : string

    (** Comment for method toto *)
    method toto : string

    (** Comment for method m *)
    method m : float -> int
  end

(** The comment for the class type my_class_type *)
class type my_class_type =
  object
    (** The comment for variable x. *)
    val mutable x : int

    (** The commend for method m. *)
    method m : int -> int
  end

(** The comment for module Foo *)
module Foo:
sig
  (** The comment for x *)
  val x : int

  (** A special comment that is kept but not associated to any element *)
end

(** The comment for module type my_module_type. *)
module type my_module_type =
sig
  (** The comment for value x. *)
  val x : int

  (** The comment for module M. *)
  module M:
  sig
    (** The comment for value y. *)
    val y : int

    (* ... *)
  end

end

(** The comment for class my_class *)
(* class my_class: *)
(*   object *)
(*     (** A comment to describe inheritance from cl *) *)
(*     inherit cl *)
(*  *)
(*     (** The comment for the instance variable tutu *) *)
(*     val mutable tutu = "tutu" *)
(*     (** The comment for toto *) *)
(*     val toto = 1 *)
(*     val titi = "titi" *)
(*     (** Comment for method toto *) *)
(*     method toto = tutu ^ "!" *)
(*     (** Comment for method m *) *)
(*     method m (f : float) = 1 *)
(*   end *)

(** The comment for class type my_class_type *)
class type my_class_type =
  object
    (** The comment for the instance variable x. *)
    val mutable x : int
    (** The commend for method m. *)
    method m : int -> int
  end

(** The comment for module Foo *)
module Foo:
sig
  (** The comment for x *)
  val x : int
  (** A special comment in the class, but not associated to any element. *)
end

(** The comment for module type my_module_type. *)
module type my_module_type =
sig
  (* Comment for value x. *)
  val x : int
  (* ... *)
end

(** Starting bla doc *)
type bla =
  | Hup (** The hup case *)
  | Hap (** The hap case *)
(** Ending bla doc *)

(** Starting bla doc *)
type bla =
  | Hup
  (** The hup case *)
  | Hap
  (** The hap case *)
(** Ending bla doc *)

type hop
(** Hop's documentation *)

type mip =
  { fup : int; (** fup field *)
    fip : int; (** fip field *) }
(** Mip's documentation *)

type t = Hey | Ho
(** Let's go. *)

type tp = [ `Hey | `Ho ]
(** Tp doc.
    Second line. *)

(** Starting function f doc *)
val f : 'a -> 'b
(** Ending function f doc. *)

val g : 'a -> t
(** Function g doc.
    Second line. *)

val g : 'a -> [`Hey | `Ho ]
(** Let's go
    Second line. *)

val x : unit -> unit
(** Here are a couple examples of some of its many uses

    {v step (fun m v -> m ~foo:v)
       +> flag "-foo" no_arg : (foo:bool -> 'm, 'm) t
    v}
*)
