(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
(**
   Generic Ast Rewriter API.

   This module provides all usual traverse functions and some higher-level ones
   on any tree structure as long as we consider only one type of nodes

   @author Louis Gesbert
   @author Valentin Gatien-Baron
   @author Mathieu Barbin
*)

open TraverseInterface

(* module type TRAVERSE_LIFT = *)
(* sig *)
(*   val foldmap : ('acc -> 'expr -> 'acc * 'expr) -> 'acc -> 'code_elt -> 'acc * 'code_elt  *)
(* end *)


(** Some Extensions *)
module Utils : sig

  (** A generalisation of the type needed in S
      ('a, 'at, 'bt ,'b) sub
      'a may be expressions where identifiers are strings
      'b an expressions where identfiers are uniq
      In that case, ('a,'a,'b,'b) represents a function that deconstruct a string expression
      into a - list of string expression
             - a function that expects an ident expression list and build you the the 'original' ident expression

      DON'T LOOK at the types, it's too scary
      Instead take a look at the following example, where you build the subs_cons function for the expressions
      of some ast:
      let subs_cons e =
        match e with
        | Apply (e1,e2) ->
          (* (e1,e2) is a pair of expression and you are currently treating
           * expressions, you write exactly that: *)
          wrap (fun x -> Apply x) ((sub_2 sub_current sub_current) (e1,e2))
        | Match pel ->
          (* pel is a list of pattern * expr
           * we just ignore the pattern since there is no expression inside them
           * we stop the deconstruction on the expression, since it is was we are currently deconstructing *)
          wrap (fun x -> Match x) (sub_list (sub_2 sub_ignore sub_current) pel)
        | _ -> ...

  *)

  type ('a, 'at, 'bt, 'b) sub = 'a -> ('bt list -> 'b) * 'at list

  val sub_2 : ('a1, 'at, 'bt, 'b1) sub -> ('a2, 'at, 'bt, 'b2) sub -> ('a1 * 'a2, 'at, 'bt, 'b1 * 'b2) sub
  val sub_3 : ('a1, 'at, 'bt, 'b1) sub -> ('a2, 'at, 'bt, 'b2) sub -> ('a3, 'at, 'bt, 'b3) sub -> ('a1 * 'a2 * 'a3, 'at, 'bt, 'b1 * 'b2 * 'b3) sub
  val sub_4 : ('a1, 'at, 'bt, 'b1) sub -> ('a2, 'at, 'bt, 'b2) sub -> ('a3, 'at, 'bt, 'b3) sub -> ('a4, 'at, 'bt, 'b4) sub -> ('a1 * 'a2 * 'a3 * 'a4, 'at, 'bt, 'b1 * 'b2 * 'b3 * 'b4) sub
  val sub_list : ('a, 'at, 'bt, 'b) sub -> ('a list, 'at, 'bt, 'b list) sub
  val sub_option : ('a, 'at, 'bt, 'b) sub -> ('a option, 'at, 'bt, 'b option) sub
  val sub_current : ('a, 'a, 'b, 'b) sub
  val sub_ignore : ('a, _, _, 'a) sub

  val wrap : ('a -> 'b) -> ('at list -> 'a) * 't list -> ('at list -> 'b) * 't list
end

(* HACK: tmp until we merge it into TRAVERSE_CORE for TraverseInterface,
   and rename it into TRAVERSE *)
module type OLD_TRAVERSE =
sig

  type 'p t constraint 'p = _ * _ * _
  val traverse_iter : (('p t -> unit) -> 'p t -> unit) -> 'p t -> unit
  val traverse_map : (('p t -> 'p t) -> 'p t -> 'p t) -> 'p t -> 'p t
  val traverse_fold : (('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val traverse_foldmap : (('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val traverse_exists : (('p t -> bool) -> 'p t -> bool) -> 'p t -> bool
  val traverse_forall : (('p t -> bool) -> 'p t -> bool) -> 'p t -> bool
  val traverse_fold_context_down : (('env -> 'a -> 'p t -> 'a) -> 'env -> 'a -> 'p t -> 'a) -> 'env -> 'a -> 'p t -> 'a
  val iter : ('p t -> unit) -> 'p t -> unit
  val iter_up : ('p t -> unit) -> 'p t -> unit
  val iter_down : ('p t -> unit) -> 'p t -> unit
  val map : ('p t -> 'p t) -> 'p t -> 'p t
  val map_up : ('p t -> 'p t) -> 'p t -> 'p t
  val map_down : ('p t -> 'p t) -> 'p t -> 'p t
  val fold : ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val fold_up : ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val fold_down : ('a -> 'p t -> 'a) -> 'a -> 'p t -> 'a
  val foldmap : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val foldmap_up : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val foldmap_down : ('a -> 'p t -> 'a * 'p t) -> 'a -> 'p t -> 'a * 'p t
  val exists : ('p t -> bool) -> 'p t -> bool
  val exists_up : ('p t -> bool) -> 'p t -> bool
  val exists_down : ('p t -> bool) -> 'p t -> bool
  val find : ('p t -> bool) -> 'p t -> 'p t option
  val find_up : ('p t -> bool) -> 'p t -> 'p t option
  val find_down : ('p t -> bool) -> 'p t -> 'p t option
  val findmap : ('p t -> 'a option) -> 'p t -> 'a option
  val findmap_up : ('p t -> 'a option) -> 'p t -> 'a option
  val findmap_down : ('p t -> 'a option) -> 'p t -> 'a option


  (** traverse all the nodes of the tree in an unspecified order *)
  val traverse_fold_right :  (('b t -> 'a -> 'a) -> 'b t -> 'a -> 'a) -> 'b t -> 'a -> 'a

  (** [fold_up_combine ?combine f acc0 t] folds [f] from leaves with [acc0], combining
      accumulators from sub-trees with [combine] before calling [f].
      Default value for combine is (fun _ b -> b)
      <!> Be carefull be using this function without combine, lots of accs are lost *)
  val fold_up_combine : ?combine:('a -> 'a -> 'a) -> ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a

  (** Folds all the nodes of the tree in an unspecified order *)
  val fold_right_down : ('b t -> 'a -> 'a) -> 'b t -> 'a -> 'a
  val foldmap_up_combine : ?combine:('a -> 'a -> 'a) -> ('a -> 'b t -> 'a * 'b t) -> 'a -> 'b t -> 'a * 'b t

  (** Non-recursive versions, e.g. if you want to handle recursion yourself and have a default case *)
  val map_nonrec : ('b t -> 'b t) -> 'b t -> 'b t
  val fold_nonrec : ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
  val foldmap_nonrec : ('a -> 'b t -> 'a * 'b t) -> 'a -> 'b t -> 'a * 'b t

  (** Just because we had fun writing it. Don't use as is, it's probably very slow.
      Applies the rewriting until fixpoint reached *)
  val map_down_fix : ('b t -> 'b t) -> 'b t -> 'b t

  (** Additional functions that let you traverse the type 'c t when they are deep into an arbitrary structure 'b
      as long as you provide the functions to unbuild/rebuild 'b into t lists *)
  type ('b, 'c) sub = ('b, 'c t, 'c t , 'b) Utils.sub

  val lift_iter_up : ('b,'c) sub -> ('c t -> unit) -> ('b -> unit)
  val lift_iter_down : ('b,'c) sub -> ('c t -> unit) -> ('b -> unit)
  val lift_map_up : ('b,'c) sub -> ('c t -> 'c t) -> ('b -> 'b)
  val lift_map_down : ('b,'c) sub -> ('c t -> 'c t) -> ('b -> 'b)
  (* like fold_map_up_for_real *)
  val lift_fold_up_combine : ('b,'c) sub -> ?combine:('a -> 'a -> 'a) -> ('a -> 'c t -> 'a) -> ('a -> 'b -> 'a)
  val lift_fold : ('b,'c) sub -> ('a -> 'c t -> 'a) -> ('a -> 'b -> 'a)
  val lift_fold_right_down : ('b,'c) sub -> ('c t -> 'a -> 'a) -> ('b -> 'a -> 'a)
  val lift_foldmap_up : ('b,'c) sub -> ('a -> 'c t -> 'a * 'c t) -> ('a -> 'b -> 'a * 'b)
  val lift_foldmap_down : ('b,'c) sub -> ('a -> 'c t -> 'a * 'c t) -> ('a -> 'b -> 'a * 'b)
  val lift_exists : ('b,'c) sub -> ('c t -> bool) -> ('b -> bool)
end


(** {6 First implementation} *)


(** Functor giving you the usual traverse functions *)
module Make (X : S) : OLD_TRAVERSE with type 'a t = 'a X.t

(** Functor for map2, fold2, etc. *)
module MakePair (Fst : S) (Snd : S) : OLD_TRAVERSE with type 'a t = 'a Fst.t * 'a Snd.t

(** {6 Second implementation} *)

(** For the second version (S2), you may do not want to write the optimised version of fold, map, iter
    in this case you can use this unoptimzed constructors, to get them from the foldmap_children function *)
module Unoptimized :
sig
  (** Simple recursion *)
  type ('acc, 't, 't2) foldmap = ('acc -> 't -> 'acc * 't) -> 'acc -> 't2 -> 'acc * 't2
  val iter : (unit, 't, 't2) foldmap -> ('t -> unit) -> 't2 -> unit
  val map : (unit, 't, 't2) foldmap -> ('t -> 't) -> 't2 -> 't2
  val fold : ('acc, 't, 't2) foldmap -> ('acc -> 't -> 'acc) -> 'acc -> 't2 -> 'acc

  (** Mutual recursion *)
  type ('acc, 'tA, 'tB) foldmapAB =
    ('acc -> 'tA -> 'acc * 'tA) ->
    ('acc -> 'tB -> 'acc * 'tB) ->
    'acc -> 'tA -> 'acc * 'tA
  val iterAB : (unit, 'tA, 'tB) foldmapAB -> ('tA -> unit) -> ('tB -> unit) -> 'tA -> unit
  val mapAB : (unit, 'tA, 'tB) foldmapAB -> ('tA -> 'tA) -> ('tB -> 'tB) -> 'tA -> 'tA
  val foldAB : ('acc, 'tA, 'tB) foldmapAB -> ('acc -> 'tA -> 'acc) -> ('acc -> 'tB -> 'acc) -> 'acc -> 'tA -> 'acc
end

open TraverseInterface
module Make2 (X : S2) : TRAVERSE with type 'a t = 'a X.t and type 'a container = 'a X.t

module MakeLift1
    (Y : LIFT2)
    (X : TRAVERSE with type 'a container = 'a Y.t and type 'a t = 'a Y.t)
  : TRAVERSE with type 'a t = 'a X.t and type 'a container = 'a Y.container

module MakeLift2
    (Y : LIFT2)
    (X : TRAVERSE with type 'a container = 'a Y.t)
  : TRAVERSE with type 'a t = 'a X.t and type 'a container = 'a Y.container

(* From there, you can build Box of Boxes with MakeBox *)
(* for example, for rewriting rules on a tuple of code, etc...*)

(** {6 Mutual Recursive Trees} *)

module MakeAB (AB : AB) : TRAVERSE_AB with type 'a tA = 'a AB.tA and type 'a tB = 'a AB.tB
