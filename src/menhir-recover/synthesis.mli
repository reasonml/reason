open MenhirSdk.Cmly_api
open Attributes

val group_assoc : ('a * 'b) list -> ('a * 'b list) list

val pp_list :
   f:(Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a list
  -> unit

(** Specification of synthesized tactics *)

module type SYNTHESIZER = sig
  module G : GRAMMAR

  (* Specification of problems

     There are two situations we want to synthesize solution for:

     - `Head` is when the dot is just in front of some non-terminal, and we
     would like to find a way to move the dot to the right of this symbol (by
     executing a sequence of actions that results in this non-terminal being
     pushed on the stack)

     - `Tail` is when the dot is in some production that we would like to
     reduce. *)

  type variable =
    | Head of G.lr1 * G.nonterminal
    | Tail of G.lr1 * G.production * int

  (* The integer parameter in `Tail` is the position of the dot in the
     production we are trying to reduce. This is necessary to uniquely identify
     a production that occurs multiple time in a state.

     For instance, in the grammar:

     %token<int> INT %token PLUS

     expr: | INT \{ $1 \} (*const*) | expr PLUS expr \{ $1 + $2 \} (*add*)

     Synthesizing `Head (st0, expr)` when `expr PLUS . expr` is in `st0` will
     output the actions to get to the state `st'` containing `expr PLUS expr .`.

     Synthesizing `Tail (st1, add, 1)` when `expr . PLUS expr` is in `st1` will
     output the actions that end up reducing `add` (which will likely be
     shifting `PLUS`, synthesizing `Head (st0, expr)` and reducing add). *)

  val variable_to_string : variable -> string
  (** A human readable representation of a [variable]. *)

  (** Specification of solutions

      A successful synthesis results in a list of actions. *)

  type action =
    | Abort
    | Reduce of G.production
    | Shift of G.symbol
    | Seq of action list

  (* `Abort` is issued if there is no solution. This is the case for instance if
     there is a semantic value that the synthesizer cannot produce, or a
     production with an infinite cost.

     `Shift` and `Reduce` are direct actions to execute on the parser.

     `Seq` is a sequence of action. *)

  val action_to_string : action -> string
  (** A human readable representation of an action. *)

  val solve : variable -> Cost.t * action list
  (** Give the solution found for a variable as a list of action. *)

  val report : Format.formatter -> unit
  (** Print the solutions or absence thereof for the whole grammar. *)
end

(** Synthesizer implementation *)

module Synthesizer (G : GRAMMAR) (_ : ATTRIBUTES with module G = G) :
  SYNTHESIZER with module G := G
