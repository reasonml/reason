(** This module provides a pretty expressive printer.
    
    This is a vendored and patched version of pretty_expressive that allows
    runtime configuration of page_width via a thunk. *)

module Make(C : Signature.CostFactory): (Signature.PrinterT with type cost = C.t)
(** The pretty printer and document combinators, parameterized by a cost factory. *)

module MakeCompat(C : Signature.CostFactory): (Signature.PrinterCompatT with type cost = C.t)
(** This functor is similar to {!Make}, but it provides operators
    that are compatible with the paper.
    {b Using [open] on it will shadow built-in identifiers.} *)

val make_debug_format : int -> string -> bool -> string -> string
(** [make_debug_format limit content is_tainted cost] returns a debugging string
    containing these parameters. *)

val default_cost_factory : page_width:(unit -> int) -> ?computation_width:(unit -> int) -> unit ->
                           (module Signature.CostFactory with type t = int * int * int)
(** The default cost factory, parameterized by a thunk for the page width limit [page_width],
    and optionally a thunk for the computation width limit [computation_width].
    
    Using thunks allows runtime configuration of page_width - the value is read
    each time it's needed during pretty printing, not captured at module creation time.
    
    When the computation width limit is not specified, it is set to
    [1.2 * page_width ()].

    In this cost factory, the cost type [t] is a triple of natural numbers.

    {ul {- The first component is {i badness}, which is roughly speaking
           the sum of squared overflows over the page width limit}
        {- The second component is sum of overflows over a column separator.}
        {- The third component is the height (number of newlines).}} *)

val version : string
(* a version string *)

