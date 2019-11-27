open MenhirSdk.Cmly_api

module type RECOVERY = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int

  type recovery = {
    prefix: int;
    cases: (G.lr1 option * item list) list;
  }
  (** [prefix] is the size of the known prefix of the stack.
      It means that in the kernel of current state, there is an item whose dot
      is at position [prefix].
      (we know the incoming symbols for these stack frames and we can enumerate
      the possible state numbers).

      [cases] is a mapping that associates to each possible state found at
      stack.[-prefix]
      (or None if the stack is empty) a list of reductions to execute.

      The actual list of actions to reduce an item [(state, prod, pos)] is
      given by
          [Synthesizer.solution (Trail (state, prod, pos))]
    *)

  val recover : G.lr1 -> recovery
end

module type RECOVER =
  functor (G : GRAMMAR) (S : Synthesis.SYNTHESIZER with module G := G) ->
    (RECOVERY with module G := G)
