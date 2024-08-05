open MenhirSdk.Cmly_api
open Attributes
open Synthesis
open Recovery_intf

module Make
    (G : GRAMMAR)
    (A : ATTRIBUTES with module G := G)
    (S : SYNTHESIZER with module G := G)
    (R : RECOVERY with module G := G) : sig
  val emit : Format.formatter -> unit
end
