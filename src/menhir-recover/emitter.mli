open MenhirSdk.Cmly_api
open Attributes
open Synthesis
open Recovery_intf

module Make
    (G : GRAMMAR)
    (_ : ATTRIBUTES with module G := G)
    (_ : SYNTHESIZER with module G := G)
    (_ : RECOVERY with module G := G) : sig
  val emit : Format.formatter -> unit
end
