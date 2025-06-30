module type RECOVERY = sig
  module Parser : MenhirLib.IncrementalEngine.EVERYTHING

  val default_value : Location.t -> 'a Parser.symbol -> 'a

  type action =
    | Abort
    | R of int
    | S : 'a Parser.symbol -> action
    | Sub of action list

  type decision =
    | Nothing
    | One of action list
    | Select of (int -> action list)

  val depth : int array
  val recover : int -> decision
  val guide : 'a Parser.symbol -> bool
  val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token
  val nullable : 'a Parser.nonterminal -> bool
end
