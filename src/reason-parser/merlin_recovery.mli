module Make
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (_ : Merlin_recovery_intf.RECOVERY with module Parser := Parser) : sig
  type 'a candidate =
    { line : int
    ; min_col : int
    ; max_col : int
    ; env : 'a Parser.env
    }

  type 'a candidates =
    { shifted : Parser.xsymbol option
    ; final : 'a option
    ; candidates : 'a candidate list
    }

  val attempt :
     'a candidates
    -> Parser.token * Lexing.position * Lexing.position
    -> [> `Accept of 'a | `Fail | `Ok of 'a Parser.checkpoint * 'a Parser.env ]

  val generate : 'a Parser.env -> 'a candidates
end
