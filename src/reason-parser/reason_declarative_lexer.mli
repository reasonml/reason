type state

val keyword_table : (string, Reason_parser.token) Hashtbl.t
val make : unit -> state
val token : state -> Lexing.lexbuf -> Reason_parser.token
