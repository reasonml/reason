(* See the comments in menhir_error_processor.ml *)

val message :
   'a Reason_parser.MenhirInterpreter.env
  -> Reason_parser.token * 'b * 'c
  -> string
