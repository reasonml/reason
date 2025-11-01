open Reason

type 'a parser_result =
  { ast : 'a
  ; comments : Reason_comment.t list
  ; parsed_as_ml : bool
  ; parsed_as_intf : bool
  }

type parse_itype =
  [ `ML
  | `Reason
  | `Binary
  | `BinaryReason
  | `Auto
  ]

type print_itype =
  [ `ML
  | `Reason
  | `Binary
  | `BinaryReason
  | `AST
  | `None
  ]

exception Invalid_config of string

module type PRINTER = sig
  type t

  val parse :
     use_stdin:bool
    -> parse_itype
    -> string
    -> (t * Reason_comment.t list) * bool

  val print :
     print_itype
    -> string
    -> bool
    -> out_channel
    -> Format.formatter
    -> t * Reason_comment.t list
    -> unit
end

val err : string -> 'a
val ocamlBinaryParser : bool -> string -> 'a parser_result
val reasonBinaryParser : bool -> string -> 'a parser_result
val prepare_output_file : string option -> out_channel
val close_output_file : string option -> out_channel -> unit
