open Migrate_parsetree.Ast_404.Parsetree

val configure :
  width:int ->
  assumeExplicitArity:bool -> constructorLists:string list -> unit

val createFormatter : unit ->
  <
    case_list : Format.formatter -> case list -> unit;
    core_type : Format.formatter -> core_type -> unit;
    expression : Format.formatter -> expression -> unit;
    pattern : Format.formatter -> pattern -> unit;
    signature : Reason_comment.t list -> Format.formatter -> signature -> unit;
    structure : Reason_comment.t list -> Format.formatter -> structure -> unit;
    toplevel_phrase : Format.formatter -> toplevel_phrase -> unit;
  >
