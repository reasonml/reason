open Ppxlib

val configure :
   width:int
  -> assumeExplicitArity:bool
  -> constructorLists:string list
  -> unit

val createFormatter :
   unit
  -> < case_list : Format.formatter -> Parsetree.case list -> unit
     ; core_type : Format.formatter -> Parsetree.core_type -> unit
     ; expression : Format.formatter -> Parsetree.expression -> unit
     ; pattern : Format.formatter -> Parsetree.pattern -> unit
     ; signature :
         Reason_comment.t list
         -> Format.formatter
         -> Parsetree.signature
         -> unit
     ; structure :
         Reason_comment.t list
         -> Format.formatter
         -> Parsetree.structure
         -> unit
     ; toplevel_phrase : Format.formatter -> Parsetree.toplevel_phrase -> unit >
