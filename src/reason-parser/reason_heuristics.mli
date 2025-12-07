open Ppxlib

val is_punned_labelled_expression : expression -> string -> bool
val isUnderscoreIdent : expression -> bool
val isUnderscoreApplication : expression -> bool
val melExprCanBeUncurried : expression -> bool
val isPipeFirst : expression -> bool
val isPipeFirstWithNonSimpleJSXChild : expression -> bool
val singleTokenPatternOmmitTrail : string -> bool

val funAppCallbackExceedsWidth :
   printWidth:int
  -> args:(arg_label * expression) list
  -> funExpr:expression
  -> unit
  -> bool
