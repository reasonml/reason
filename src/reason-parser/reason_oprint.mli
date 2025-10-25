open Format
open Reason_omp.Ast_414.Outcometree

val print_ident : formatter -> out_ident -> unit
val print_out_value : formatter -> out_value -> unit
val print_out_label : formatter -> string * bool * out_type -> unit
val print_out_type : formatter -> out_type -> unit
val print_out_constr : formatter -> out_constructor -> unit
val print_out_class_type : formatter -> out_class_type -> unit
val print_out_module_type : formatter -> out_module_type -> unit
val print_out_sig_item : formatter -> out_sig_item -> unit
val print_out_signature : formatter -> out_sig_item list -> unit
val print_out_type_extension : formatter -> out_type_extension -> unit
val print_out_phrase : formatter -> out_phrase -> unit
val parenthesized_ident : string -> bool
