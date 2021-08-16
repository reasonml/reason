open Migrate_parsetree.Ast_408

type labelled_parameter =
  | Term of Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern
  | Type of string

type let_bindings = {
  lbs_bindings: Parsetree.value_binding list;
  lbs_rec: Asttypes.rec_flag;
  lbs_extension: (Parsetree.attributes * string Asttypes.loc) option;
  lbs_loc: Location.t;
}
