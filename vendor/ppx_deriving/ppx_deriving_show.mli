module Ppx_deriving :
  sig
    type deriver = {
      name : string;
      core_type : (Parsetree.core_type -> Parsetree.expression) option;
      type_decl_str :
        options:(string * Parsetree.expression) list ->
        path:string list ->
        Parsetree.type_declaration list -> Parsetree.structure;
      type_ext_str :
        options:(string * Parsetree.expression) list ->
        path:string list -> Parsetree.type_extension -> Parsetree.structure;
      module_type_decl_str :
        options:(string * Parsetree.expression) list ->
        path:string list ->
        Parsetree.module_type_declaration -> Parsetree.structure;
      type_decl_sig :
        options:(string * Parsetree.expression) list ->
        path:string list ->
        Parsetree.type_declaration list -> Parsetree.signature;
      type_ext_sig :
        options:(string * Parsetree.expression) list ->
        path:string list -> Parsetree.type_extension -> Parsetree.signature;
      module_type_decl_sig :
        options:(string * Parsetree.expression) list ->
        path:string list ->
        Parsetree.module_type_declaration -> Parsetree.signature;
    }
    val registry : (string, deriver) Hashtbl.t
    val hooks : (deriver -> unit) Queue.t
    val add_register_hook : (deriver -> unit) -> unit
    val register : deriver -> unit
    val derivers : unit -> deriver list
    val lookup : string -> deriver option
    val raise_errorf :
      ?sub:Location.error list ->
      ?if_highlight:string ->
      ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a
    val create :
      string ->
      ?core_type:(Parsetree.core_type -> Parsetree.expression) ->
      ?type_ext_str:(options:(string * Parsetree.expression) list ->
                     path:string list ->
                     Parsetree.type_extension -> Parsetree.structure) ->
      ?type_ext_sig:(options:(string * Parsetree.expression) list ->
                     path:string list ->
                     Parsetree.type_extension -> Parsetree.signature) ->
      ?type_decl_str:(options:(string * Parsetree.expression) list ->
                      path:string list ->
                      Parsetree.type_declaration list -> Parsetree.structure) ->
      ?type_decl_sig:(options:(string * Parsetree.expression) list ->
                      path:string list ->
                      Parsetree.type_declaration list -> Parsetree.signature) ->
      ?module_type_decl_str:(options:(string * Parsetree.expression) list ->
                             path:string list ->
                             Parsetree.module_type_declaration ->
                             Parsetree.structure) ->
      ?module_type_decl_sig:(options:(string * Parsetree.expression) list ->
                             path:string list ->
                             Parsetree.module_type_declaration ->
                             Parsetree.signature) ->
      unit -> deriver
    val string_of_core_type : Parsetree.core_type -> string
    module Arg :
      sig
        type 'a conv = Parsetree.expression -> ('a, string) Result.result
        val expr : 'a -> ('a, 'b) Result.result
        val int : Parsetree.expression -> (int, string) Result.result
        val bool : Parsetree.expression -> (bool, string) Result.result
        val string : Parsetree.expression -> (string, string) Result.result
        val char : Parsetree.expression -> (char, string) Result.result
        val enum :
          Asttypes.label list ->
          Parsetree.expression -> (Asttypes.label, string) Result.result
        val list :
          (Parsetree.expression -> ('a, string) Result.result) ->
          Parsetree.expression -> ('a list, string) Result.result
        val get_attr :
          deriver:string ->
          (Parsetree.expression -> ('a, string) Result.result) ->
          (string Asttypes.loc * Parsetree.payload) option -> 'a option
        val get_flag :
          deriver:string ->
          (string Asttypes.loc * Parsetree.payload) option -> bool
        val get_expr :
          deriver:string ->
          (Parsetree.expression -> ('a, string) Result.result) ->
          Parsetree.expression -> 'a
      end
    type quoter = {
      mutable next_id : int;
      mutable bindings : Parsetree.value_binding list;
    }
    val create_quoter : unit -> quoter
    val quote : quoter:quoter -> Parsetree.expression -> Parsetree.expression
    val sanitize :
      ?module_:Longident.t ->
      ?quoter:quoter -> Parsetree.expression -> Parsetree.expression
    val with_quoter :
      (quoter -> 'a -> Parsetree.expression) -> 'a -> Parsetree.expression
    val expand_path : path:string list -> string -> string
    val path_of_type_decl :
      path:string list -> Parsetree.type_declaration -> string list
    val mangle :
      ?fixpoint:string ->
      [< `Prefix of string
       | `PrefixSuffix of string * string
       | `Suffix of string ] ->
      string -> string
    val mangle_type_decl :
      ?fixpoint:string ->
      [< `Prefix of string
       | `PrefixSuffix of string * string
       | `Suffix of string ] ->
      Parsetree.type_declaration -> string
    val mangle_lid :
      ?fixpoint:string ->
      [< `Prefix of string
       | `PrefixSuffix of string * string
       | `Suffix of string ] ->
      Longident.t -> Longident.t
    val attr :
      deriver:string ->
      string ->
      (string Asttypes.loc * 'a) list -> (string Asttypes.loc * 'a) option
    val attr_warning :
      Parsetree.expression -> string Asttypes.loc * Parsetree.payload
    val attr_nobuiltin :
      deriver:string ->
      (string Asttypes.loc * Parsetree.payload) list -> bool
    val remove_pervasive_lid : Longident.t -> Longident.t
    val remove_pervasives :
      deriver:string -> Parsetree.core_type -> Parsetree.core_type
    val fold_left_type_params :
      ('a -> string -> 'a) -> 'a -> (Parsetree.core_type * 'b) list -> 'a
    val fold_left_type_decl :
      ('a -> string -> 'a) -> 'a -> Parsetree.type_declaration -> 'a
    val fold_left_type_ext :
      ('a -> string -> 'a) -> 'a -> Parsetree.type_extension -> 'a
    val fold_right_type_params :
      (string -> 'a -> 'a) -> (Parsetree.core_type * 'b) list -> 'a -> 'a
    val fold_right_type_decl :
      (string -> 'a -> 'a) -> Parsetree.type_declaration -> 'a -> 'a
    val fold_right_type_ext :
      (string -> 'a -> 'a) -> Parsetree.type_extension -> 'a -> 'a
    val free_vars_in_core_type : Parsetree.core_type -> String.t list
    val var_name_of_int : int -> string
    val fresh_var : string list -> string
    val poly_fun_of_type_decl :
      Parsetree.type_declaration ->
      Parsetree.expression -> Parsetree.expression
    val poly_fun_of_type_ext :
      Parsetree.type_extension ->
      Parsetree.expression -> Parsetree.expression
    val poly_apply_of_type_decl :
      Parsetree.type_declaration ->
      Parsetree.expression -> Parsetree.expression
    val poly_apply_of_type_ext :
      Parsetree.type_extension ->
      Parsetree.expression -> Parsetree.expression
    val poly_arrow_of_type_decl :
      (Parsetree.core_type -> Parsetree.core_type) ->
      Parsetree.type_declaration ->
      Parsetree.core_type -> Parsetree.core_type
    val poly_arrow_of_type_ext :
      (Parsetree.core_type -> Parsetree.core_type) ->
      Parsetree.type_extension -> Parsetree.core_type -> Parsetree.core_type
    val core_type_of_type_decl :
      Parsetree.type_declaration -> Parsetree.core_type
    val core_type_of_type_ext :
      Parsetree.type_extension -> Parsetree.core_type
    val instantiate :
      string list ->
      Parsetree.type_declaration ->
      Parsetree.core_type * string list * string list
    val fold_exprs : ?unit:'a -> ('a -> 'a -> 'a) -> 'a list -> 'a
    val seq_reduce :
      ?sep:Parsetree.expression ->
      Parsetree.expression -> Parsetree.expression -> Parsetree.expression
    val binop_reduce :
      Parsetree.expression ->
      Parsetree.expression -> Parsetree.expression -> Parsetree.expression
    val strong_type_of_type : Parsetree.core_type -> Parsetree.core_type
    val derive :
      'a ref ->
      Location.t ->
      'b ->
      Parsetree.attributes ->
      (deriver ->
       options:(string * Parsetree.expression) list ->
       path:'a -> 'c -> 'b list) ->
      'c -> 'b list
    val derive_type_decl :
      'a ref ->
      Parsetree.type_declaration list ->
      Location.t ->
      'b ->
      (deriver ->
       options:(string * Parsetree.expression) list ->
       path:'a -> Parsetree.type_declaration list -> 'b list) ->
      'b list
    val derive_type_ext :
      'a ref ->
      Parsetree.type_extension ->
      Location.t ->
      'b ->
      (deriver ->
       options:(string * Parsetree.expression) list ->
       path:'a -> Parsetree.type_extension -> 'b list) ->
      'b list
    val derive_module_type_decl :
      'a ref ->
      Parsetree.module_type_declaration ->
      Location.t ->
      'b ->
      (deriver ->
       options:(string * Parsetree.expression) list ->
       path:'a -> Parsetree.module_type_declaration -> 'b list) ->
      'b list
    val module_from_input_name : unit -> string list
    val pstr_desc_rec_flag :
      Parsetree.structure_item_desc -> Asttypes.rec_flag
    val mapper : Ast_mapper.mapper
    val hash_variant : string -> int
  end
module Ppx_deriving_show :
  sig
    val deriver : string
    val raise_errorf :
      ?sub:Location.error list ->
      ?if_highlight:string ->
      ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a
    val parse_options : (string * Parsetree.expression) list -> unit
    val attr_nobuiltin :
      (string Asttypes.loc * Parsetree.payload) list -> bool
    val attr_printer :
      (string Asttypes.loc * Parsetree.payload) list ->
      Parsetree.expression option
    val attr_polyprinter :
      (string Asttypes.loc * Parsetree.payload) list ->
      Parsetree.expression option
    val attr_opaque : (string Asttypes.loc * Parsetree.payload) list -> bool
    val argn : int -> string
    val argl : string -> string
    val pattn : 'a list -> Parsetree.pattern list
    val pattl :
      Parsetree.label_declaration list -> (string * Parsetree.pattern) list
    val pconstrrec :
      string -> (string * Parsetree.pattern) list -> Parsetree.pattern
    val wrap_printer :
      Ppx_deriving.quoter -> Parsetree.expression -> Parsetree.expression

    val pp_type_of_decl :
      options:(string * Parsetree.expression) list ->
      path:'a -> Parsetree.type_declaration -> Parsetree.core_type
    val show_type_of_decl :
      options:(string * Parsetree.expression) list ->
      path:'a -> Parsetree.type_declaration -> Parsetree.core_type
    val sig_of_type :
      options:(string * Parsetree.expression) list ->
      path:'a -> Parsetree.type_declaration -> Parsetree.signature_item list
    val expr_of_typ :
      Ppx_deriving.quoter -> Parsetree.core_type -> Parsetree.expression
    val str_of_type :
      options:(string * Parsetree.expression) list ->
      path:string list ->
      Parsetree.type_declaration -> Parsetree.value_binding list
  end
