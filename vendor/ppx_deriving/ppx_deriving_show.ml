open Migrate_parsetree
open Ast_404

open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper

module To_current = Convert(OCaml_404)(OCaml_current)

module Ast_convenience : sig
  (*  This file is part of the ppx_tools package.  It is released  *)
  (*  under the terms of the MIT license (see LICENSE file).       *)
  (*  Copyright 2013  Alain Frisch and LexiFi                      *)

  (** {1 Convenience functions to help build and deconstruct AST fragments.} *)

  (** {2 Compatibility modules} *)

  module Label : sig
    type t = Asttypes.arg_label

    val nolabel : t
  end

  (** {2 Expressions} *)

  val evar: ?loc:loc -> ?attrs:attrs -> string -> expression
  val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
  val app: ?loc:loc -> ?attrs:attrs -> expression -> expression list -> expression

  val str: ?loc:loc -> ?attrs:attrs -> string -> expression

  (** Return [()] if the list is empty. Tail rec. *)

  (** {2 Patterns} *)

  val pvar: ?loc:loc -> ?attrs:attrs -> string -> pattern
  val pconstr: ?loc:loc -> ?attrs:attrs -> string -> pattern list -> pattern
  val precord: ?loc:loc -> ?attrs:attrs -> ?closed:closed_flag -> (string * pattern) list -> pattern
  val ptuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
end = struct
  (*  This file is part of the ppx_tools package.  It is released  *)
  (*  under the terms of the MIT license (see LICENSE file).       *)
  (*  Copyright 2013  Alain Frisch and LexiFi                      *)

  module Label = struct
    type t = Asttypes.arg_label
    let nolabel = Nolabel
  end

  let may_tuple ?loc tup = function
    | [] -> None
    | [x] -> Some x
    | l -> Some (tup ?loc ?attrs:None l)

  let lid ?(loc = !default_loc) s = mkloc (Longident.parse s) loc
  let constr ?loc ?attrs s args = Exp.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Exp.tuple args)
  let unit ?loc ?attrs () = constr ?loc ?attrs "()" []
  let tuple ?loc ?attrs = function
    | [] -> unit ?loc ?attrs ()
    | [x] -> x
    | xs -> Exp.tuple ?loc ?attrs xs
  let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Pconst_string (s, None))
  let app ?loc ?attrs f l = if l = [] then f else Exp.apply ?loc ?attrs f (List.map (fun a -> Label.nolabel, a) l)
  let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lid ?loc s)
  let pvar ?(loc = !default_loc) ?attrs s = Pat.var ~loc ?attrs (mkloc s loc)
  let pconstr ?loc ?attrs s args = Pat.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Pat.tuple args)
  let precord ?loc ?attrs ?(closed = Open) l =
    Pat.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.ppat_loc s, e)) l) closed
  let punit ?loc ?attrs () = pconstr ?loc ?attrs "()" []
  let ptuple ?loc ?attrs = function
    | [] -> punit ?loc ?attrs ()
    | [x] -> x
    | xs -> Pat.tuple ?loc ?attrs xs
end

open Ast_convenience

module Ppx_deriving : sig
  (** Public API of [ppx_deriving] executable. *)

  (** {2 Error handling} *)

  (** [raise_error] is a shorthand for raising [Location.Error] with the result
      of [Location.errorf]. *)
  val raise_errorf : ?sub:Location.error list -> ?if_highlight:string ->
    ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a

  (** [string_of_core_type typ] unparses [typ], omitting any attributes. *)
  val string_of_core_type : Parsetree.core_type -> string

  (** {2 Option parsing} *)

  (** {!Arg} contains convenience functions that extract constants from
      AST fragments, to be used when parsing options or [[\@attributes]]
      attached to types, fields or constructors.

      The [~name] argument is used in error messages and should receive
      the name of the deriving plugin, e.g. ["show"]. *)
  module Arg : sig
    (** A type of conversion functions.

        A conversion function of type ['a conv] converts a raw expression into an
        argument of type ['a]. Or returns [Result.Error "error"] if conversion
        fails. *)
    type 'a conv = expression -> ('a, string) Result.result

    (** [expr] returns the input expression as-is. *)
    val expr : expression conv

    (** [get_attr ~deriver conv attr] extracts the expression from [attr] and converts
        it with [conv], raising [Location.Error] if [attr] is not a structure with
        a single expression or [conv] fails; or returns [None] if [attr] is [None].
        The name of the deriving plugin should be passed as [deriver]; it is used
        in error messages.

        Example usage:
        {[
          let deriver = "index"
          (* ... *)
          let kind =
            match Ppx_deriving.attr ~deriver "kind" pcd_attributes |>
                  Ppx_deriving.Arg.(get_attr ~deriver (enum ["flat"; "nested"])) with
            | Some "flat" -> `flat | Some "nested" -> `nested | None -> `default
          in ..
        ]} *)
    val get_attr : deriver:string -> 'a conv -> attribute option -> 'a option

    (** [get_flag ~deriver attr] returns [true] if [attr] is an empty attribute
        or [false] if it is absent, raising [Location.Error] if [attr] is not
        a structure.
        The name of the deriving plugin should be passed as [deriver]; it is used
        in error messages. *)
    val get_flag : deriver:string -> attribute option -> bool
  end

  (** {2 Hygiene} *)

  (** A [quoter] remembers a set of expressions. *)
  type quoter

  (** [quoter ()] creates an empty quoter. *)
  val create_quoter : unit -> quoter

  (** [quote quoter expr] records a pure expression [expr] within [quoter] and
      returns an expression which has the same value as [expr] in the context
      that [sanitize] provides. *)
  val quote : quoter:quoter -> expression -> expression

  (** [sanitize module_ quoter expr] wraps [expr] in a way that ensures that the
      contents of [module_] and {!Pervasives}, as well as the identifiers in
      expressions returned by [quote] are in scope, and returns the wrapped
      expression. [module_] defaults to !{Ppx_deriving_runtime} if it's not
      provided*)
  val sanitize : ?module_:Longident.t -> ?quoter:quoter -> expression -> expression

  (** {2 AST manipulation} *)

  (** [expand_path name] returns [name] with the [path] module path prepended,
      e.g. [expand_path ["Foo";"M"] "t"] = ["Foo.M.t"] and [expand_path [] "t"] = ["t"] *)
  val expand_path : path:string list -> string -> string

  (** [path_of_type_decl ~path type_] returns [path] if [type_] does not have a manifest
      or the manifest is not a constructor, and the module path of manifest otherwise.

      [path_of_type_decl] is useful when determining the canonical path location
      of fields and constructors; e.g. for [type bar = M.foo = A | B], it will return
      [["M"]]. *)
  val path_of_type_decl : path:string list -> type_declaration -> string list

  (** [mangle_type_decl ~fixpoint affix type_] derives a function name from [type_] name
      by doing nothing if [type_] is named [fixpoint] (["t"] by default), or
      appending and/or prepending [affix] via an underscore. *)
  val mangle_type_decl :
    ?fixpoint:string ->
    [ `Prefix of string | `Suffix of string | `PrefixSuffix of string * string ] ->
    type_declaration -> string

  (** [mangle_lid ~fixpoint affix lid] does the same as {!mangle_type_decl}, but for
      the last component of [lid]. *)
  val mangle_lid : ?fixpoint:string ->
    [ `Prefix of string | `Suffix of string | `PrefixSuffix of string * string] ->
    Longident.t -> Longident.t

  (** [attr ~deriver name attrs] searches for an attribute [\[\@deriving.deriver.attr\]]
      in [attrs] if any attribute with name starting with [\@deriving.deriver] exists,
      or [\[\@deriver.attr\]] if any attribute with name starting with [\@deriver] exists,
      or [\[\@attr\]] otherwise. *)
  val attr : deriver:string -> string -> attributes -> attribute option

  (** [remove_pervasives ~deriver typ] removes the leading "Pervasives."
      module name in longidents.
      Type expressions marked with [\[\@nobuiltin\]] are ignored.

      The name of the deriving plugin should be passed as [deriver]; it is used
      in error messages. *)
  val remove_pervasives : deriver:string -> core_type -> core_type

  (** [poly_fun_of_type_decl type_ expr] wraps [expr] into [fun poly_N -> ...] for every
      type parameter ['N] present in [type_]. For example, if [type_] refers to
      [type ('a, 'b) map], [expr] will be wrapped into [fun poly_a poly_b -> [%e expr]].

      [_] parameters are ignored.  *)
  val poly_fun_of_type_decl : type_declaration -> expression -> expression

  (** [poly_apply_of_type_decl type_ expr] wraps [expr] into [expr poly_N] for every
      type parameter ['N] present in [type_]. For example, if [type_] refers to
      [type ('a, 'b) map], [expr] will be wrapped into [[%e expr] poly_a poly_b].

      [_] parameters are ignored. *)
  val poly_apply_of_type_decl : type_declaration -> expression -> expression

  (** [poly_arrow_of_type_decl fn type_ typ] wraps [typ] in an arrow with [fn [%type: 'N]]
      as argument for every type parameter ['N] present in [type_]. For example, if
      [type_] refers to [type ('a, 'b) map] and [fn] is [fun var -> [%type: [%t var] -> string]],
      [typ] will be wrapped into [('a -> string) -> ('b -> string) -> [%t typ]].

      [_] parameters are ignored. *)
  val poly_arrow_of_type_decl : (core_type -> core_type) ->
    type_declaration -> core_type -> core_type

  (** [core_type_of_type_decl type_] constructs type [('a, 'b, ...) t] for
      type declaration [type ('a, 'b, ...) t = ...]. *)
  val core_type_of_type_decl : type_declaration -> core_type

  (** [fold_exprs ~unit fn exprs] folds [exprs] using head of [exprs] as initial
      accumulator value, or [unit] if [exprs = []].

      See also {!seq_reduce} and {!binop_reduce}. *)
  val fold_exprs : ?unit:expression -> (expression -> expression -> expression) ->
    expression list -> expression

  (** When [sep] is present:
      [seq_reduce] ≡ [fun x a b -> [%expr [%e a]; [%e x]; [%e b]]].
      When [sep] is missing:
      [seq_reduce] ≡ [fun a b -> [%expr [%e a]; [%e b]]]. *)
  val seq_reduce : ?sep:expression -> expression -> expression -> expression

  (** [strong_type_of_type ty] transform a type ty to
      [freevars . ty], giving a strong polymorphic type *)
  val strong_type_of_type: core_type -> core_type
end = struct
  let raise_errorf ?sub  ?if_highlight  ?loc  message =
    message |>
    (Printf.kprintf
       (fun str  ->
          let err = Location.error ?sub ?if_highlight ?loc str  in
          raise (Location.Error err)))

  let string_of_core_type typ =
    Format.asprintf "%a" Pprintast.core_type
      (To_current.copy_core_type { typ with ptyp_attributes = [] })

  module Arg =
  struct
    type 'a conv = expression -> ('a,string) Result.result
    open Result
    let expr expr = Ok expr

    let get_attr ~deriver  conv attr =
      match attr with
      | None  -> None
      | Some ({ txt = name },PStr ({ pstr_desc = Pstr_eval (expr,[]) }::[]))
        ->
        (match conv expr with
         | Ok v -> Some v
         | Error desc ->
           raise_errorf ~loc:(expr.pexp_loc)
             "%s: invalid [@%s]: %s expected" deriver name desc)
      | Some ({ txt = name; loc },_) ->
        raise_errorf ~loc "%s: invalid [@%s]: value expected" deriver name

    let get_flag ~deriver  attr =
      match attr with
      | None  -> false
      | Some ({ txt = name },PStr []) -> true
      | Some ({ txt = name; loc },_) ->
        raise_errorf ~loc "%s: invalid [@%s]: empty structure expected"
          deriver name
  end

  type quoter = {
    mutable next_id: int ;
    mutable bindings: value_binding list }
  let create_quoter () = { next_id = 0; bindings = [] }
  let quote ~quoter  expr =
    let name = "__" ^ (string_of_int quoter.next_id)  in
    quoter.bindings <-
      (Vb.mk (pvar name)
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_fun
                (Asttypes.Nolabel, None,
                 {
                   Parsetree.ppat_desc =
                     (Parsetree.Ppat_construct
                        ({
                          Asttypes.txt = (Longident.Lident "()");
                          Asttypes.loc =
                            (Pervasives.(!) Ast_helper.default_loc)
                        }, None));
                   Parsetree.ppat_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.ppat_attributes = []
                 }, expr));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         })
      :: (quoter.bindings);
    quoter.next_id <- quoter.next_id + 1;
    {
      Parsetree.pexp_desc =
        (Parsetree.Pexp_apply
           ((evar name),
            [(Asttypes.Nolabel,
              {
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_construct
                     ({
                       Asttypes.txt = (Longident.Lident "()");
                       Asttypes.loc =
                         (Pervasives.(!) Ast_helper.default_loc)
                     }, None));
                Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.pexp_attributes = []
              })]));
      Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
      Parsetree.pexp_attributes = []
    }
  let sanitize ?(module_= Lident "Ppx_deriving_runtime")  ?(quoter=
                                                            create_quoter ())  expr =
    let body =
      Exp.open_
        ~attrs:[((mkloc "ocaml.warning" (!Ast_helper.default_loc)),
                 (PStr
                    [{
                      Parsetree.pstr_desc =
                        (Parsetree.Pstr_eval
                           ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_constant
                                  (Parsetree.Pconst_string ("-A", None)));
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           }, []));
                      Parsetree.pstr_loc =
                        (Pervasives.(!) Ast_helper.default_loc)
                    }]))] Override
        { txt = module_; loc = (!Ast_helper.default_loc) } expr
    in
    match quoter.bindings with
    | [] -> body
    | bindings -> Exp.let_ Nonrecursive bindings body

  let expand_path ~path  ident = String.concat "." (path @ [ident])

  let path_of_type_decl ~path  type_decl =
    match type_decl.ptype_manifest with
    | Some { ptyp_desc = Ptyp_constr ({ txt = lid },_) } ->
      (match lid with
       | Lident _ -> []
       | Ldot (lid,_) -> Longident.flatten lid
       | Lapply _ -> assert false)
    | _ -> path

  let mangle ?(fixpoint= "t")  affix name =
    match ((name = fixpoint), affix) with
    | (true ,(`Prefix x|`Suffix x)) -> x
    | (true ,`PrefixSuffix (p,s)) -> p ^ ("_" ^ s)
    | (false ,`PrefixSuffix (p,s)) -> p ^ ("_" ^ (name ^ ("_" ^ s)))
    | (false ,`Prefix x) -> x ^ ("_" ^ name)
    | (false ,`Suffix x) -> name ^ ("_" ^ x)
  let mangle_type_decl ?fixpoint  affix { ptype_name = { txt = name } } =
    mangle ?fixpoint affix name
  let mangle_lid ?fixpoint  affix lid =
    match lid with
    | Lident s -> Lident (mangle ?fixpoint affix s)
    | Ldot (p,s) -> Ldot (p, (mangle ?fixpoint affix s))
    | Lapply _ -> assert false
  let attr ~deriver  name attrs =
    let starts str prefix =
      ((String.length str) >= (String.length prefix)) &&
      ((String.sub str 0 (String.length prefix)) = prefix)
    in
    let try_prefix prefix f =
      if List.exists (fun ({ txt },_)  -> starts txt prefix) attrs
      then prefix ^ name
      else f ()  in
    let name =
      try_prefix ("deriving." ^ (deriver ^ "."))
        (fun ()  -> try_prefix (deriver ^ ".") (fun ()  -> name))
    in
    try Some (List.find (fun ({ txt },_)  -> txt = name) attrs)
    with | Not_found  -> None

  let attr_nobuiltin ~deriver  attrs =
    (attrs |> (attr ~deriver "nobuiltin")) |> (Arg.get_flag ~deriver)
  let rec remove_pervasive_lid =
    function
    | Lident _ as lid -> lid
    | Ldot (Lident "Pervasives",s) -> Lident s
    | Ldot (lid,s) -> Ldot ((remove_pervasive_lid lid), s)
    | Lapply (lid,lid2) ->
      Lapply ((remove_pervasive_lid lid), (remove_pervasive_lid lid2))

  let remove_pervasives ~deriver  typ =
    if attr_nobuiltin ~deriver typ.ptyp_attributes
    then typ
    else
      (let open Ast_mapper in
       let map_typ mapper typ =
         match typ.ptyp_desc with
         | Ptyp_constr (lid,l) ->
           let lid = { lid with txt = (remove_pervasive_lid lid.txt) }  in
           {
             typ with
             ptyp_desc =
               (Ptyp_constr (lid, (List.map (mapper.typ mapper) l)))
           }
         | Ptyp_class (lid,l) ->
           let lid = { lid with txt = (remove_pervasive_lid lid.txt) }  in
           {
             typ with
             ptyp_desc =
               (Ptyp_class (lid, (List.map (mapper.typ mapper) l)))
           }
         | _ -> default_mapper.typ mapper typ  in
       let m = { default_mapper with typ = map_typ }  in m.typ m typ)

  let fold_left_type_params fn accum params =
    List.fold_left
      (fun accum  ->
         fun (param,_)  ->
           match param with
           | { ptyp_desc = Ptyp_any  } -> accum
           | { ptyp_desc = Ptyp_var name } -> fn accum name
           | _ -> assert false) accum params

  let fold_left_type_decl fn accum { ptype_params } =
    fold_left_type_params fn accum ptype_params

  let fold_right_type_params fn params accum =
    List.fold_right
      (fun (param,_)  ->
         fun accum  ->
           match param with
           | { ptyp_desc = Ptyp_any  } -> accum
           | { ptyp_desc = Ptyp_var name } -> fn name accum
           | _ -> assert false) params accum

  let fold_right_type_decl fn { ptype_params } accum =
    fold_right_type_params fn ptype_params accum

  let free_vars_in_core_type typ =
    let rec free_in typ =
      match typ with
      | { ptyp_desc = Ptyp_any  } -> []
      | { ptyp_desc = Ptyp_var name } -> [name]
      | { ptyp_desc = Ptyp_arrow (_,x,y) } -> (free_in x) @ (free_in y)
      | { ptyp_desc = (Ptyp_tuple xs|Ptyp_constr (_,xs)) } ->
        (List.map free_in xs) |> List.concat
      | { ptyp_desc = Ptyp_alias (x,name) } -> [name] @ (free_in x)
      | { ptyp_desc = Ptyp_poly (bound,x) } ->
        List.filter (fun y  -> not (List.mem y bound)) (free_in x)
      | { ptyp_desc = Ptyp_variant (rows,_,_) } ->
        ((List.map
            (function
              | Rtag (_,_,_,ts) -> List.map free_in ts
              | Rinherit t -> [free_in t]) rows)
         |> List.concat)
        |> List.concat
      | _ -> assert false  in
    let uniq lst =
      let module StringSet = (Set.Make)(String) in
      (lst |> StringSet.of_list) |> StringSet.elements
    in
    (free_in typ) |> uniq

  let poly_fun_of_type_decl type_decl expr =
    fold_right_type_decl
      (fun name  ->
         fun expr  -> Exp.fun_ Label.nolabel None (pvar ("poly_" ^ name)) expr)
      type_decl expr

  let poly_apply_of_type_decl type_decl expr =
    fold_left_type_decl
      (fun expr  ->
         fun name  -> Exp.apply expr [(Label.nolabel, (evar ("poly_" ^ name)))])
      expr type_decl

  let poly_arrow_of_type_decl fn type_decl typ =
    fold_right_type_decl
      (fun name  -> fun typ  -> Typ.arrow Label.nolabel (fn (Typ.var name)) typ)
      type_decl typ

  let core_type_of_type_decl { ptype_name = { txt = name }; ptype_params } =
    Typ.constr (mknoloc (Lident name)) (List.map fst ptype_params)

  let fold_exprs ?unit  fn exprs =
    match exprs with
    | a::[] -> a
    | hd::tl -> List.fold_left fn hd tl
    | [] ->
      (match unit with
       | Some x -> x
       | None  -> raise (Invalid_argument "Ppx_deriving.fold_exprs"))

  let seq_reduce ?sep  a b =
    match sep with
    | Some x ->
      {
        Parsetree.pexp_desc =
          (Parsetree.Pexp_sequence
             (a,
              {
                Parsetree.pexp_desc = (Parsetree.Pexp_sequence (x, b));
                Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.pexp_attributes = []
              }));
        Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
        Parsetree.pexp_attributes = []
      }
    | None  ->
      {
        Parsetree.pexp_desc = (Parsetree.Pexp_sequence (a, b));
        Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
        Parsetree.pexp_attributes = []
      }

  let strong_type_of_type ty =
    let free_vars = free_vars_in_core_type ty  in
    Typ.force_poly @@ (Typ.poly free_vars ty)
end

let deriver = "show"
let raise_errorf = Ppx_deriving.raise_errorf
let parse_options options =
  options |>
  (List.iter
     (fun (name,expr)  ->
        match name with
        | _ ->
          raise_errorf ~loc:(expr.pexp_loc)
            "%s does not support option %s" deriver name))

let attr_nobuiltin attrs =
  let open Ppx_deriving in
  (attrs |> (attr ~deriver "nobuiltin")) |> (Arg.get_flag ~deriver)

let attr_printer attrs =
  let open Ppx_deriving in
  (attrs |> (attr ~deriver "printer")) |>
  (let open Arg in get_attr ~deriver expr)

let attr_polyprinter attrs =
  let open Ppx_deriving in
  (attrs |> (attr ~deriver "polyprinter")) |>
  (let open Arg in get_attr ~deriver expr)

let attr_opaque attrs =
  let open Ppx_deriving in
  (attrs |> (attr ~deriver "opaque")) |> (Arg.get_flag ~deriver)

let argn = Printf.sprintf "a%d"
let argl = Printf.sprintf "a%s"
let pattn typs = List.mapi (fun i  -> fun _  -> pvar (argn i)) typs
let pattl labels =
  List.map (fun { pld_name = { txt = n } }  -> (n, (pvar (argl n)))) labels
let pconstrrec name fields = pconstr name [precord ~closed:Closed fields]
let wrap_printer quoter printer =
  Ppx_deriving.quote quoter
    {
      Parsetree.pexp_desc =
        (Parsetree.Pexp_let
           (Asttypes.Nonrecursive,
            [{
              Parsetree.pvb_pat =
                {
                  Parsetree.ppat_desc =
                    (Parsetree.Ppat_var
                       {
                         Asttypes.txt = "fprintf";
                         Asttypes.loc =
                           (Pervasives.(!) Ast_helper.default_loc)
                       });
                  Parsetree.ppat_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.ppat_attributes = []
                };
              Parsetree.pvb_expr =
                {
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Lident "Format"), "fprintf"));
                         Asttypes.loc =
                           (Pervasives.(!) Ast_helper.default_loc)
                       });
                  Parsetree.pexp_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.pexp_attributes = []
                };
              Parsetree.pvb_attributes = [];
              Parsetree.pvb_loc = (Pervasives.(!) Ast_helper.default_loc)
            }], printer));
      Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
      Parsetree.pexp_attributes =
        [({
            Asttypes.txt = "ocaml.warning";
            Asttypes.loc = (Pervasives.(!) Ast_helper.default_loc)
          },
            (Parsetree.PStr
               [{
                 Parsetree.pstr_desc =
                   (Parsetree.Pstr_eval
                      ({
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_constant
                             (Parsetree.Pconst_string ("-26", None)));
                        Parsetree.pexp_loc =
                          (Pervasives.(!) Ast_helper.default_loc);
                        Parsetree.pexp_attributes = []
                      }, []));
                 Parsetree.pstr_loc = (Pervasives.(!) Ast_helper.default_loc)
               }]))]
    }

let pp_type_of_decl ~options  ~path  type_decl =
  parse_options options;
  (let typ = Ppx_deriving.core_type_of_type_decl type_decl  in
   Ppx_deriving.poly_arrow_of_type_decl
     (fun var  ->
        {
          Parsetree.ptyp_desc =
            (Parsetree.Ptyp_arrow
               (Asttypes.Nolabel,
                {
                  Parsetree.ptyp_desc =
                    (Parsetree.Ptyp_constr
                       ({
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Lident "Format"), "formatter"));
                         Asttypes.loc =
                           (Pervasives.(!) Ast_helper.default_loc)
                       }, []));
                  Parsetree.ptyp_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.ptyp_attributes = []
                },
                {
                  Parsetree.ptyp_desc =
                    (Parsetree.Ptyp_arrow
                       (Asttypes.Nolabel, var,
                        {
                          Parsetree.ptyp_desc =
                            (Parsetree.Ptyp_constr
                               ({
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident
                                          "Ppx_deriving_runtime"), "unit"));
                                 Asttypes.loc =
                                   (Pervasives.(!) Ast_helper.default_loc)
                               }, []));
                          Parsetree.ptyp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.ptyp_attributes = []
                        }));
                  Parsetree.ptyp_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.ptyp_attributes = []
                }));
          Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
          Parsetree.ptyp_attributes = []
        }) type_decl
     {
       Parsetree.ptyp_desc =
         (Parsetree.Ptyp_arrow
            (Asttypes.Nolabel,
             {
               Parsetree.ptyp_desc =
                 (Parsetree.Ptyp_constr
                    ({
                      Asttypes.txt =
                        (Longident.Ldot
                           ((Longident.Lident "Format"), "formatter"));
                      Asttypes.loc =
                        (Pervasives.(!) Ast_helper.default_loc)
                    }, []));
               Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
               Parsetree.ptyp_attributes = []
             },
             {
               Parsetree.ptyp_desc =
                 (Parsetree.Ptyp_arrow
                    (Asttypes.Nolabel, typ,
                     {
                       Parsetree.ptyp_desc =
                         (Parsetree.Ptyp_constr
                            ({
                              Asttypes.txt =
                                (Longident.Ldot
                                   ((Longident.Lident
                                       "Ppx_deriving_runtime"), "unit"));
                              Asttypes.loc =
                                (Pervasives.(!) Ast_helper.default_loc)
                            }, []));
                       Parsetree.ptyp_loc =
                         (Pervasives.(!) Ast_helper.default_loc);
                       Parsetree.ptyp_attributes = []
                     }));
               Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
               Parsetree.ptyp_attributes = []
             }));
       Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
       Parsetree.ptyp_attributes = []
     })

let show_type_of_decl ~options  ~path  type_decl =
  parse_options options;
  (let typ = Ppx_deriving.core_type_of_type_decl type_decl  in
   Ppx_deriving.poly_arrow_of_type_decl
     (fun var  ->
        {
          Parsetree.ptyp_desc =
            (Parsetree.Ptyp_arrow
               (Asttypes.Nolabel,
                {
                  Parsetree.ptyp_desc =
                    (Parsetree.Ptyp_constr
                       ({
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Lident "Format"), "formatter"));
                         Asttypes.loc =
                           (Pervasives.(!) Ast_helper.default_loc)
                       }, []));
                  Parsetree.ptyp_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.ptyp_attributes = []
                },
                {
                  Parsetree.ptyp_desc =
                    (Parsetree.Ptyp_arrow
                       (Asttypes.Nolabel, var,
                        {
                          Parsetree.ptyp_desc =
                            (Parsetree.Ptyp_constr
                               ({
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident
                                          "Ppx_deriving_runtime"), "unit"));
                                 Asttypes.loc =
                                   (Pervasives.(!) Ast_helper.default_loc)
                               }, []));
                          Parsetree.ptyp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.ptyp_attributes = []
                        }));
                  Parsetree.ptyp_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.ptyp_attributes = []
                }));
          Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
          Parsetree.ptyp_attributes = []
        }) type_decl
     {
       Parsetree.ptyp_desc =
         (Parsetree.Ptyp_arrow
            (Asttypes.Nolabel, typ,
             {
               Parsetree.ptyp_desc =
                 (Parsetree.Ptyp_constr
                    ({
                      Asttypes.txt =
                        (Longident.Ldot
                           ((Longident.Lident "Ppx_deriving_runtime"),
                            "string"));
                      Asttypes.loc =
                        (Pervasives.(!) Ast_helper.default_loc)
                    }, []));
               Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
               Parsetree.ptyp_attributes = []
             }));
       Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
       Parsetree.ptyp_attributes = []
     })

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter  in
  match attr_printer typ.ptyp_attributes with
  | Some printer ->
    {
      Parsetree.pexp_desc =
        (Parsetree.Pexp_apply
           ((wrap_printer quoter printer),
            [(Asttypes.Nolabel,
              {
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_ident
                     {
                       Asttypes.txt = (Longident.Lident "fmt");
                       Asttypes.loc =
                         (Pervasives.(!) Ast_helper.default_loc)
                     });
                Parsetree.pexp_loc =
                  (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.pexp_attributes = []
              })]));
      Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
      Parsetree.pexp_attributes = []
    }
  | None  ->
    if attr_opaque typ.ptyp_attributes
    then
      {
        Parsetree.pexp_desc =
          (Parsetree.Pexp_fun
             (Asttypes.Nolabel, None,
              {
                Parsetree.ppat_desc = Parsetree.Ppat_any;
                Parsetree.ppat_loc =
                  (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.ppat_attributes = []
              },
              {
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_apply
                     ({
                       Parsetree.pexp_desc =
                         (Parsetree.Pexp_ident
                            {
                              Asttypes.txt =
                                (Longident.Ldot
                                   ((Longident.Lident "Format"),
                                    "pp_print_string"));
                              Asttypes.loc =
                                (Pervasives.(!) Ast_helper.default_loc)
                            });
                       Parsetree.pexp_loc =
                         (Pervasives.(!) Ast_helper.default_loc);
                       Parsetree.pexp_attributes = []
                     },
                       [(Asttypes.Nolabel,
                         {
                           Parsetree.pexp_desc =
                             (Parsetree.Pexp_ident
                                {
                                  Asttypes.txt = (Longident.Lident "fmt");
                                  Asttypes.loc =
                                    (Pervasives.(!)
                                       Ast_helper.default_loc)
                                });
                           Parsetree.pexp_loc =
                             (Pervasives.(!) Ast_helper.default_loc);
                           Parsetree.pexp_attributes = []
                         });
                        (Asttypes.Nolabel,
                         {
                           Parsetree.pexp_desc =
                             (Parsetree.Pexp_constant
                                (Parsetree.Pconst_string
                                   ("<opaque>", None)));
                           Parsetree.pexp_loc =
                             (Pervasives.(!) Ast_helper.default_loc);
                           Parsetree.pexp_attributes = []
                         })]));
                Parsetree.pexp_loc =
                  (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.pexp_attributes = []
              }));
        Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
        Parsetree.pexp_attributes = []
      }
    else
      (let format x =
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_apply
                ({
                  Parsetree.pexp_desc =
                    (Parsetree.Pexp_ident
                       {
                         Asttypes.txt =
                           (Longident.Ldot
                              ((Longident.Lident "Format"), "fprintf"));
                         Asttypes.loc =
                           (Pervasives.(!) Ast_helper.default_loc)
                       });
                  Parsetree.pexp_loc =
                    (Pervasives.(!) Ast_helper.default_loc);
                  Parsetree.pexp_attributes = []
                },
                  [(Asttypes.Nolabel,
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_ident
                           {
                             Asttypes.txt = (Longident.Lident "fmt");
                             Asttypes.loc =
                               (Pervasives.(!) Ast_helper.default_loc)
                           });
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    });
                   (Asttypes.Nolabel, (str x))]));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         }  in
       let seq start finish fold typ =
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_fun
                (Asttypes.Nolabel, None,
                 {
                   Parsetree.ppat_desc =
                     (Parsetree.Ppat_var
                        {
                          Asttypes.txt = "x";
                          Asttypes.loc =
                            (Pervasives.(!) Ast_helper.default_loc)
                        });
                   Parsetree.ppat_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.ppat_attributes = []
                 },
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_sequence
                        ({
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_apply
                               ({
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_ident
                                      {
                                        Asttypes.txt =
                                          (Longident.Ldot
                                             ((Longident.Lident "Format"),
                                              "fprintf"));
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
                                      });
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               },
                                 [(Asttypes.Nolabel,
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_ident
                                          {
                                            Asttypes.txt =
                                              (Longident.Lident "fmt");
                                            Asttypes.loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc)
                                          });
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.pexp_attributes = []
                                   });
                                  (Asttypes.Nolabel, (str start))]));
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        },
                          {
                            Parsetree.pexp_desc =
                              (Parsetree.Pexp_sequence
                                 ({
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_apply
                                        ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Lident
                                                      "ignore");
                                                 Asttypes.loc =
                                                   (Pervasives.(!)
                                                      Ast_helper.default_loc)
                                               });
                                          Parsetree.pexp_loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc);
                                          Parsetree.pexp_attributes = []
                                        },
                                          [(Asttypes.Nolabel,
                                            {
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_apply
                                                   (fold,
                                                    [(Asttypes.Nolabel,
                                                      {
                                                        Parsetree.pexp_desc
                                                        =
                                                          (Parsetree.Pexp_fun
                                                             (Asttypes.Nolabel,
                                                              None,
                                                              {
                                                                Parsetree.ppat_desc
                                                                =
                                                                  (Parsetree.Ppat_var
                                                                     {
                                                                       Asttypes.txt
                                                                       = "sep";
                                                                       Asttypes.loc
                                                                       =
                                                                         (Pervasives.(!)
                                                                            Ast_helper.default_loc)
                                                                     });
                                                                Parsetree.ppat_loc
                                                                =
                                                                  (Pervasives.(!)
                                                                     Ast_helper.default_loc);
                                                                Parsetree.ppat_attributes
                                                                = []
                                                              },
                                                              {
                                                                Parsetree.pexp_desc
                                                                =
                                                                  (Parsetree.Pexp_fun
                                                                     (Asttypes.Nolabel,
                                                                      None,
                                                                      {
                                                                        Parsetree.ppat_desc
                                                                        =
                                                                          (Parsetree.Ppat_var
                                                                             {
                                                                               Asttypes.txt
                                                                               = "x";
                                                                               Asttypes.loc
                                                                               =
                                                                                 (Pervasives.(!)
                                                                                    Ast_helper.default_loc)
                                                                             });
                                                                        Parsetree.ppat_loc
                                                                        =
                                                                          (Pervasives.(!)
                                                                             Ast_helper.default_loc);
                                                                        Parsetree.ppat_attributes
                                                                        = []
                                                                      },
                                                                      {
                                                                        Parsetree.pexp_desc
                                                                        =
                                                                          (Parsetree.Pexp_sequence
                                                                             ({
                                                                               Parsetree.pexp_desc
                                                                               =
                                                                                 (Parsetree.Pexp_ifthenelse
                                                                                    ({
                                                                                      Parsetree.pexp_desc
                                                                                      =
                                                                                        (Parsetree.Pexp_ident
                                                                                           {
                                                                                             Asttypes.txt
                                                                                             =
                                                                                               (Longident.Lident
                                                                                                  "sep");
                                                                                             Asttypes.loc
                                                                                             =
                                                                                               (Pervasives.(!)
                                                                                                  Ast_helper.default_loc)
                                                                                           });
                                                                                      Parsetree.pexp_loc
                                                                                      =
                                                                                        (Pervasives.(!)
                                                                                           Ast_helper.default_loc);
                                                                                      Parsetree.pexp_attributes
                                                                                      = []
                                                                                    },
                                                                                      {
                                                                                        Parsetree.pexp_desc
                                                                                        =
                                                                                          (Parsetree.Pexp_apply
                                                                                             ({
                                                                                               Parsetree.pexp_desc
                                                                                               =
                                                                                                 (Parsetree.Pexp_ident
                                                                                                    {
                                                                                                      Asttypes.txt
                                                                                                      =
                                                                                                        (Longident.Ldot
                                                                                                           ((Longident.Lident
                                                                                                               "Format"),
                                                                                                            "fprintf"));
                                                                                                      Asttypes.loc
                                                                                                      =
                                                                                                        (Pervasives.(!)
                                                                                                           Ast_helper.default_loc)
                                                                                                    });
                                                                                               Parsetree.pexp_loc
                                                                                               =
                                                                                                 (Pervasives.(!)
                                                                                                    Ast_helper.default_loc);
                                                                                               Parsetree.pexp_attributes
                                                                                               = []
                                                                                             },
                                                                                               [
                                                                                                 (Asttypes.Nolabel,
                                                                                                  {
                                                                                                    Parsetree.pexp_desc
                                                                                                    =
                                                                                                      (Parsetree.Pexp_ident
                                                                                                         {
                                                                                                           Asttypes.txt
                                                                                                           =
                                                                                                             (Longident.Lident
                                                                                                                "fmt");
                                                                                                           Asttypes.loc
                                                                                                           =
                                                                                                             (Pervasives.(!)
                                                                                                                Ast_helper.default_loc)
                                                                                                         });
                                                                                                    Parsetree.pexp_loc
                                                                                                    =
                                                                                                      (Pervasives.(!)
                                                                                                         Ast_helper.default_loc);
                                                                                                    Parsetree.pexp_attributes
                                                                                                    = []
                                                                                                  });
                                                                                                 (Asttypes.Nolabel,
                                                                                                  {
                                                                                                    Parsetree.pexp_desc
                                                                                                    =
                                                                                                      (Parsetree.Pexp_constant
                                                                                                         (Parsetree.Pconst_string
                                                                                                            (";@ ",
                                                                                                             None)));
                                                                                                    Parsetree.pexp_loc
                                                                                                    =
                                                                                                      (Pervasives.(!)
                                                                                                         Ast_helper.default_loc);
                                                                                                    Parsetree.pexp_attributes
                                                                                                    = []
                                                                                                  })]));
                                                                                        Parsetree.pexp_loc
                                                                                        =
                                                                                          (Pervasives.(!)
                                                                                             Ast_helper.default_loc);
                                                                                        Parsetree.pexp_attributes
                                                                                        = []
                                                                                      }, None));
                                                                               Parsetree.pexp_loc
                                                                               =
                                                                                 (Pervasives.(!)
                                                                                    Ast_helper.default_loc);
                                                                               Parsetree.pexp_attributes
                                                                               = []
                                                                             },
                                                                               {
                                                                                 Parsetree.pexp_desc
                                                                                 =
                                                                                   (Parsetree.Pexp_sequence
                                                                                      ({
                                                                                        Parsetree.pexp_desc
                                                                                        =
                                                                                          (Parsetree.Pexp_apply
                                                                                             ((expr_of_typ
                                                                                                 typ),
                                                                                              [
                                                                                                (Asttypes.Nolabel,
                                                                                                 {
                                                                                                   Parsetree.pexp_desc
                                                                                                   =
                                                                                                     (Parsetree.Pexp_ident
                                                                                                        {
                                                                                                          Asttypes.txt
                                                                                                          =
                                                                                                            (Longident.Lident
                                                                                                               "x");
                                                                                                          Asttypes.loc
                                                                                                          =
                                                                                                            (Pervasives.(!)
                                                                                                               Ast_helper.default_loc)
                                                                                                        });
                                                                                                   Parsetree.pexp_loc
                                                                                                   =
                                                                                                     (Pervasives.(!)
                                                                                                        Ast_helper.default_loc);
                                                                                                   Parsetree.pexp_attributes
                                                                                                   = []
                                                                                                 })]));
                                                                                        Parsetree.pexp_loc
                                                                                        =
                                                                                          (Pervasives.(!)
                                                                                             Ast_helper.default_loc);
                                                                                        Parsetree.pexp_attributes
                                                                                        = []
                                                                                      },
                                                                                        {
                                                                                          Parsetree.pexp_desc
                                                                                          =
                                                                                            (Parsetree.Pexp_construct
                                                                                               ({
                                                                                                 Asttypes.txt
                                                                                                 =
                                                                                                   (Longident.Lident
                                                                                                      "true");
                                                                                                 Asttypes.loc
                                                                                                 =
                                                                                                   (Pervasives.(!)
                                                                                                      Ast_helper.default_loc)
                                                                                               }, None));
                                                                                          Parsetree.pexp_loc
                                                                                          =
                                                                                            (Pervasives.(!)
                                                                                               Ast_helper.default_loc);
                                                                                          Parsetree.pexp_attributes
                                                                                          = []
                                                                                        }));
                                                                                 Parsetree.pexp_loc
                                                                                 =
                                                                                   (Pervasives.(!)
                                                                                      Ast_helper.default_loc);
                                                                                 Parsetree.pexp_attributes
                                                                                 = []
                                                                               }));
                                                                        Parsetree.pexp_loc
                                                                        =
                                                                          (Pervasives.(!)
                                                                             Ast_helper.default_loc);
                                                                        Parsetree.pexp_attributes
                                                                        = []
                                                                      }));
                                                                Parsetree.pexp_loc
                                                                =
                                                                  (Pervasives.(!)
                                                                     Ast_helper.default_loc);
                                                                Parsetree.pexp_attributes
                                                                = []
                                                              }));
                                                        Parsetree.pexp_loc
                                                        =
                                                          (Pervasives.(!)
                                                             Ast_helper.default_loc);
                                                        Parsetree.pexp_attributes
                                                        = []
                                                      });
                                                     (Asttypes.Nolabel,
                                                      {
                                                        Parsetree.pexp_desc
                                                        =
                                                          (Parsetree.Pexp_construct
                                                             ({
                                                               Asttypes.txt
                                                               =
                                                                 (Longident.Lident
                                                                    "false");
                                                               Asttypes.loc
                                                               =
                                                                 (Pervasives.(!)
                                                                    Ast_helper.default_loc)
                                                             }, None));
                                                        Parsetree.pexp_loc
                                                        =
                                                          (Pervasives.(!)
                                                             Ast_helper.default_loc);
                                                        Parsetree.pexp_attributes
                                                        = []
                                                      });
                                                     (Asttypes.Nolabel,
                                                      {
                                                        Parsetree.pexp_desc
                                                        =
                                                          (Parsetree.Pexp_ident
                                                             {
                                                               Asttypes.txt
                                                               =
                                                                 (Longident.Lident
                                                                    "x");
                                                               Asttypes.loc
                                                               =
                                                                 (Pervasives.(!)
                                                                    Ast_helper.default_loc)
                                                             });
                                                        Parsetree.pexp_loc
                                                        =
                                                          (Pervasives.(!)
                                                             Ast_helper.default_loc);
                                                        Parsetree.pexp_attributes
                                                        = []
                                                      })]));
                                              Parsetree.pexp_loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc);
                                              Parsetree.pexp_attributes
                                              = []
                                            })]));
                                   Parsetree.pexp_loc =
                                     (Pervasives.(!)
                                        Ast_helper.default_loc);
                                   Parsetree.pexp_attributes = []
                                 },
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ({
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Ldot
                                                        ((Longident.Lident
                                                            "Format"),
                                                         "fprintf"));
                                                   Asttypes.loc =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc)
                                                 });
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes =
                                              []
                                          },
                                            [(Asttypes.Nolabel,
                                              {
                                                Parsetree.pexp_desc =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt =
                                                         (Longident.Lident
                                                            "fmt");
                                                       Asttypes.loc =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc)
                                                     });
                                                Parsetree.pexp_loc =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              });
                                             (Asttypes.Nolabel,
                                              (str finish))]));
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.pexp_attributes = []
                                   }));
                            Parsetree.pexp_loc =
                              (Pervasives.(!) Ast_helper.default_loc);
                            Parsetree.pexp_attributes = []
                          }));
                   Parsetree.pexp_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.pexp_attributes = []
                 }));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         }  in
       let typ = Ppx_deriving.remove_pervasives ~deriver typ  in
       match typ with
       | { Parsetree.ptyp_desc = Parsetree.Ptyp_any ;
           Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ } ->
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_fun
                (Asttypes.Nolabel, None,
                 {
                   Parsetree.ppat_desc = Parsetree.Ppat_any;
                   Parsetree.ppat_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.ppat_attributes = []
                 },
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_apply
                        ({
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_ident
                               {
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident "Format"),
                                       "pp_print_string"));
                                 Asttypes.loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc)
                               });
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        },
                          [(Asttypes.Nolabel,
                            {
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_ident
                                   {
                                     Asttypes.txt =
                                       (Longident.Lident "fmt");
                                     Asttypes.loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc)
                                   });
                              Parsetree.pexp_loc =
                                (Pervasives.(!) Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            });
                           (Asttypes.Nolabel,
                            {
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_constant
                                   (Parsetree.Pconst_string ("_", None)));
                              Parsetree.pexp_loc =
                                (Pervasives.(!) Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            })]));
                   Parsetree.pexp_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.pexp_attributes = []
                 }));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         }
       | { ptyp_desc = Ptyp_arrow _ } ->
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_fun
                (Asttypes.Nolabel, None,
                 {
                   Parsetree.ppat_desc = Parsetree.Ppat_any;
                   Parsetree.ppat_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.ppat_attributes = []
                 },
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_apply
                        ({
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_ident
                               {
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident "Format"),
                                       "pp_print_string"));
                                 Asttypes.loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc)
                               });
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        },
                          [(Asttypes.Nolabel,
                            {
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_ident
                                   {
                                     Asttypes.txt =
                                       (Longident.Lident "fmt");
                                     Asttypes.loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc)
                                   });
                              Parsetree.pexp_loc =
                                (Pervasives.(!) Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            });
                           (Asttypes.Nolabel,
                            {
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_constant
                                   (Parsetree.Pconst_string
                                      ("<fun>", None)));
                              Parsetree.pexp_loc =
                                (Pervasives.(!) Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            })]));
                   Parsetree.pexp_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.pexp_attributes = []
                 }));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         }
       | { ptyp_desc = Ptyp_constr _ } ->
         let builtin = not (attr_nobuiltin typ.ptyp_attributes)  in
         (match (builtin, typ) with
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "unit";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_fun
                   (Asttypes.Nolabel, None,
                    {
                      Parsetree.ppat_desc =
                        (Parsetree.Ppat_construct
                           ({
                             Asttypes.txt = (Longident.Lident "()");
                             Asttypes.loc =
                               (Pervasives.(!) Ast_helper.default_loc)
                           }, None));
                      Parsetree.ppat_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.ppat_attributes = []
                    },
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_apply
                           ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_ident
                                  {
                                    Asttypes.txt =
                                      (Longident.Ldot
                                         ((Longident.Lident "Format"),
                                          "pp_print_string"));
                                    Asttypes.loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc)
                                  });
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           },
                             [(Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_ident
                                      {
                                        Asttypes.txt =
                                          (Longident.Lident "fmt");
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
                                      });
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               });
                              (Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_constant
                                      (Parsetree.Pconst_string
                                         ("()", None)));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               })]));
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    }));
              Parsetree.pexp_loc =
                (Pervasives.(!) Ast_helper.default_loc);
              Parsetree.pexp_attributes = []
            }
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "int";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%d"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "int32";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
          |(true
           ,{
             Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                 ({
                   Asttypes.txt = Longident.Ldot
                       (Longident.Lident "Int32","t");
                   Asttypes.loc = _ },[]);
             Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%ldl"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "int64";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
          |(true
           ,{
             Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                 ({
                   Asttypes.txt = Longident.Ldot
                       (Longident.Lident "Int64","t");
                   Asttypes.loc = _ },[]);
             Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%LdL"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "nativeint";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
          |(true
           ,{
             Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                 ({
                   Asttypes.txt = Longident.Ldot
                       (Longident.Lident "Nativeint","t");
                   Asttypes.loc = _ },[]);
             Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%ndn"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "float";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%F"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "bool";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%B"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "char";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%C"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "string";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
          |(true
           ,{
             Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                 ({
                   Asttypes.txt = Longident.Ldot
                       (Longident.Lident "String","t");
                   Asttypes.loc = _ },[]);
             Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            -> format "%S"
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "bytes";
                     Asttypes.loc = _ },[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
          |(true
           ,{
             Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                 ({
                   Asttypes.txt = Longident.Ldot
                       (Longident.Lident "Bytes","t");
                   Asttypes.loc = _ },[]);
             Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_fun
                   (Asttypes.Nolabel, None,
                    {
                      Parsetree.ppat_desc =
                        (Parsetree.Ppat_var
                           {
                             Asttypes.txt = "x";
                             Asttypes.loc =
                               (Pervasives.(!) Ast_helper.default_loc)
                           });
                      Parsetree.ppat_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.ppat_attributes = []
                    },
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_apply
                           ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_ident
                                  {
                                    Asttypes.txt =
                                      (Longident.Ldot
                                         ((Longident.Lident "Format"),
                                          "fprintf"));
                                    Asttypes.loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc)
                                  });
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           },
                             [(Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_ident
                                      {
                                        Asttypes.txt =
                                          (Longident.Lident "fmt");
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
                                      });
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               });
                              (Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_constant
                                      (Parsetree.Pconst_string
                                         ("%S", None)));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               });
                              (Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_apply
                                      ({
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_ident
                                             {
                                               Asttypes.txt =
                                                 (Longident.Ldot
                                                    ((Longident.Lident
                                                        "Bytes"),
                                                     "to_string"));
                                               Asttypes.loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc)
                                             });
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes =
                                          []
                                      },
                                        [(Asttypes.Nolabel,
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Lident
                                                        "x");
                                                   Asttypes.loc =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc)
                                                 });
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes
                                            = []
                                          })]));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               })]));
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    }));
              Parsetree.pexp_loc =
                (Pervasives.(!) Ast_helper.default_loc);
              Parsetree.pexp_attributes = []
            }
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "ref";
                     Asttypes.loc = _ },typ::[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_fun
                   (Asttypes.Nolabel, None,
                    {
                      Parsetree.ppat_desc =
                        (Parsetree.Ppat_var
                           {
                             Asttypes.txt = "x";
                             Asttypes.loc =
                               (Pervasives.(!) Ast_helper.default_loc)
                           });
                      Parsetree.ppat_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.ppat_attributes = []
                    },
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_sequence
                           ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_apply
                                  ({
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_ident
                                         {
                                           Asttypes.txt =
                                             (Longident.Ldot
                                                ((Longident.Lident
                                                    "Format"),
                                                 "pp_print_string"));
                                           Asttypes.loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc)
                                         });
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  },
                                    [(Asttypes.Nolabel,
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_ident
                                             {
                                               Asttypes.txt =
                                                 (Longident.Lident
                                                    "fmt");
                                               Asttypes.loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc)
                                             });
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes =
                                          []
                                      });
                                     (Asttypes.Nolabel,
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_constant
                                             (Parsetree.Pconst_string
                                                ("ref (", None)));
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes =
                                          []
                                      })]));
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           },
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_sequence
                                    ({
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_apply
                                           ((expr_of_typ typ),
                                            [(Asttypes.Nolabel,
                                              {
                                                Parsetree.pexp_desc
                                                =
                                                  (Parsetree.Pexp_apply
                                                     ({
                                                       Parsetree.pexp_desc
                                                       =
                                                         (
                                                           Parsetree.Pexp_ident
                                                             {
                                                               Asttypes.txt
                                                               =
                                                                 (Longident.Lident
                                                                    "!");
                                                               Asttypes.loc
                                                               =
                                                                 (Pervasives.(!)
                                                                    Ast_helper.default_loc)
                                                             });
                                                       Parsetree.pexp_loc
                                                       =
                                                         (
                                                           Pervasives.(!)
                                                             Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                       = []
                                                     },
                                                       [(Asttypes.Nolabel,
                                                         {
                                                           Parsetree.pexp_desc
                                                           =
                                                             (Parsetree.Pexp_ident
                                                                {
                                                                  Asttypes.txt
                                                                  =
                                                                    (Longident.Lident
                                                                       "x");
                                                                  Asttypes.loc
                                                                  =
                                                                    (Pervasives.(!)
                                                                       Ast_helper.default_loc)
                                                                });
                                                           Parsetree.pexp_loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc);
                                                           Parsetree.pexp_attributes
                                                           = []
                                                         })]));
                                                Parsetree.pexp_loc
                                                =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              })]));
                                      Parsetree.pexp_loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc);
                                      Parsetree.pexp_attributes = []
                                    },
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_apply
                                             ({
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_ident
                                                    {
                                                      Asttypes.txt =
                                                        (Longident.Ldot
                                                           ((Longident.Lident
                                                               "Format"),
                                                            "pp_print_string"));
                                                      Asttypes.loc =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc)
                                                    });
                                               Parsetree.pexp_loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc);
                                               Parsetree.pexp_attributes
                                               = []
                                             },
                                               [(Asttypes.Nolabel,
                                                 {
                                                   Parsetree.pexp_desc
                                                   =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt
                                                          =
                                                            (
                                                              Longident.Lident
                                                                "fmt");
                                                          Asttypes.loc
                                                          =
                                                            (
                                                              Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                        });
                                                   Parsetree.pexp_loc
                                                   =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc);
                                                   Parsetree.pexp_attributes
                                                   = []
                                                 });
                                                (Asttypes.Nolabel,
                                                 {
                                                   Parsetree.pexp_desc
                                                   =
                                                     (Parsetree.Pexp_constant
                                                        (Parsetree.Pconst_string
                                                           (")", None)));
                                                   Parsetree.pexp_loc
                                                   =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc);
                                                   Parsetree.pexp_attributes
                                                   = []
                                                 })]));
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes = []
                                      }));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!)
                                    Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             }));
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    }));
              Parsetree.pexp_loc =
                (Pervasives.(!) Ast_helper.default_loc);
              Parsetree.pexp_attributes = []
            }
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "list";
                     Asttypes.loc = _ },typ::[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            seq "@[<2>[" "@,]@]"
              {
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_ident
                     {
                       Asttypes.txt =
                         (Longident.Ldot
                            ((Longident.Lident "List"), "fold_left"));
                       Asttypes.loc =
                         (Pervasives.(!) Ast_helper.default_loc)
                     });
                Parsetree.pexp_loc =
                  (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.pexp_attributes = []
              } typ
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "array";
                     Asttypes.loc = _ },typ::[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            seq "@[<2>[|" "@,|]@]"
              {
                Parsetree.pexp_desc =
                  (Parsetree.Pexp_ident
                     {
                       Asttypes.txt =
                         (Longident.Ldot
                            ((Longident.Lident "Array"), "fold_left"));
                       Asttypes.loc =
                         (Pervasives.(!) Ast_helper.default_loc)
                     });
                Parsetree.pexp_loc =
                  (Pervasives.(!) Ast_helper.default_loc);
                Parsetree.pexp_attributes = []
              } typ
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({ Asttypes.txt = Longident.Lident "option";
                     Asttypes.loc = _ },typ::[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_function
                   [{
                     Parsetree.pc_lhs =
                       {
                         Parsetree.ppat_desc =
                           (Parsetree.Ppat_construct
                              ({
                                Asttypes.txt =
                                  (Longident.Lident "None");
                                Asttypes.loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc)
                              }, None));
                         Parsetree.ppat_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.ppat_attributes = []
                       };
                     Parsetree.pc_guard = None;
                     Parsetree.pc_rhs =
                       {
                         Parsetree.pexp_desc =
                           (Parsetree.Pexp_apply
                              ({
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_ident
                                     {
                                       Asttypes.txt =
                                         (Longident.Ldot
                                            ((Longident.Lident
                                                "Format"),
                                             "pp_print_string"));
                                       Asttypes.loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc)
                                     });
                                Parsetree.pexp_loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc);
                                Parsetree.pexp_attributes = []
                              },
                                [(Asttypes.Nolabel,
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_ident
                                         {
                                           Asttypes.txt =
                                             (Longident.Lident "fmt");
                                           Asttypes.loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc)
                                         });
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  });
                                 (Asttypes.Nolabel,
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_constant
                                         (Parsetree.Pconst_string
                                            ("None", None)));
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  })]));
                         Parsetree.pexp_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.pexp_attributes = []
                       }
                   };
                    {
                      Parsetree.pc_lhs =
                        {
                          Parsetree.ppat_desc =
                            (Parsetree.Ppat_construct
                               ({
                                 Asttypes.txt =
                                   (Longident.Lident "Some");
                                 Asttypes.loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc)
                               },
                                 (Some
                                    {
                                      Parsetree.ppat_desc =
                                        (Parsetree.Ppat_var
                                           {
                                             Asttypes.txt = "x";
                                             Asttypes.loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc)
                                           });
                                      Parsetree.ppat_loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc);
                                      Parsetree.ppat_attributes = []
                                    })));
                          Parsetree.ppat_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.ppat_attributes = []
                        };
                      Parsetree.pc_guard = None;
                      Parsetree.pc_rhs =
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_sequence
                               ({
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_apply
                                      ({
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_ident
                                             {
                                               Asttypes.txt =
                                                 (Longident.Ldot
                                                    ((Longident.Lident
                                                        "Format"),
                                                     "pp_print_string"));
                                               Asttypes.loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc)
                                             });
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes = []
                                      },
                                        [(Asttypes.Nolabel,
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Lident
                                                        "fmt");
                                                   Asttypes.loc =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc)
                                                 });
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes
                                            = []
                                          });
                                         (Asttypes.Nolabel,
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_constant
                                                 (Parsetree.Pconst_string
                                                    ("(Some ", None)));
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes =
                                              []
                                          })]));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               },
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_sequence
                                        ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_apply
                                               ((expr_of_typ typ),
                                                [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_ident
                                                         {
                                                           Asttypes.txt
                                                           =
                                                             (Longident.Lident
                                                                "x");
                                                           Asttypes.loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                         });
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  })]));
                                          Parsetree.pexp_loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc);
                                          Parsetree.pexp_attributes =
                                            []
                                        },
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_apply
                                                 ({
                                                   Parsetree.pexp_desc
                                                   =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt
                                                          =
                                                            (Longident.Ldot
                                                               ((Longident.Lident
                                                                   "Format"),
                                                                "pp_print_string"));
                                                          Asttypes.loc
                                                          =
                                                            (Pervasives.(!)
                                                               Ast_helper.default_loc)
                                                        });
                                                   Parsetree.pexp_loc =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc);
                                                   Parsetree.pexp_attributes
                                                   = []
                                                 },
                                                   [(Asttypes.Nolabel,
                                                     {
                                                       Parsetree.pexp_desc
                                                       =
                                                         (Parsetree.Pexp_ident
                                                            {
                                                              Asttypes.txt
                                                              =
                                                                (Longident.Lident
                                                                   "fmt");
                                                              Asttypes.loc
                                                              =
                                                                (Pervasives.(!)
                                                                   Ast_helper.default_loc)
                                                            });
                                                       Parsetree.pexp_loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                       = []
                                                     });
                                                    (Asttypes.Nolabel,
                                                     {
                                                       Parsetree.pexp_desc
                                                       =
                                                         (Parsetree.Pexp_constant
                                                            (Parsetree.Pconst_string
                                                               (")",
                                                                None)));
                                                       Parsetree.pexp_loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                       = []
                                                     })]));
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes =
                                              []
                                          }));
                                   Parsetree.pexp_loc =
                                     (Pervasives.(!)
                                        Ast_helper.default_loc);
                                   Parsetree.pexp_attributes = []
                                 }));
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        }
                    }]);
              Parsetree.pexp_loc =
                (Pervasives.(!) Ast_helper.default_loc);
              Parsetree.pexp_attributes = []
            }
          | (true
            ,{
              Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                  ({
                    Asttypes.txt = Longident.Ldot
                        (Longident.Lident "Result","result");
                    Asttypes.loc = _ },ok_t::err_t::[]);
              Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ })
            ->
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_function
                   [{
                     Parsetree.pc_lhs =
                       {
                         Parsetree.ppat_desc =
                           (Parsetree.Ppat_construct
                              ({
                                Asttypes.txt =
                                  (Longident.Ldot
                                     ((Longident.Lident "Result"),
                                      "Ok"));
                                Asttypes.loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc)
                              },
                                (Some
                                   {
                                     Parsetree.ppat_desc =
                                       (Parsetree.Ppat_var
                                          {
                                            Asttypes.txt = "ok";
                                            Asttypes.loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc)
                                          });
                                     Parsetree.ppat_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.ppat_attributes = []
                                   })));
                         Parsetree.ppat_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.ppat_attributes = []
                       };
                     Parsetree.pc_guard = None;
                     Parsetree.pc_rhs =
                       {
                         Parsetree.pexp_desc =
                           (Parsetree.Pexp_sequence
                              ({
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_apply
                                     ({
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_ident
                                            {
                                              Asttypes.txt =
                                                (Longident.Ldot
                                                   ((Longident.Lident
                                                       "Format"),
                                                    "pp_print_string"));
                                              Asttypes.loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc)
                                            });
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes =
                                         []
                                     },
                                       [(Asttypes.Nolabel,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_ident
                                                {
                                                  Asttypes.txt =
                                                    (Longident.Lident
                                                       "fmt");
                                                  Asttypes.loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc)
                                                });
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes
                                           = []
                                         });
                                        (Asttypes.Nolabel,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_constant
                                                (Parsetree.Pconst_string
                                                   ("(Ok ", None)));
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes
                                           = []
                                         })]));
                                Parsetree.pexp_loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc);
                                Parsetree.pexp_attributes = []
                              },
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_sequence
                                       ({
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_apply
                                              ((expr_of_typ ok_t),
                                               [(Asttypes.Nolabel,
                                                 {
                                                   Parsetree.pexp_desc
                                                   =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt
                                                          =
                                                            (Longident.Lident
                                                               "ok");
                                                          Asttypes.loc
                                                          =
                                                            (Pervasives.(!)
                                                               Ast_helper.default_loc)
                                                        });
                                                   Parsetree.pexp_loc
                                                   =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc);
                                                   Parsetree.pexp_attributes
                                                   = []
                                                 })]));
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes =
                                           []
                                       },
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_apply
                                                ({
                                                  Parsetree.pexp_desc
                                                  =
                                                    (Parsetree.Pexp_ident
                                                       {
                                                         Asttypes.txt
                                                         =
                                                           (Longident.Ldot
                                                              ((Longident.Lident
                                                                  "Format"),
                                                               "pp_print_string"));
                                                         Asttypes.loc
                                                         =
                                                           (Pervasives.(!)
                                                              Ast_helper.default_loc)
                                                       });
                                                  Parsetree.pexp_loc
                                                  =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc);
                                                  Parsetree.pexp_attributes
                                                  = []
                                                },
                                                  [(Asttypes.Nolabel,
                                                    {
                                                      Parsetree.pexp_desc
                                                      =
                                                        (Parsetree.Pexp_ident
                                                           {
                                                             Asttypes.txt
                                                             =
                                                               (Longident.Lident
                                                                  "fmt");
                                                             Asttypes.loc
                                                             =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc)
                                                           });
                                                      Parsetree.pexp_loc
                                                      =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc);
                                                      Parsetree.pexp_attributes
                                                      = []
                                                    });
                                                   (Asttypes.Nolabel,
                                                    {
                                                      Parsetree.pexp_desc
                                                      =
                                                        (Parsetree.Pexp_constant
                                                           (Parsetree.Pconst_string
                                                              (")",
                                                               None)));
                                                      Parsetree.pexp_loc
                                                      =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc);
                                                      Parsetree.pexp_attributes
                                                      = []
                                                    })]));
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes =
                                             []
                                         }));
                                  Parsetree.pexp_loc =
                                    (Pervasives.(!)
                                       Ast_helper.default_loc);
                                  Parsetree.pexp_attributes = []
                                }));
                         Parsetree.pexp_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.pexp_attributes = []
                       }
                   };
                    {
                      Parsetree.pc_lhs =
                        {
                          Parsetree.ppat_desc =
                            (Parsetree.Ppat_construct
                               ({
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident "Result"),
                                       "Error"));
                                 Asttypes.loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc)
                               },
                                 (Some
                                    {
                                      Parsetree.ppat_desc =
                                        (Parsetree.Ppat_var
                                           {
                                             Asttypes.txt = "e";
                                             Asttypes.loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc)
                                           });
                                      Parsetree.ppat_loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc);
                                      Parsetree.ppat_attributes = []
                                    })));
                          Parsetree.ppat_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.ppat_attributes = []
                        };
                      Parsetree.pc_guard = None;
                      Parsetree.pc_rhs =
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_sequence
                               ({
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_apply
                                      ({
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_ident
                                             {
                                               Asttypes.txt =
                                                 (Longident.Ldot
                                                    ((Longident.Lident
                                                        "Format"),
                                                     "pp_print_string"));
                                               Asttypes.loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc)
                                             });
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes = []
                                      },
                                        [(Asttypes.Nolabel,
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_ident
                                                 {
                                                   Asttypes.txt =
                                                     (Longident.Lident
                                                        "fmt");
                                                   Asttypes.loc =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc)
                                                 });
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes
                                            = []
                                          });
                                         (Asttypes.Nolabel,
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_constant
                                                 (Parsetree.Pconst_string
                                                    ("(Error ", None)));
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes =
                                              []
                                          })]));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               },
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_sequence
                                        ({
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_apply
                                               ((expr_of_typ err_t),
                                                [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_ident
                                                         {
                                                           Asttypes.txt
                                                           =
                                                             (Longident.Lident
                                                                "e");
                                                           Asttypes.loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                         });
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  })]));
                                          Parsetree.pexp_loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc);
                                          Parsetree.pexp_attributes =
                                            []
                                        },
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_apply
                                                 ({
                                                   Parsetree.pexp_desc
                                                   =
                                                     (Parsetree.Pexp_ident
                                                        {
                                                          Asttypes.txt
                                                          =
                                                            (Longident.Ldot
                                                               ((Longident.Lident
                                                                   "Format"),
                                                                "pp_print_string"));
                                                          Asttypes.loc
                                                          =
                                                            (Pervasives.(!)
                                                               Ast_helper.default_loc)
                                                        });
                                                   Parsetree.pexp_loc =
                                                     (Pervasives.(!)
                                                        Ast_helper.default_loc);
                                                   Parsetree.pexp_attributes
                                                   = []
                                                 },
                                                   [(Asttypes.Nolabel,
                                                     {
                                                       Parsetree.pexp_desc
                                                       =
                                                         (Parsetree.Pexp_ident
                                                            {
                                                              Asttypes.txt
                                                              =
                                                                (Longident.Lident
                                                                   "fmt");
                                                              Asttypes.loc
                                                              =
                                                                (Pervasives.(!)
                                                                   Ast_helper.default_loc)
                                                            });
                                                       Parsetree.pexp_loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                       = []
                                                     });
                                                    (Asttypes.Nolabel,
                                                     {
                                                       Parsetree.pexp_desc
                                                       =
                                                         (Parsetree.Pexp_constant
                                                            (Parsetree.Pconst_string
                                                               (")",
                                                                None)));
                                                       Parsetree.pexp_loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                       = []
                                                     })]));
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes =
                                              []
                                          }));
                                   Parsetree.pexp_loc =
                                     (Pervasives.(!)
                                        Ast_helper.default_loc);
                                   Parsetree.pexp_attributes = []
                                 }));
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        }
                    }]);
              Parsetree.pexp_loc =
                (Pervasives.(!) Ast_helper.default_loc);
              Parsetree.pexp_attributes = []
            }
          | (true
            ,({
               Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                   ({ Asttypes.txt = Longident.Lident "lazy_t";
                      Asttypes.loc = _ },typ::[]);
               Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ }
             |{
               Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                   ({
                     Asttypes.txt = Longident.Ldot
                         (Longident.Lident "Lazy","t");
                     Asttypes.loc = _ },typ::[]);
               Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ }))
            ->
            {
              Parsetree.pexp_desc =
                (Parsetree.Pexp_fun
                   (Asttypes.Nolabel, None,
                    {
                      Parsetree.ppat_desc =
                        (Parsetree.Ppat_var
                           {
                             Asttypes.txt = "x";
                             Asttypes.loc =
                               (Pervasives.(!) Ast_helper.default_loc)
                           });
                      Parsetree.ppat_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.ppat_attributes = []
                    },
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_ifthenelse
                           ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_apply
                                  ({
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_ident
                                         {
                                           Asttypes.txt =
                                             (Longident.Ldot
                                                ((Longident.Lident
                                                    "Lazy"),
                                                 "is_val"));
                                           Asttypes.loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc)
                                         });
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  },
                                    [(Asttypes.Nolabel,
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_ident
                                             {
                                               Asttypes.txt =
                                                 (Longident.Lident
                                                    "x");
                                               Asttypes.loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc)
                                             });
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes =
                                          []
                                      })]));
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           },
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_apply
                                    ((expr_of_typ typ),
                                     [(Asttypes.Nolabel,
                                       {
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_apply
                                              ({
                                                Parsetree.pexp_desc
                                                =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt
                                                       =
                                                         (Longident.Ldot
                                                            ((Longident.Lident
                                                                "Lazy"),
                                                             "force"));
                                                       Asttypes.loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc)
                                                     });
                                                Parsetree.pexp_loc
                                                =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              },
                                                [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_ident
                                                         {
                                                           Asttypes.txt
                                                           =
                                                             (Longident.Lident
                                                                "x");
                                                           Asttypes.loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                         });
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  })]));
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes =
                                           []
                                       })]));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!)
                                    Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             },
                             (Some
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_apply
                                       ({
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_ident
                                              {
                                                Asttypes.txt =
                                                  (Longident.Ldot
                                                     ((Longident.Lident
                                                         "Format"),
                                                      "pp_print_string"));
                                                Asttypes.loc =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc)
                                              });
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes =
                                           []
                                       },
                                         [(Asttypes.Nolabel,
                                           {
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_ident
                                                  {
                                                    Asttypes.txt =
                                                      (Longident.Lident
                                                         "fmt");
                                                    Asttypes.loc =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc)
                                                  });
                                             Parsetree.pexp_loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc);
                                             Parsetree.pexp_attributes
                                             = []
                                           });
                                          (Asttypes.Nolabel,
                                           {
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_constant
                                                  (Parsetree.Pconst_string
                                                     ("<not evaluated>",
                                                      None)));
                                             Parsetree.pexp_loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc);
                                             Parsetree.pexp_attributes
                                             = []
                                           })]));
                                  Parsetree.pexp_loc =
                                    (Pervasives.(!)
                                       Ast_helper.default_loc);
                                  Parsetree.pexp_attributes = []
                                })));
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    }));
              Parsetree.pexp_loc =
                (Pervasives.(!) Ast_helper.default_loc);
              Parsetree.pexp_attributes = []
            }
          | (_,{ ptyp_desc = Ptyp_constr ({ txt = lid },args) }) ->
            let args_pp =
              List.map
                (fun typ  ->
                   {
                     Parsetree.pexp_desc =
                       (Parsetree.Pexp_fun
                          (Asttypes.Nolabel, None,
                           {
                             Parsetree.ppat_desc =
                               (Parsetree.Ppat_var
                                  {
                                    Asttypes.txt = "fmt";
                                    Asttypes.loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc)
                                  });
                             Parsetree.ppat_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.ppat_attributes = []
                           }, (expr_of_typ typ)));
                     Parsetree.pexp_loc =
                       (Pervasives.(!) Ast_helper.default_loc);
                     Parsetree.pexp_attributes = []
                   }) args
            in
            let printer =
              match attr_polyprinter typ.ptyp_attributes with
              | Some printer -> wrap_printer quoter printer
              | None  ->
                let printer =
                  Exp.ident
                    (mknoloc
                       (Ppx_deriving.mangle_lid (`Prefix "pp") lid))
                in
                Ppx_deriving.quote quoter printer
            in
            app printer
              (args_pp @
               [{
                 Parsetree.pexp_desc =
                   (Parsetree.Pexp_ident
                      {
                        Asttypes.txt = (Longident.Lident "fmt");
                        Asttypes.loc =
                          (Pervasives.(!) Ast_helper.default_loc)
                      });
                 Parsetree.pexp_loc =
                   (Pervasives.(!) Ast_helper.default_loc);
                 Parsetree.pexp_attributes = []
               }])
          | _ -> assert false)
       | { ptyp_desc = Ptyp_tuple typs } ->
         let args =
           List.mapi
             (fun i  -> fun typ  -> app (expr_of_typ typ) [evar (argn i)])
             typs
         in
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_fun
                (Asttypes.Nolabel, None,
                 (ptuple
                    (List.mapi (fun i  -> fun _  -> pvar (argn i)) typs)),
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_sequence
                        ({
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_apply
                               ({
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_ident
                                      {
                                        Asttypes.txt =
                                          (Longident.Ldot
                                             ((Longident.Lident
                                                 "Format"), "fprintf"));
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
                                      });
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               },
                                 [(Asttypes.Nolabel,
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_ident
                                          {
                                            Asttypes.txt =
                                              (Longident.Lident "fmt");
                                            Asttypes.loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc)
                                          });
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.pexp_attributes = []
                                   });
                                  (Asttypes.Nolabel,
                                   {
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_constant
                                          (Parsetree.Pconst_string
                                             ("(@[", None)));
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.pexp_attributes = []
                                   })]));
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        },
                          {
                            Parsetree.pexp_desc =
                              (Parsetree.Pexp_sequence
                                 ((args |>
                                   (let open Ppx_deriving in
                                    fold_exprs
                                      (seq_reduce
                                         ~sep:{
                                           Parsetree.pexp_desc
                                           =
                                             (Parsetree.Pexp_apply
                                                ({
                                                  Parsetree.pexp_desc
                                                  =
                                                    (Parsetree.Pexp_ident
                                                       {
                                                         Asttypes.txt
                                                         =
                                                           (Longident.Ldot
                                                              ((Longident.Lident
                                                                  "Format"),
                                                               "fprintf"));
                                                         Asttypes.loc
                                                         =
                                                           (Pervasives.(!)
                                                              Ast_helper.default_loc)
                                                       });
                                                  Parsetree.pexp_loc
                                                  =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc);
                                                  Parsetree.pexp_attributes
                                                  = []
                                                },
                                                  [(Asttypes.Nolabel,
                                                    {
                                                      Parsetree.pexp_desc
                                                      =
                                                        (Parsetree.Pexp_ident
                                                           {
                                                             Asttypes.txt
                                                             =
                                                               (Longident.Lident
                                                                  "fmt");
                                                             Asttypes.loc
                                                             =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc)
                                                           });
                                                      Parsetree.pexp_loc
                                                      =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc);
                                                      Parsetree.pexp_attributes
                                                      = []
                                                    });
                                                   (Asttypes.Nolabel,
                                                    {
                                                      Parsetree.pexp_desc
                                                      =
                                                        (Parsetree.Pexp_constant
                                                           (Parsetree.Pconst_string
                                                              (",@ ",
                                                               None)));
                                                      Parsetree.pexp_loc
                                                      =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc);
                                                      Parsetree.pexp_attributes
                                                      = []
                                                    })]));
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes
                                           = []
                                         }))),
                                  {
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_ident
                                                {
                                                  Asttypes.txt =
                                                    (Longident.Ldot
                                                       ((Longident.Lident
                                                           "Format"),
                                                        "fprintf"));
                                                  Asttypes.loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc)
                                                });
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes =
                                             []
                                         },
                                           [(Asttypes.Nolabel,
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_ident
                                                    {
                                                      Asttypes.txt =
                                                        (Longident.Lident
                                                           "fmt");
                                                      Asttypes.loc =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc)
                                                    });
                                               Parsetree.pexp_loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc);
                                               Parsetree.pexp_attributes
                                               = []
                                             });
                                            (Asttypes.Nolabel,
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_constant
                                                    (Parsetree.Pconst_string
                                                       ("@])", None)));
                                               Parsetree.pexp_loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc);
                                               Parsetree.pexp_attributes
                                               = []
                                             })]));
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  }));
                            Parsetree.pexp_loc =
                              (Pervasives.(!) Ast_helper.default_loc);
                            Parsetree.pexp_attributes = []
                          }));
                   Parsetree.pexp_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.pexp_attributes = []
                 }));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         }
       | { ptyp_desc = Ptyp_variant (fields,_,_); ptyp_loc } ->
         let cases =
           fields |>
           (List.map
              (fun field  ->
                 match field with
                 | Rtag (label,_,true ,[]) ->
                   Exp.case (Pat.variant label None)
                     {
                       Parsetree.pexp_desc =
                         (Parsetree.Pexp_apply
                            ({
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_ident
                                   {
                                     Asttypes.txt =
                                       (Longident.Ldot
                                          ((Longident.Lident
                                              "Format"),
                                           "pp_print_string"));
                                     Asttypes.loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc)
                                   });
                              Parsetree.pexp_loc =
                                (Pervasives.(!)
                                   Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            },
                              [(Asttypes.Nolabel,
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_ident
                                       {
                                         Asttypes.txt =
                                           (Longident.Lident "fmt");
                                         Asttypes.loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc)
                                       });
                                  Parsetree.pexp_loc =
                                    (Pervasives.(!)
                                       Ast_helper.default_loc);
                                  Parsetree.pexp_attributes = []
                                });
                               (Asttypes.Nolabel, (str ("`" ^ label)))]));
                       Parsetree.pexp_loc =
                         (Pervasives.(!) Ast_helper.default_loc);
                       Parsetree.pexp_attributes = []
                     }
                 | Rtag (label,_,false ,typ::[]) ->
                   Exp.case
                     (Pat.variant label
                        (Some
                           {
                             Parsetree.ppat_desc =
                               (Parsetree.Ppat_var
                                  {
                                    Asttypes.txt = "x";
                                    Asttypes.loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc)
                                  });
                             Parsetree.ppat_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.ppat_attributes = []
                           }))
                     {
                       Parsetree.pexp_desc =
                         (Parsetree.Pexp_sequence
                            ({
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_apply
                                   ({
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_ident
                                          {
                                            Asttypes.txt =
                                              (Longident.Ldot
                                                 ((Longident.Lident
                                                     "Format"),
                                                  "fprintf"));
                                            Asttypes.loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc)
                                          });
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.pexp_attributes = []
                                   },
                                     [(Asttypes.Nolabel,
                                       {
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_ident
                                              {
                                                Asttypes.txt =
                                                  (Longident.Lident
                                                     "fmt");
                                                Asttypes.loc =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc)
                                              });
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes
                                         = []
                                       });
                                      (Asttypes.Nolabel,
                                       (str
                                          ("`" ^
                                           (label ^ " (@[<hov>"))))]));
                              Parsetree.pexp_loc =
                                (Pervasives.(!)
                                   Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            },
                              {
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_sequence
                                     ({
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_apply
                                            ((expr_of_typ typ),
                                             [(Asttypes.Nolabel,
                                               {
                                                 Parsetree.pexp_desc
                                                 =
                                                   (Parsetree.Pexp_ident
                                                      {
                                                        Asttypes.txt
                                                        =
                                                          (Longident.Lident
                                                             "x");
                                                        Asttypes.loc
                                                        =
                                                          (Pervasives.(!)
                                                             Ast_helper.default_loc)
                                                      });
                                                 Parsetree.pexp_loc
                                                 =
                                                   (Pervasives.(!)
                                                      Ast_helper.default_loc);
                                                 Parsetree.pexp_attributes
                                                 = []
                                               })]));
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes =
                                         []
                                     },
                                       {
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_apply
                                              ({
                                                Parsetree.pexp_desc
                                                =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt
                                                       =
                                                         (Longident.Ldot
                                                            ((Longident.Lident
                                                                "Format"),
                                                             "fprintf"));
                                                       Asttypes.loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc)
                                                     });
                                                Parsetree.pexp_loc =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              },
                                                [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_ident
                                                         {
                                                           Asttypes.txt
                                                           =
                                                             (Longident.Lident
                                                                "fmt");
                                                           Asttypes.loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                         });
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  });
                                                 (Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_constant
                                                         (Parsetree.Pconst_string
                                                            ("@])",
                                                             None)));
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  })]));
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes =
                                           []
                                       }));
                                Parsetree.pexp_loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc);
                                Parsetree.pexp_attributes = []
                              }));
                       Parsetree.pexp_loc =
                         (Pervasives.(!) Ast_helper.default_loc);
                       Parsetree.pexp_attributes = []
                     }
                 | Rinherit
                     ({ ptyp_desc = Ptyp_constr (tname,_) } as typ) ->
                   Exp.case
                     {
                       Parsetree.ppat_desc =
                         (Parsetree.Ppat_alias
                            ((Pat.type_ tname),
                             {
                               Asttypes.txt = "x";
                               Asttypes.loc =
                                 (Pervasives.(!)
                                    Ast_helper.default_loc)
                             }));
                       Parsetree.ppat_loc =
                         (Pervasives.(!) Ast_helper.default_loc);
                       Parsetree.ppat_attributes = []
                     }
                     {
                       Parsetree.pexp_desc =
                         (Parsetree.Pexp_apply
                            ((expr_of_typ typ),
                             [(Asttypes.Nolabel,
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_ident
                                      {
                                        Asttypes.txt =
                                          (Longident.Lident "x");
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
                                      });
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               })]));
                       Parsetree.pexp_loc =
                         (Pervasives.(!) Ast_helper.default_loc);
                       Parsetree.pexp_attributes = []
                     }
                 | _ ->
                   raise_errorf ~loc:ptyp_loc
                     "%s cannot be derived for %s" deriver
                     (Ppx_deriving.string_of_core_type typ)))
         in
         Exp.function_ cases
       | { ptyp_desc = Ptyp_var name } ->
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_apply
                ((evar ("poly_" ^ name)),
                 [(Asttypes.Nolabel,
                   {
                     Parsetree.pexp_desc =
                       (Parsetree.Pexp_ident
                          {
                            Asttypes.txt = (Longident.Lident "fmt");
                            Asttypes.loc =
                              (Pervasives.(!) Ast_helper.default_loc)
                          });
                     Parsetree.pexp_loc =
                       (Pervasives.(!) Ast_helper.default_loc);
                     Parsetree.pexp_attributes = []
                   })]));
           Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.pexp_attributes = []
         }
       | { ptyp_desc = Ptyp_alias (typ,_) } -> expr_of_typ typ
       | { ptyp_loc } ->
         raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver
           (Ppx_deriving.string_of_core_type typ))

let str_of_type ~options  ~path  ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  (let quoter = Ppx_deriving.create_quoter ()  in
   let path = Ppx_deriving.path_of_type_decl ~path type_decl  in
   let prettyprinter =
     match ((type_decl.ptype_kind), (type_decl.ptype_manifest)) with
     | (Ptype_abstract ,Some manifest) ->
       {
         Parsetree.pexp_desc =
           (Parsetree.Pexp_fun
              (Asttypes.Nolabel, None,
               {
                 Parsetree.ppat_desc =
                   (Parsetree.Ppat_var
                      {
                        Asttypes.txt = "fmt";
                        Asttypes.loc =
                          (Pervasives.(!) Ast_helper.default_loc)
                      });
                 Parsetree.ppat_loc =
                   (Pervasives.(!) Ast_helper.default_loc);
                 Parsetree.ppat_attributes = []
               }, (expr_of_typ quoter manifest)));
         Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
         Parsetree.pexp_attributes = []
       }
     | (Ptype_variant constrs,_) ->
       let cases =
         constrs |>
         (List.map
            (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes }
              ->
                let constr_name = Ppx_deriving.expand_path ~path name'  in
                match ((attr_printer pcd_attributes), pcd_args) with
                | (Some printer,Pcstr_tuple args) ->
                  let rec range from_idx to_idx =
                    if from_idx = to_idx
                    then []
                    else from_idx :: (range (from_idx + 1) to_idx)  in
                  let indices = range 0 (List.length args)  in
                  let pattern_vars =
                    List.map (fun i  -> pvar ("a" ^ (string_of_int i)))
                      indices
                  in
                  let expr_vars =
                    List.map (fun i  -> evar ("a" ^ (string_of_int i)))
                      indices
                  in
                  Exp.case (pconstr name' pattern_vars)
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_apply
                           ((wrap_printer quoter printer),
                            [(Asttypes.Nolabel,
                              {
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_ident
                                     {
                                       Asttypes.txt =
                                         (Longident.Lident "fmt");
                                       Asttypes.loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc)
                                     });
                                Parsetree.pexp_loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc);
                                Parsetree.pexp_attributes = []
                              });
                             (Asttypes.Nolabel, (tuple expr_vars))]));
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    }
                | (Some printer,Pcstr_record labels) ->
                  let args =
                    labels |>
                    (List.map
                       (fun { pld_name = { txt = n } }  ->
                          evar (argl n)))
                  in
                  Exp.case (pconstrrec name' (pattl labels))
                    (app (wrap_printer quoter printer)
                       ({
                         Parsetree.pexp_desc =
                           (Parsetree.Pexp_ident
                              {
                                Asttypes.txt = (Longident.Lident "fmt");
                                Asttypes.loc =
                                  (Pervasives.(!)
                                     Ast_helper.default_loc)
                              });
                         Parsetree.pexp_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.pexp_attributes = []
                       } :: args))
                | (None ,Pcstr_tuple typs) ->
                  let args =
                    List.mapi
                      (fun i  ->
                         fun typ  ->
                           app (expr_of_typ quoter typ) [evar (argn i)])
                      typs
                  in
                  let printer =
                    match args with
                    | [] ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_apply
                             ({
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_ident
                                    {
                                      Asttypes.txt =
                                        (Longident.Ldot
                                           ((Longident.Lident
                                               "Format"),
                                            "pp_print_string"));
                                      Asttypes.loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc)
                                    });
                               Parsetree.pexp_loc =
                                 (Pervasives.(!)
                                    Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             },
                               [(Asttypes.Nolabel,
                                 {
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_ident
                                        {
                                          Asttypes.txt =
                                            (Longident.Lident "fmt");
                                          Asttypes.loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc)
                                        });
                                   Parsetree.pexp_loc =
                                     (Pervasives.(!)
                                        Ast_helper.default_loc);
                                   Parsetree.pexp_attributes = []
                                 });
                                (Asttypes.Nolabel, (str constr_name))]));
                        Parsetree.pexp_loc =
                          (Pervasives.(!) Ast_helper.default_loc);
                        Parsetree.pexp_attributes = []
                      }
                    | arg::[] ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_sequence
                             ({
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_apply
                                    ({
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_ident
                                           {
                                             Asttypes.txt =
                                               (Longident.Ldot
                                                  ((Longident.Lident
                                                      "Format"),
                                                   "fprintf"));
                                             Asttypes.loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc)
                                           });
                                      Parsetree.pexp_loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc);
                                      Parsetree.pexp_attributes = []
                                    },
                                      [(Asttypes.Nolabel,
                                        {
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Lident
                                                      "fmt");
                                                 Asttypes.loc =
                                                   (Pervasives.(!)
                                                      Ast_helper.default_loc)
                                               });
                                          Parsetree.pexp_loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc);
                                          Parsetree.pexp_attributes
                                          = []
                                        });
                                       (Asttypes.Nolabel,
                                        (str
                                           ("(@[<2>" ^
                                            (constr_name ^ "@ "))))]));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!)
                                    Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             },
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_sequence
                                      (arg,
                                       {
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_apply
                                              ({
                                                Parsetree.pexp_desc
                                                =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt
                                                       =
                                                         (Longident.Ldot
                                                            ((Longident.Lident
                                                                "Format"),
                                                             "fprintf"));
                                                       Asttypes.loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc)
                                                     });
                                                Parsetree.pexp_loc =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              },
                                                [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_ident
                                                         {
                                                           Asttypes.txt
                                                           =
                                                             (Longident.Lident
                                                                "fmt");
                                                           Asttypes.loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                         });
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  });
                                                 (Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_constant
                                                         (Parsetree.Pconst_string
                                                            ("@])",
                                                             None)));
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  })]));
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes =
                                           []
                                       }));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               }));
                        Parsetree.pexp_loc =
                          (Pervasives.(!) Ast_helper.default_loc);
                        Parsetree.pexp_attributes = []
                      }
                    | args ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_sequence
                             ({
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_apply
                                    ({
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_ident
                                           {
                                             Asttypes.txt =
                                               (Longident.Ldot
                                                  ((Longident.Lident
                                                      "Format"),
                                                   "fprintf"));
                                             Asttypes.loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc)
                                           });
                                      Parsetree.pexp_loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc);
                                      Parsetree.pexp_attributes = []
                                    },
                                      [(Asttypes.Nolabel,
                                        {
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_ident
                                               {
                                                 Asttypes.txt =
                                                   (Longident.Lident
                                                      "fmt");
                                                 Asttypes.loc =
                                                   (Pervasives.(!)
                                                      Ast_helper.default_loc)
                                               });
                                          Parsetree.pexp_loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc);
                                          Parsetree.pexp_attributes
                                          = []
                                        });
                                       (Asttypes.Nolabel,
                                        (str
                                           ("(@[<2>" ^
                                            (constr_name ^ " (@,"))))]));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!)
                                    Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             },
                               {
                                 Parsetree.pexp_desc =
                                   (Parsetree.Pexp_sequence
                                      ((args |>
                                        (let open Ppx_deriving in
                                         fold_exprs
                                           (seq_reduce
                                              ~sep:{
                                                Parsetree.pexp_desc
                                                =
                                                  (Parsetree.Pexp_apply
                                                     ({
                                                       Parsetree.pexp_desc
                                                       =
                                                         (Parsetree.Pexp_ident
                                                            {
                                                              Asttypes.txt
                                                              =
                                                                (Longident.Ldot
                                                                   ((Longident.Lident
                                                                       "Format"),
                                                                    "fprintf"));
                                                              Asttypes.loc
                                                              =
                                                                (Pervasives.(!)
                                                                   Ast_helper.default_loc)
                                                            });
                                                       Parsetree.pexp_loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                       = []
                                                     },
                                                       [
                                                         (Asttypes.Nolabel,
                                                          {
                                                            Parsetree.pexp_desc
                                                            =
                                                              (Parsetree.Pexp_ident
                                                                 {
                                                                   Asttypes.txt
                                                                   =
                                                                     (Longident.Lident
                                                                        "fmt");
                                                                   Asttypes.loc
                                                                   =
                                                                     (Pervasives.(!)
                                                                        Ast_helper.default_loc)
                                                                 });
                                                            Parsetree.pexp_loc
                                                            =
                                                              (Pervasives.(!)
                                                                 Ast_helper.default_loc);
                                                            Parsetree.pexp_attributes
                                                            = []
                                                          });
                                                         (Asttypes.Nolabel,
                                                          {
                                                            Parsetree.pexp_desc
                                                            =
                                                              (Parsetree.Pexp_constant
                                                                 (Parsetree.Pconst_string
                                                                    (",@ ",
                                                                     None)));
                                                            Parsetree.pexp_loc
                                                            =
                                                              (Pervasives.(!)
                                                                 Ast_helper.default_loc);
                                                            Parsetree.pexp_attributes
                                                            = []
                                                          })]));
                                                Parsetree.pexp_loc
                                                =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              }))),
                                       {
                                         Parsetree.pexp_desc =
                                           (Parsetree.Pexp_apply
                                              ({
                                                Parsetree.pexp_desc
                                                =
                                                  (Parsetree.Pexp_ident
                                                     {
                                                       Asttypes.txt
                                                       =
                                                         (Longident.Ldot
                                                            ((Longident.Lident
                                                                "Format"),
                                                             "fprintf"));
                                                       Asttypes.loc
                                                       =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc)
                                                     });
                                                Parsetree.pexp_loc =
                                                  (Pervasives.(!)
                                                     Ast_helper.default_loc);
                                                Parsetree.pexp_attributes
                                                = []
                                              },
                                                [(Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_ident
                                                         {
                                                           Asttypes.txt
                                                           =
                                                             (Longident.Lident
                                                                "fmt");
                                                           Asttypes.loc
                                                           =
                                                             (Pervasives.(!)
                                                                Ast_helper.default_loc)
                                                         });
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  });
                                                 (Asttypes.Nolabel,
                                                  {
                                                    Parsetree.pexp_desc
                                                    =
                                                      (Parsetree.Pexp_constant
                                                         (Parsetree.Pconst_string
                                                            ("@,))@]",
                                                             None)));
                                                    Parsetree.pexp_loc
                                                    =
                                                      (Pervasives.(!)
                                                         Ast_helper.default_loc);
                                                    Parsetree.pexp_attributes
                                                    = []
                                                  })]));
                                         Parsetree.pexp_loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
                                         Parsetree.pexp_attributes =
                                           []
                                       }));
                                 Parsetree.pexp_loc =
                                   (Pervasives.(!)
                                      Ast_helper.default_loc);
                                 Parsetree.pexp_attributes = []
                               }));
                        Parsetree.pexp_loc =
                          (Pervasives.(!) Ast_helper.default_loc);
                        Parsetree.pexp_attributes = []
                      }
                  in
                  Exp.case (pconstr name' (pattn typs)) printer
                | (None ,Pcstr_record labels) ->
                  let args =
                    labels |>
                    (List.map
                       (fun { pld_name = { txt = n }; pld_type = typ }
                         ->
                           {
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_sequence
                                  ({
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_apply
                                         ({
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_ident
                                                {
                                                  Asttypes.txt =
                                                    (Longident.Ldot
                                                       ((Longident.Lident
                                                           "Format"),
                                                        "fprintf"));
                                                  Asttypes.loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc)
                                                });
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes
                                           = []
                                         },
                                           [(Asttypes.Nolabel,
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_ident
                                                    {
                                                      Asttypes.txt =
                                                        (Longident.Lident
                                                           "fmt");
                                                      Asttypes.loc =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc)
                                                    });
                                               Parsetree.pexp_loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc);
                                               Parsetree.pexp_attributes
                                               = []
                                             });
                                            (Asttypes.Nolabel,
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_constant
                                                    (Parsetree.Pconst_string
                                                       ("@[%s =@ ",
                                                        None)));
                                               Parsetree.pexp_loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc);
                                               Parsetree.pexp_attributes
                                               = []
                                             });
                                            (Asttypes.Nolabel, (str n))]));
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  },
                                    {
                                      Parsetree.pexp_desc =
                                        (Parsetree.Pexp_sequence
                                           ({
                                             Parsetree.pexp_desc =
                                               (Parsetree.Pexp_apply
                                                  ((expr_of_typ quoter
                                                      typ),
                                                   [(Asttypes.Nolabel,
                                                     (evar (argl n)))]));
                                             Parsetree.pexp_loc =
                                               (Pervasives.(!)
                                                  Ast_helper.default_loc);
                                             Parsetree.pexp_attributes
                                             = []
                                           },
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_apply
                                                    ({
                                                      Parsetree.pexp_desc
                                                      =
                                                        (Parsetree.Pexp_ident
                                                           {
                                                             Asttypes.txt
                                                             =
                                                               (Longident.Ldot
                                                                  ((Longident.Lident
                                                                      "Format"),
                                                                   "fprintf"));
                                                             Asttypes.loc
                                                             =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc)
                                                           });
                                                      Parsetree.pexp_loc
                                                      =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc);
                                                      Parsetree.pexp_attributes
                                                      = []
                                                    },
                                                      [(Asttypes.Nolabel,
                                                        {
                                                          Parsetree.pexp_desc
                                                          =
                                                            (Parsetree.Pexp_ident
                                                               {
                                                                 Asttypes.txt
                                                                 =
                                                                   (Longident.Lident
                                                                      "fmt");
                                                                 Asttypes.loc
                                                                 =
                                                                   (Pervasives.(!)
                                                                      Ast_helper.default_loc)
                                                               });
                                                          Parsetree.pexp_loc
                                                          =
                                                            (Pervasives.(!)
                                                               Ast_helper.default_loc);
                                                          Parsetree.pexp_attributes
                                                          = []
                                                        });
                                                       (Asttypes.Nolabel,
                                                        {
                                                          Parsetree.pexp_desc
                                                          =
                                                            (Parsetree.Pexp_constant
                                                               (Parsetree.Pconst_string
                                                                  ("@]",
                                                                   None)));
                                                          Parsetree.pexp_loc
                                                          =
                                                            (Pervasives.(!)
                                                               Ast_helper.default_loc);
                                                          Parsetree.pexp_attributes
                                                          = []
                                                        })]));
                                               Parsetree.pexp_loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc);
                                               Parsetree.pexp_attributes
                                               = []
                                             }));
                                      Parsetree.pexp_loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc);
                                      Parsetree.pexp_attributes = []
                                    }));
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           }))
                  in
                  let printer =
                    {
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_sequence
                           ({
                             Parsetree.pexp_desc =
                               (Parsetree.Pexp_apply
                                  ({
                                    Parsetree.pexp_desc =
                                      (Parsetree.Pexp_ident
                                         {
                                           Asttypes.txt =
                                             (Longident.Ldot
                                                ((Longident.Lident
                                                    "Format"),
                                                 "fprintf"));
                                           Asttypes.loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc)
                                         });
                                    Parsetree.pexp_loc =
                                      (Pervasives.(!)
                                         Ast_helper.default_loc);
                                    Parsetree.pexp_attributes = []
                                  },
                                    [(Asttypes.Nolabel,
                                      {
                                        Parsetree.pexp_desc =
                                          (Parsetree.Pexp_ident
                                             {
                                               Asttypes.txt =
                                                 (Longident.Lident
                                                    "fmt");
                                               Asttypes.loc =
                                                 (Pervasives.(!)
                                                    Ast_helper.default_loc)
                                             });
                                        Parsetree.pexp_loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.pexp_attributes = []
                                      });
                                     (Asttypes.Nolabel,
                                      (str
                                         ("@[<2>" ^
                                          (constr_name ^ " {@,"))))]));
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           },
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_sequence
                                    ((args |>
                                      (let open Ppx_deriving in
                                       fold_exprs
                                         (seq_reduce
                                            ~sep:{
                                              Parsetree.pexp_desc
                                              =
                                                (Parsetree.Pexp_apply
                                                   ({
                                                     Parsetree.pexp_desc
                                                     =
                                                       (Parsetree.Pexp_ident
                                                          {
                                                            Asttypes.txt
                                                            =
                                                              (Longident.Ldot
                                                                 ((Longident.Lident
                                                                     "Format"),
                                                                  "fprintf"));
                                                            Asttypes.loc
                                                            =
                                                              (Pervasives.(!)
                                                                 Ast_helper.default_loc)
                                                          });
                                                     Parsetree.pexp_loc
                                                     =
                                                       (Pervasives.(!)
                                                          Ast_helper.default_loc);
                                                     Parsetree.pexp_attributes
                                                     = []
                                                   },
                                                     [(Asttypes.Nolabel,
                                                       {
                                                         Parsetree.pexp_desc
                                                         =
                                                           (Parsetree.Pexp_ident
                                                              {
                                                                Asttypes.txt
                                                                =
                                                                  (Longident.Lident
                                                                     "fmt");
                                                                Asttypes.loc
                                                                =
                                                                  (Pervasives.(!)
                                                                     Ast_helper.default_loc)
                                                              });
                                                         Parsetree.pexp_loc
                                                         =
                                                           (Pervasives.(!)
                                                              Ast_helper.default_loc);
                                                         Parsetree.pexp_attributes
                                                         = []
                                                       });
                                                      (Asttypes.Nolabel,
                                                       {
                                                         Parsetree.pexp_desc
                                                         =
                                                           (Parsetree.Pexp_constant
                                                              (Parsetree.Pconst_string
                                                                 (";@ ",
                                                                  None)));
                                                         Parsetree.pexp_loc
                                                         =
                                                           (Pervasives.(!)
                                                              Ast_helper.default_loc);
                                                         Parsetree.pexp_attributes
                                                         = []
                                                       })]));
                                              Parsetree.pexp_loc
                                              =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc);
                                              Parsetree.pexp_attributes
                                              = []
                                            }))),
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_apply
                                            ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Ldot
                                                          ((Longident.Lident
                                                              "Format"),
                                                           "fprintf"));
                                                     Asttypes.loc =
                                                       (Pervasives.(!)
                                                          Ast_helper.default_loc)
                                                   });
                                              Parsetree.pexp_loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc);
                                              Parsetree.pexp_attributes
                                              = []
                                            },
                                              [(Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc
                                                  =
                                                    (Parsetree.Pexp_ident
                                                       {
                                                         Asttypes.txt
                                                         =
                                                           (Longident.Lident
                                                              "fmt");
                                                         Asttypes.loc
                                                         =
                                                           (Pervasives.(!)
                                                              Ast_helper.default_loc)
                                                       });
                                                  Parsetree.pexp_loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc);
                                                  Parsetree.pexp_attributes
                                                  = []
                                                });
                                               (Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_constant
                                                       (Parsetree.Pconst_string
                                                          ("@]}", None)));
                                                  Parsetree.pexp_loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc);
                                                  Parsetree.pexp_attributes
                                                  = []
                                                })]));
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes = []
                                     }));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!) Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             }));
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    }  in
                  Exp.case (pconstrrec name' (pattl labels)) printer))
       in
       {
         Parsetree.pexp_desc =
           (Parsetree.Pexp_fun
              (Asttypes.Nolabel, None,
               {
                 Parsetree.ppat_desc =
                   (Parsetree.Ppat_var
                      {
                        Asttypes.txt = "fmt";
                        Asttypes.loc =
                          (Pervasives.(!) Ast_helper.default_loc)
                      });
                 Parsetree.ppat_loc =
                   (Pervasives.(!) Ast_helper.default_loc);
                 Parsetree.ppat_attributes = []
               }, (Exp.function_ cases)));
         Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
         Parsetree.pexp_attributes = []
       }
     | (Ptype_record labels,_) ->
       let fields =
         labels |>
         (List.mapi
            (fun i  ->
               fun
                 { pld_name = { txt = name }; pld_type; pld_attributes }
                 ->
                   let field_name =
                     if i = 0
                     then Ppx_deriving.expand_path ~path name
                     else name  in
                   let pld_type =
                     {
                       pld_type with
                       ptyp_attributes =
                         (pld_attributes @ pld_type.ptyp_attributes)
                     }  in
                   {
                     Parsetree.pexp_desc =
                       (Parsetree.Pexp_sequence
                          ({
                            Parsetree.pexp_desc =
                              (Parsetree.Pexp_apply
                                 ({
                                   Parsetree.pexp_desc =
                                     (Parsetree.Pexp_ident
                                        {
                                          Asttypes.txt =
                                            (Longident.Ldot
                                               ((Longident.Lident
                                                   "Format"), "fprintf"));
                                          Asttypes.loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc)
                                        });
                                   Parsetree.pexp_loc =
                                     (Pervasives.(!)
                                        Ast_helper.default_loc);
                                   Parsetree.pexp_attributes = []
                                 },
                                   [(Asttypes.Nolabel,
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_ident
                                            {
                                              Asttypes.txt =
                                                (Longident.Lident "fmt");
                                              Asttypes.loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc)
                                            });
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes = []
                                     });
                                    (Asttypes.Nolabel,
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_constant
                                            (Parsetree.Pconst_string
                                               ("@[%s =@ ", None)));
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes = []
                                     });
                                    (Asttypes.Nolabel, (str field_name))]));
                            Parsetree.pexp_loc =
                              (Pervasives.(!) Ast_helper.default_loc);
                            Parsetree.pexp_attributes = []
                          },
                            {
                              Parsetree.pexp_desc =
                                (Parsetree.Pexp_sequence
                                   ({
                                     Parsetree.pexp_desc =
                                       (Parsetree.Pexp_apply
                                          ((expr_of_typ quoter pld_type),
                                           [(Asttypes.Nolabel,
                                             (Exp.field (evar "x")
                                                (mknoloc (Lident name))))]));
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!)
                                          Ast_helper.default_loc);
                                     Parsetree.pexp_attributes = []
                                   },
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_apply
                                            ({
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_ident
                                                   {
                                                     Asttypes.txt =
                                                       (Longident.Ldot
                                                          ((Longident.Lident
                                                              "Format"),
                                                           "fprintf"));
                                                     Asttypes.loc =
                                                       (Pervasives.(!)
                                                          Ast_helper.default_loc)
                                                   });
                                              Parsetree.pexp_loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc);
                                              Parsetree.pexp_attributes =
                                                []
                                            },
                                              [(Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_ident
                                                       {
                                                         Asttypes.txt =
                                                           (Longident.Lident
                                                              "fmt");
                                                         Asttypes.loc =
                                                           (Pervasives.(!)
                                                              Ast_helper.default_loc)
                                                       });
                                                  Parsetree.pexp_loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc);
                                                  Parsetree.pexp_attributes
                                                  = []
                                                });
                                               (Asttypes.Nolabel,
                                                {
                                                  Parsetree.pexp_desc =
                                                    (Parsetree.Pexp_constant
                                                       (Parsetree.Pconst_string
                                                          ("@]", None)));
                                                  Parsetree.pexp_loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc);
                                                  Parsetree.pexp_attributes
                                                  = []
                                                })]));
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes = []
                                     }));
                              Parsetree.pexp_loc =
                                (Pervasives.(!) Ast_helper.default_loc);
                              Parsetree.pexp_attributes = []
                            }));
                     Parsetree.pexp_loc =
                       (Pervasives.(!) Ast_helper.default_loc);
                     Parsetree.pexp_attributes = []
                   }))
       in
       {
         Parsetree.pexp_desc =
           (Parsetree.Pexp_fun
              (Asttypes.Nolabel, None,
               {
                 Parsetree.ppat_desc =
                   (Parsetree.Ppat_var
                      {
                        Asttypes.txt = "fmt";
                        Asttypes.loc =
                          (Pervasives.(!) Ast_helper.default_loc)
                      });
                 Parsetree.ppat_loc =
                   (Pervasives.(!) Ast_helper.default_loc);
                 Parsetree.ppat_attributes = []
               },
               {
                 Parsetree.pexp_desc =
                   (Parsetree.Pexp_fun
                      (Asttypes.Nolabel, None,
                       {
                         Parsetree.ppat_desc =
                           (Parsetree.Ppat_var
                              {
                                Asttypes.txt = "x";
                                Asttypes.loc =
                                  (Pervasives.(!) Ast_helper.default_loc)
                              });
                         Parsetree.ppat_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.ppat_attributes = []
                       },
                       {
                         Parsetree.pexp_desc =
                           (Parsetree.Pexp_sequence
                              ({
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_apply
                                     ({
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_ident
                                            {
                                              Asttypes.txt =
                                                (Longident.Ldot
                                                   ((Longident.Lident
                                                       "Format"),
                                                    "fprintf"));
                                              Asttypes.loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc)
                                            });
                                       Parsetree.pexp_loc =
                                         (Pervasives.(!)
                                            Ast_helper.default_loc);
                                       Parsetree.pexp_attributes = []
                                     },
                                       [(Asttypes.Nolabel,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_ident
                                                {
                                                  Asttypes.txt =
                                                    (Longident.Lident
                                                       "fmt");
                                                  Asttypes.loc =
                                                    (Pervasives.(!)
                                                       Ast_helper.default_loc)
                                                });
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes =
                                             []
                                         });
                                        (Asttypes.Nolabel,
                                         {
                                           Parsetree.pexp_desc =
                                             (Parsetree.Pexp_constant
                                                (Parsetree.Pconst_string
                                                   ("@[<2>{ ", None)));
                                           Parsetree.pexp_loc =
                                             (Pervasives.(!)
                                                Ast_helper.default_loc);
                                           Parsetree.pexp_attributes =
                                             []
                                         })]));
                                Parsetree.pexp_loc =
                                  (Pervasives.(!) Ast_helper.default_loc);
                                Parsetree.pexp_attributes = []
                              },
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_sequence
                                       ((fields |>
                                         (let open Ppx_deriving in
                                          fold_exprs
                                            (seq_reduce
                                               ~sep:{
                                                 Parsetree.pexp_desc
                                                 =
                                                   (Parsetree.Pexp_apply
                                                      ({
                                                        Parsetree.pexp_desc
                                                        =
                                                          (Parsetree.Pexp_ident
                                                             {
                                                               Asttypes.txt
                                                               =
                                                                 (Longident.Ldot
                                                                    ((Longident.Lident
                                                                        "Format"),
                                                                     "fprintf"));
                                                               Asttypes.loc
                                                               =
                                                                 (Pervasives.(!)
                                                                    Ast_helper.default_loc)
                                                             });
                                                        Parsetree.pexp_loc
                                                        =
                                                          (Pervasives.(!)
                                                             Ast_helper.default_loc);
                                                        Parsetree.pexp_attributes
                                                        = []
                                                      },
                                                        [
                                                          (Asttypes.Nolabel,
                                                           {
                                                             Parsetree.pexp_desc
                                                             =
                                                               (Parsetree.Pexp_ident
                                                                  {
                                                                    Asttypes.txt
                                                                    =
                                                                      (Longident.Lident
                                                                         "fmt");
                                                                    Asttypes.loc
                                                                    =
                                                                      (Pervasives.(!)
                                                                         Ast_helper.default_loc)
                                                                  });
                                                             Parsetree.pexp_loc
                                                             =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc);
                                                             Parsetree.pexp_attributes
                                                             = []
                                                           });
                                                          (Asttypes.Nolabel,
                                                           {
                                                             Parsetree.pexp_desc
                                                             =
                                                               (Parsetree.Pexp_constant
                                                                  (Parsetree.Pconst_string
                                                                     (";@ ",
                                                                      None)));
                                                             Parsetree.pexp_loc
                                                             =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc);
                                                             Parsetree.pexp_attributes
                                                             = []
                                                           })]));
                                                 Parsetree.pexp_loc
                                                 =
                                                   (Pervasives.(!)
                                                      Ast_helper.default_loc);
                                                 Parsetree.pexp_attributes
                                                 = []
                                               }))),
                                        {
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_apply
                                               ({
                                                 Parsetree.pexp_desc =
                                                   (Parsetree.Pexp_ident
                                                      {
                                                        Asttypes.txt =
                                                          (Longident.Ldot
                                                             ((Longident.Lident
                                                                 "Format"),
                                                              "fprintf"));
                                                        Asttypes.loc =
                                                          (Pervasives.(!)
                                                             Ast_helper.default_loc)
                                                      });
                                                 Parsetree.pexp_loc =
                                                   (Pervasives.(!)
                                                      Ast_helper.default_loc);
                                                 Parsetree.pexp_attributes
                                                 = []
                                               },
                                                 [(Asttypes.Nolabel,
                                                   {
                                                     Parsetree.pexp_desc
                                                     =
                                                       (Parsetree.Pexp_ident
                                                          {
                                                            Asttypes.txt
                                                            =
                                                              (
                                                                Longident.Lident
                                                                  "fmt");
                                                            Asttypes.loc
                                                            =
                                                              (
                                                                Pervasives.(!)
                                                                  Ast_helper.default_loc)
                                                          });
                                                     Parsetree.pexp_loc
                                                     =
                                                       (Pervasives.(!)
                                                          Ast_helper.default_loc);
                                                     Parsetree.pexp_attributes
                                                     = []
                                                   });
                                                  (Asttypes.Nolabel,
                                                   {
                                                     Parsetree.pexp_desc
                                                     =
                                                       (Parsetree.Pexp_constant
                                                          (Parsetree.Pconst_string
                                                             ("@ }@]",
                                                              None)));
                                                     Parsetree.pexp_loc
                                                     =
                                                       (Pervasives.(!)
                                                          Ast_helper.default_loc);
                                                     Parsetree.pexp_attributes
                                                     = []
                                                   })]));
                                          Parsetree.pexp_loc =
                                            (Pervasives.(!)
                                               Ast_helper.default_loc);
                                          Parsetree.pexp_attributes = []
                                        }));
                                  Parsetree.pexp_loc =
                                    (Pervasives.(!)
                                       Ast_helper.default_loc);
                                  Parsetree.pexp_attributes = []
                                }));
                         Parsetree.pexp_loc =
                           (Pervasives.(!) Ast_helper.default_loc);
                         Parsetree.pexp_attributes = []
                       }));
                 Parsetree.pexp_loc =
                   (Pervasives.(!) Ast_helper.default_loc);
                 Parsetree.pexp_attributes = []
               }));
         Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
         Parsetree.pexp_attributes = []
       }
     | (Ptype_abstract ,None ) ->
       raise_errorf ~loc "%s cannot be derived for fully abstract types"
         deriver
     | (Ptype_open ,_) ->
       raise_errorf ~loc "%s cannot be derived for open types" deriver
   in
   let pp_poly_apply =
     Ppx_deriving.poly_apply_of_type_decl type_decl
       (evar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl))
   in
   let stringprinter =
     {
       Parsetree.pexp_desc =
         (Parsetree.Pexp_fun
            (Asttypes.Nolabel, None,
             {
               Parsetree.ppat_desc =
                 (Parsetree.Ppat_var
                    {
                      Asttypes.txt = "x";
                      Asttypes.loc = (Pervasives.(!) Ast_helper.default_loc)
                    });
               Parsetree.ppat_loc = (Pervasives.(!) Ast_helper.default_loc);
               Parsetree.ppat_attributes = []
             },
             {
               Parsetree.pexp_desc =
                 (Parsetree.Pexp_apply
                    ({
                      Parsetree.pexp_desc =
                        (Parsetree.Pexp_ident
                           {
                             Asttypes.txt =
                               (Longident.Ldot
                                  ((Longident.Lident "Format"), "asprintf"));
                             Asttypes.loc =
                               (Pervasives.(!) Ast_helper.default_loc)
                           });
                      Parsetree.pexp_loc =
                        (Pervasives.(!) Ast_helper.default_loc);
                      Parsetree.pexp_attributes = []
                    },
                      [(Asttypes.Nolabel,
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_constant
                               (Parsetree.Pconst_string ("%a", None)));
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        });
                       (Asttypes.Nolabel, pp_poly_apply);
                       (Asttypes.Nolabel,
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_ident
                               {
                                 Asttypes.txt = (Longident.Lident "x");
                                 Asttypes.loc =
                                   (Pervasives.(!) Ast_helper.default_loc)
                               });
                          Parsetree.pexp_loc =
                            (Pervasives.(!) Ast_helper.default_loc);
                          Parsetree.pexp_attributes = []
                        })]));
               Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
               Parsetree.pexp_attributes = []
             }));
       Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
       Parsetree.pexp_attributes = []
     }  in
   let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl  in
   let pp_type =
     Ppx_deriving.strong_type_of_type @@
     (pp_type_of_decl ~options ~path type_decl)
   in
   let show_type =
     Ppx_deriving.strong_type_of_type @@
     (show_type_of_decl ~options ~path type_decl)
   in
   let pp_var = pvar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl)
   in
   let show_var =
     pvar (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl)  in
   [Vb.mk (Pat.constraint_ pp_var pp_type)
      (Ppx_deriving.sanitize ~quoter (polymorphize prettyprinter));
    Vb.mk (Pat.constraint_ show_var show_type) (polymorphize stringprinter)])
