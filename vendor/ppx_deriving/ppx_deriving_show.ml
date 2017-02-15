(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

module Ast_convenience =
  struct
(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(*  It has been trimmed down and adapted to fit the needs of     *)
(*  Facebook by Max Bernstein. Sort of like that little notice   *)
(*  that appears before some films on airplanes.                 *)


open Parsetree
open Asttypes
open Location
open Ast_helper


module Label = struct
  let nolabel = ""
end

let may_tuple ?loc tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc ?attrs:None l)

let lid ?(loc = !default_loc) s = mkloc (Longident.parse s) loc
let constr ?loc ?attrs s args = Exp.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Exp.tuple args)
let nil ?loc ?attrs () = constr ?loc ?attrs "[]" []
let unit ?loc ?attrs () = constr ?loc ?attrs "()" []
let tuple ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | [x] -> x
  | xs -> Exp.tuple ?loc ?attrs xs
let cons ?loc ?attrs hd tl = constr ?loc ?attrs "::" [hd; tl]
let list ?loc ?attrs l = List.fold_right (cons ?loc ?attrs) l (nil ?loc ?attrs ())
let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const_string (s, None))
let int ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let int32 ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let int64 ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
let char ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_char x)
let float ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_float x)
let app ?loc ?attrs f l = if l = [] then f else Exp.apply ?loc ?attrs f (List.map (fun a -> Label.nolabel, a) l)

let pconstr ?loc ?attrs s args = Pat.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Pat.tuple args)
let precord ?loc ?attrs ?(closed = Open) l =
  Pat.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.ppat_loc s, e)) l) closed
let pvar ?(loc = !default_loc) ?attrs s = Pat.var ~loc ?attrs (mkloc s loc)
let punit ?loc ?attrs () = pconstr ?loc ?attrs "()" []
let ptuple ?loc ?attrs = function
  | [] -> punit ?loc ?attrs ()
  | [x] -> x
  | xs -> Pat.tuple ?loc ?attrs xs
let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lid ?loc s)

let find_attr s attrs =
  try Some (snd (List.find (fun (x, _) -> x.txt = s) attrs))
  with Not_found -> None

let has_attr s attrs =
  find_attr s attrs <> None

end

module Ppx_deriving_runtime =
  struct
    module Predef =
      struct
        type _int = int
        type _char = char
        type _string = string
        type _float = float
        type _bool = bool
        type _unit = unit
        type _exn = exn
        type 'a _array = 'a array
        type 'a _list = 'a list
        type 'a _option = 'a option =
          | None
          | Some of 'a
        type _nativeint = nativeint
        type _int32 = int32
        type _int64 = int64
        type 'a _lazy_t = 'a lazy_t
        type _bytes = bytes
      end
    type int = Predef._int
    type char = Predef._char
    type string = Predef._string
    type float = Predef._float
    type bool = Predef._bool
    type unit = Predef._unit
    type exn = Predef._exn
    type 'a array = 'a Predef._array
    type 'a list = 'a Predef._list
    type 'a option = 'a Predef._option =
      | None
      | Some of 'a
    type nativeint = Predef._nativeint
    type int32 = Predef._int32
    type int64 = Predef._int64
    type 'a lazy_t = 'a Predef._lazy_t
    type bytes = Predef._bytes
    module Pervasives = Pervasives
    module Char = Char
    module String = String
    module Printexc = Printexc
    module Array = Array
    module List = List
    module Nativeint = Nativeint
    module Int32 = Int32
    module Int64 = Int64
    module Lazy = Lazy
    module Bytes = Bytes
    module Hashtbl = Hashtbl
    module Queue = Queue
    module Stack = Stack
    module Set = Set
    module Map = Map
    module Weak = Weak
    module Printf = Printf
    module Format = Format
    module Buffer = Buffer
    module Result = Result
    include Pervasives
  end
module Ppx_deriving =
  struct
    open Longident
    open Location
    open Asttypes
    open Parsetree
    open Ast_helper
    open Ast_convenience
    type deriver =
      {
      name: string;
      core_type: (core_type -> expression) option;
      type_decl_str:
        options:(string* expression) list ->
          path:string list -> type_declaration list -> structure;
      type_ext_str:
        options:(string* expression) list ->
          path:string list -> type_extension -> structure;
      module_type_decl_str:
        options:(string* expression) list ->
          path:string list -> module_type_declaration -> structure;
      type_decl_sig:
        options:(string* expression) list ->
          path:string list -> type_declaration list -> signature;
      type_ext_sig:
        options:(string* expression) list ->
          path:string list -> type_extension -> signature;
      module_type_decl_sig:
        options:(string* expression) list ->
          path:string list -> module_type_declaration -> signature;}
    let registry: (string,deriver) Hashtbl.t = Hashtbl.create 16
    let hooks = Queue.create ()
    let add_register_hook f = Queue.add f hooks
    let register d =
      Hashtbl.add registry d.name d; Queue.iter (fun f  -> f d) hooks
    let derivers () =
      Hashtbl.fold (fun _  -> fun v  -> fun acc  -> v :: acc) registry []
    let lookup name =
      try Some (Hashtbl.find registry name) with | Not_found  -> None
    let raise_errorf ?sub  ?if_highlight  ?loc  message =
      message |>
        (Printf.kprintf
           (fun str  ->
              let err = Location.error ?sub ?if_highlight ?loc str in
              raise (Location.Error err)))
    let create =
      let def_ext_str name ~options  ~path  typ_ext =
        raise_errorf
          "Extensible types in structures not supported by deriver %s" name in
      let def_ext_sig name ~options  ~path  typ_ext =
        raise_errorf
          "Extensible types in signatures not supported by deriver %s" name in
      let def_decl_str name ~options  ~path  typ_decl =
        raise_errorf
          "Type declarations in structures not supported by deriver %s" name in
      let def_decl_sig name ~options  ~path  typ_decl =
        raise_errorf
          "Type declaratons in signatures not supported by deriver %s" name in
      let def_module_type_decl_str name ~options  ~path  module_type_decl =
        raise_errorf
          "Module type declarations in structures not supported by deriver %s"
          name in
      let def_module_type_decl_sig name ~options  ~path  module_type_decl =
        raise_errorf
          "Module type declarations in signatures not supported by deriver %s"
          name in
      fun name  ->
        fun ?core_type  ->
          fun ?(type_ext_str= def_ext_str name)  ->
            fun ?(type_ext_sig= def_ext_sig name)  ->
              fun ?(type_decl_str= def_decl_str name)  ->
                fun ?(type_decl_sig= def_decl_sig name)  ->
                  fun ?(module_type_decl_str= def_module_type_decl_str name)
                    ->
                    fun ?(module_type_decl_sig=
                      def_module_type_decl_sig name)  ->
                      fun ()  ->
                        {
                          name;
                          core_type;
                          type_decl_str;
                          type_ext_str;
                          module_type_decl_str;
                          type_decl_sig;
                          type_ext_sig;
                          module_type_decl_sig
                        }
    let string_of_core_type typ =
      Format.asprintf "%a" Pprintast.core_type
        { typ with ptyp_attributes = [] }
    module Arg =
      struct
        type 'a conv = expression -> ('a,string) Result.result
        open Result
        let expr expr = Ok expr
        let int expr =
          match expr with
          | { pexp_desc = Pexp_constant (Const_int n) } -> Ok n
          | _ -> Error "integer"
        let bool expr =
          match expr with
          | {
              Parsetree.pexp_desc = Parsetree.Pexp_construct
                ({ Asttypes.txt = Longident.Lident "true"; Asttypes.loc = _ },None
                 );
              Parsetree.pexp_loc = _; Parsetree.pexp_attributes = _ } ->
              Ok true
          | {
              Parsetree.pexp_desc = Parsetree.Pexp_construct
                ({ Asttypes.txt = Longident.Lident "false"; Asttypes.loc = _
                   },None
                 );
              Parsetree.pexp_loc = _; Parsetree.pexp_attributes = _ } ->
              Ok false
          | _ -> Error "boolean"
        let string expr =
          match expr with
          | { pexp_desc = Pexp_constant (Const_string (n,None )) } -> Ok n
          | _ -> Error "string"
        let char =
          function
          | { pexp_desc = Pexp_constant (Const_char c) } -> Ok c
          | _ -> Error "char"
        let enum values expr =
          match expr with
          | { pexp_desc = Pexp_variant (name,None ) } when
              List.mem name values -> Ok name
          | _ ->
              Error
                (Printf.sprintf "one of: %s"
                   (String.concat ", " (List.map (fun s  -> "`" ^ s) values)))
        let list expr =
          let rec loop acc =
            function
            | {
                Parsetree.pexp_desc = Parsetree.Pexp_construct
                  ({ Asttypes.txt = Longident.Lident "[]"; Asttypes.loc = _ },None
                   );
                Parsetree.pexp_loc = _; Parsetree.pexp_attributes = _ } ->
                Ok (List.rev acc)
            | {
                Parsetree.pexp_desc = Parsetree.Pexp_construct
                  ({ Asttypes.txt = Longident.Lident "::"; Asttypes.loc = _ },Some
                   { Parsetree.pexp_desc = Parsetree.Pexp_tuple (x::xs::[]);
                     Parsetree.pexp_loc = _; Parsetree.pexp_attributes = _ });
                Parsetree.pexp_loc = _; Parsetree.pexp_attributes = _ } ->
                (match expr x with
                 | Ok v -> loop (v :: acc) xs
                 | Error e -> Error ("list:" ^ e))
            | _ -> Error "list" in
          loop []
        let get_attr ~deriver  conv attr =
          match attr with
          | None  -> None
          | Some
              ({ txt = name },PStr ({ pstr_desc = Pstr_eval (expr,[]) }::[]))
              ->
              (match conv expr with
               | Ok v -> Some v
               | Error desc ->
                   raise_errorf ~loc:(expr.pexp_loc)
                     "%s: invalid [@%s]: %s expected" deriver name desc)
          | Some ({ txt = name; loc },_) ->
              raise_errorf ~loc "%s: invalid [@%s]: value expected" deriver
                name
        let get_flag ~deriver  attr =
          match attr with
          | None  -> false
          | Some ({ txt = name },PStr []) -> true
          | Some ({ txt = name; loc },_) ->
              raise_errorf ~loc "%s: invalid [@%s]: empty structure expected"
                deriver name
        let get_expr ~deriver  conv expr =
          match conv expr with
          | Error desc ->
              raise_errorf ~loc:(expr.pexp_loc) "%s: %s expected" deriver
                desc
          | Ok v -> v
      end
    type quoter =
      {
      mutable next_id: int;
      mutable bindings: value_binding list;}
    let create_quoter () = { next_id = 0; bindings = [] }
    let quote ~quoter  expr =
      let name = "__" ^ (string_of_int quoter.next_id) in
      quoter.bindings <-
        (Vb.mk (pvar name)
           {
             Parsetree.pexp_desc =
               (Parsetree.Pexp_fun
                  ("", None,
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
               [("",
                  {
                    Parsetree.pexp_desc =
                      (Parsetree.Pexp_construct
                         ({
                            Asttypes.txt = (Longident.Lident "()");
                            Asttypes.loc =
                              (Pervasives.(!) Ast_helper.default_loc)
                          }, None));
                    Parsetree.pexp_loc =
                      (Pervasives.(!) Ast_helper.default_loc);
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
                                       (Asttypes.Const_string ("-A", None)));
                                  Parsetree.pexp_loc =
                                    (Pervasives.(!) Ast_helper.default_loc);
                                  Parsetree.pexp_attributes = []
                                }, []));
                          Parsetree.pstr_loc =
                            (Pervasives.(!) Ast_helper.default_loc)
                        }]))] Override
          { txt = module_; loc = (!Ast_helper.default_loc) } expr in
      match quoter.bindings with
      | [] -> body
      | bindings -> Exp.let_ Nonrecursive bindings body
    let with_quoter fn a =
      let quoter = create_quoter () in sanitize ~quoter (fn quoter a)
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
          ((String.sub str 0 (String.length prefix)) = prefix) in
      let try_prefix prefix f =
        if List.exists (fun ({ txt },_)  -> starts txt prefix) attrs
        then prefix ^ name
        else f () in
      let name =
        try_prefix ("deriving." ^ (deriver ^ "."))
          (fun ()  -> try_prefix (deriver ^ ".") (fun ()  -> name)) in
      try Some (List.find (fun ({ txt },_)  -> txt = name) attrs)
      with | Not_found  -> None
    let attr_warning expr =
      let loc = !default_loc in
      let structure = { pstr_desc = (Pstr_eval (expr, [])); pstr_loc = loc } in
      ({ txt = "ocaml.warning"; loc }, (PStr [structure]))
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
        let open Ast_mapper in
          (let map_typ mapper typ =
             match typ.ptyp_desc with
             | Ptyp_constr (lid,l) ->
                 let lid = { lid with txt = (remove_pervasive_lid lid.txt) } in
                 {
                   typ with
                   ptyp_desc =
                     (Ptyp_constr (lid, (List.map (mapper.typ mapper) l)))
                 }
             | Ptyp_class (lid,l) ->
                 let lid = { lid with txt = (remove_pervasive_lid lid.txt) } in
                 {
                   typ with
                   ptyp_desc =
                     (Ptyp_class (lid, (List.map (mapper.typ mapper) l)))
                 }
             | _ -> default_mapper.typ mapper typ in
           let m = { default_mapper with typ = map_typ } in m.typ m typ)
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
    let fold_left_type_ext fn accum { ptyext_params } =
      fold_left_type_params fn accum ptyext_params
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
    let fold_right_type_ext fn { ptyext_params } accum =
      fold_right_type_params fn ptyext_params accum
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
        | _ -> assert false in
      let uniq lst =
        let module StringSet = Set.Make(String) in
          (lst |> StringSet.of_list) |> StringSet.elements in
      (free_in typ) |> uniq
    let var_name_of_int i =
      let letter = "abcdefghijklmnopqrstuvwxyz" in
      let rec loop i =
        if i < 26
        then [letter.[i]]
        else (letter.[i mod 26]) :: (loop (i / 26)) in
      String.concat "" (List.map (String.make 1) (loop i))
    let fresh_var bound =
      let rec loop i =
        let var_name = var_name_of_int i in
        if List.mem var_name bound then loop (i + 1) else var_name in
      loop 0
    let poly_fun_of_type_decl type_decl expr =
      fold_right_type_decl
        (fun name  ->
           fun expr  ->
             Exp.fun_ Label.nolabel None (pvar ("poly_" ^ name)) expr)
        type_decl expr
    let poly_fun_of_type_ext type_ext expr =
      fold_right_type_ext
        (fun name  ->
           fun expr  ->
             Exp.fun_ Label.nolabel None (pvar ("poly_" ^ name)) expr)
        type_ext expr
    let poly_apply_of_type_decl type_decl expr =
      fold_left_type_decl
        (fun expr  ->
           fun name  ->
             Exp.apply expr [(Label.nolabel, (evar ("poly_" ^ name)))]) expr
        type_decl
    let poly_apply_of_type_ext type_ext expr =
      fold_left_type_ext
        (fun expr  ->
           fun name  ->
             Exp.apply expr [(Label.nolabel, (evar ("poly_" ^ name)))]) expr
        type_ext
    let poly_arrow_of_type_decl fn type_decl typ =
      fold_right_type_decl
        (fun name  ->
           fun typ  -> Typ.arrow Label.nolabel (fn (Typ.var name)) typ)
        type_decl typ
    let poly_arrow_of_type_ext fn type_ext typ =
      fold_right_type_ext
        (fun name  ->
           fun typ  -> Typ.arrow Label.nolabel (fn (Typ.var name)) typ)
        type_ext typ
    let core_type_of_type_decl { ptype_name = { txt = name }; ptype_params }
      = Typ.constr (mknoloc (Lident name)) (List.map fst ptype_params)
    let core_type_of_type_ext { ptyext_path; ptyext_params } =
      Typ.constr ptyext_path (List.map fst ptyext_params)
    let instantiate bound type_decl =
      let (vars,bound) =
        List.fold_right
          (fun _  ->
             fun (vars,bound)  ->
               let v = fresh_var bound in ((v :: vars), (v :: bound)))
          (free_vars_in_core_type (core_type_of_type_decl type_decl))
          ([], bound) in
      let vars = List.rev vars in
      let core_type =
        core_type_of_type_decl
          {
            type_decl with
            ptype_params =
              (List.map2
                 (fun v  -> fun (_,variance)  -> ((Typ.var v), variance))
                 vars type_decl.ptype_params)
          } in
      (core_type, vars, bound)
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
                     Parsetree.pexp_loc =
                       (Pervasives.(!) Ast_helper.default_loc);
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
    let binop_reduce x a b =
      {
        Parsetree.pexp_desc = (Parsetree.Pexp_apply (x, [("", a); ("", b)]));
        Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
        Parsetree.pexp_attributes = []
      }
    let strong_type_of_type ty =
      let free_vars = free_vars_in_core_type ty in
      Typ.force_poly @@ (Typ.poly free_vars ty)
    let derive path pstr_loc item attributes fn arg =
      let deriving = find_attr "deriving" attributes in
      let (deriver_exprs,loc) =
        match deriving with
        | Some (PStr
            ({ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple exprs },[]);
               pstr_loc }::[]))
            -> (exprs, pstr_loc)
        | Some (PStr
            ({
               pstr_desc = Pstr_eval
                 (({ pexp_desc = (Pexp_ident _|Pexp_apply _) } as expr),[]);
               pstr_loc }::[]))
            -> ([expr], pstr_loc)
        | _ ->
            raise_errorf ~loc:pstr_loc
              "Unrecognized [@@deriving] annotation syntax" in
      List.fold_left
        (fun items  ->
           fun deriver_expr  ->
             let (name,options) =
               match deriver_expr with
               | { pexp_desc = Pexp_ident name } -> (name, [])
               | {
                   pexp_desc = Pexp_apply
                     ({ pexp_desc = Pexp_ident name },(label,{
                                                               pexp_desc =
                                                                 Pexp_record
                                                                 (options,None
                                                                  )
                                                               })::[])
                   } when label = Label.nolabel ->
                   (name,
                     (options |>
                        (List.map
                           (fun ({ txt },expr)  ->
                              ((String.concat "." (Longident.flatten txt)),
                                expr)))))
               | { pexp_loc } ->
                   raise_errorf ~loc:pexp_loc
                     "Unrecognized [@@deriving] option syntax" in
             let (name,loc) =
               ((String.concat "_" (Longident.flatten name.txt)), (name.loc)) in
             let (is_optional,options) =
               match List.assoc "optional" options with
               | exception Not_found  -> (false, options)
               | expr ->
                   (((let open Arg in get_expr ~deriver:name bool) expr),
                     (List.remove_assoc "optional" options)) in
             match lookup name with
             | Some deriver ->
                 items @ ((fn deriver) ~options ~path:(!path) arg)
             | None  ->
                 if is_optional
                 then items
                 else raise_errorf ~loc "Cannot locate deriver %s" name)
        [item] deriver_exprs
    let derive_type_decl path typ_decls pstr_loc item fn =
      let attributes =
        List.concat
          (List.map (fun { ptype_attributes = attrs }  -> attrs) typ_decls) in
      derive path pstr_loc item attributes fn typ_decls
    let derive_type_ext path typ_ext pstr_loc item fn =
      let attributes = typ_ext.ptyext_attributes in
      derive path pstr_loc item attributes fn typ_ext
    let derive_module_type_decl path module_type_decl pstr_loc item fn =
      let attributes = module_type_decl.pmtd_attributes in
      derive path pstr_loc item attributes fn module_type_decl
    let module_from_input_name () =
      match !Location.input_name with
      | "//toplevel//" -> []
      | filename ->
          [String.capitalize
             (let open Filename in basename (chop_suffix filename ".ml"))]
    let pstr_desc_rec_flag pstr =
      match pstr with
      | Pstr_type typ_decls ->
          if
            List.exists (fun ty  -> has_attr "nonrec" ty.ptype_attributes)
              typ_decls
          then Nonrecursive
          else Recursive
      | _ -> assert false
    let mapper =
      let module_nesting = ref [] in
      let with_module name f =
        let old_nesting = !module_nesting in
        module_nesting := ((!module_nesting) @ [name]);
        (let result = f () in module_nesting := old_nesting; result) in
      let expression mapper expr =
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = name; loc },payload) } when
            let open String in
              ((length name) >= 7) && ((sub name 0 7) = "derive.")
            ->
            let name = String.sub name 7 ((String.length name) - 7) in
            let deriver =
              match lookup name with
              | Some { core_type = Some deriver } -> deriver
              | Some _ ->
                  raise_errorf ~loc
                    "Deriver %s does not support inline notation" name
              | None  -> raise_errorf ~loc "Cannot locate deriver %s" name in
            (match payload with
             | PTyp typ -> deriver typ
             | _ -> raise_errorf ~loc "Unrecognized [%%derive.*] syntax")
        | { pexp_desc = Pexp_extension ({ txt = name; loc },PTyp typ) } ->
            (match lookup name with
             | Some { core_type = Some deriver } ->
                 Ast_helper.with_default_loc typ.ptyp_loc
                   (fun ()  -> deriver typ)
             | _ -> (let open Ast_mapper in default_mapper.expr) mapper expr)
        | _ -> (let open Ast_mapper in default_mapper.expr) mapper expr in
      let structure mapper items =
        match items with
        | { pstr_desc = (Pstr_type typ_decls as pstr_desc); pstr_loc }::rest
            when
            (List.exists (fun ty  -> has_attr "deriving" ty.ptype_attributes)
               typ_decls)
              && ((pstr_desc_rec_flag pstr_desc) = Nonrecursive)
            ->
            raise_errorf ~loc:pstr_loc
              "The nonrec flag is not supported by ppx_deriving"
        | ({ pstr_desc = Pstr_type typ_decls; pstr_loc } as item)::rest when
            List.exists (fun ty  -> has_attr "deriving" ty.ptype_attributes)
              typ_decls
            ->
            let derived =
              Ast_helper.with_default_loc pstr_loc
                (fun ()  ->
                   derive_type_decl module_nesting typ_decls pstr_loc item
                     (fun deriver  -> deriver.type_decl_str)) in
            derived @ (mapper.Ast_mapper.structure mapper rest)
        | ({ pstr_desc = Pstr_typext typ_ext; pstr_loc } as item)::rest when
            has_attr "deriving" typ_ext.ptyext_attributes ->
            let derived =
              Ast_helper.with_default_loc pstr_loc
                (fun ()  ->
                   derive_type_ext module_nesting typ_ext pstr_loc item
                     (fun deriver  -> deriver.type_ext_str)) in
            derived @ (mapper.Ast_mapper.structure mapper rest)
        | ({ pstr_desc = Pstr_modtype modtype; pstr_loc } as item)::rest when
            has_attr "deriving" modtype.pmtd_attributes ->
            let derived =
              Ast_helper.with_default_loc pstr_loc
                (fun ()  ->
                   derive_module_type_decl module_nesting modtype pstr_loc
                     item (fun deriver  -> deriver.module_type_decl_str)) in
            derived @ (mapper.Ast_mapper.structure mapper rest)
        | ({ pstr_desc = Pstr_module ({ pmb_name = { txt = name } } as mb) }
             as item)::rest
            ->
            let derived =
              {
                item with
                pstr_desc =
                  (Pstr_module
                     (with_module name
                        (fun ()  ->
                           mapper.Ast_mapper.module_binding mapper mb)))
              } in
            derived :: (mapper.Ast_mapper.structure mapper rest)
        | ({ pstr_desc = Pstr_recmodule mbs } as item)::rest ->
            let derived =
              {
                item with
                pstr_desc =
                  (Pstr_recmodule
                     (mbs |>
                        (List.map
                           (fun ({ pmb_name = { txt = name } } as mb)  ->
                              with_module name
                                (fun ()  ->
                                   mapper.Ast_mapper.module_binding mapper mb)))))
              } in
            derived :: (mapper.Ast_mapper.structure mapper rest)
        | ({ pstr_loc } as item)::rest ->
            let derived = mapper.Ast_mapper.structure_item mapper item in
            derived :: (mapper.Ast_mapper.structure mapper rest)
        | [] -> [] in
      let signature mapper items =
        match items with
        | ({ psig_desc = Psig_type typ_decls; psig_loc } as item)::rest when
            List.exists (fun ty  -> has_attr "deriving" ty.ptype_attributes)
              typ_decls
            ->
            let derived =
              Ast_helper.with_default_loc psig_loc
                (fun ()  ->
                   derive_type_decl module_nesting typ_decls psig_loc item
                     (fun deriver  -> deriver.type_decl_sig)) in
            derived @ (mapper.Ast_mapper.signature mapper rest)
        | ({ psig_desc = Psig_typext typ_ext; psig_loc } as item)::rest when
            has_attr "deriving" typ_ext.ptyext_attributes ->
            let derived =
              Ast_helper.with_default_loc psig_loc
                (fun ()  ->
                   derive_type_ext module_nesting typ_ext psig_loc item
                     (fun deriver  -> deriver.type_ext_sig)) in
            derived @ (mapper.Ast_mapper.signature mapper rest)
        | ({ psig_desc = Psig_modtype modtype; psig_loc } as item)::rest when
            has_attr "deriving" modtype.pmtd_attributes ->
            let derived =
              Ast_helper.with_default_loc psig_loc
                (fun ()  ->
                   derive_module_type_decl module_nesting modtype psig_loc
                     item (fun deriver  -> deriver.module_type_decl_sig)) in
            derived @ (mapper.Ast_mapper.signature mapper rest)
        | ({ psig_desc = Psig_module ({ pmd_name = { txt = name } } as md) }
             as item)::rest
            ->
            let derived =
              {
                item with
                psig_desc =
                  (Psig_module
                     (with_module name
                        (fun ()  ->
                           mapper.Ast_mapper.module_declaration mapper md)))
              } in
            derived :: (mapper.Ast_mapper.signature mapper rest)
        | ({ psig_desc = Psig_recmodule mds } as item)::rest ->
            let derived =
              {
                item with
                psig_desc =
                  (Psig_recmodule
                     (mds |>
                        (List.map
                           (fun ({ pmd_name = { txt = name } } as md)  ->
                              with_module name
                                (fun ()  ->
                                   mapper.Ast_mapper.module_declaration
                                     mapper md)))))
              } in
            derived :: (mapper.Ast_mapper.signature mapper rest)
        | ({ psig_loc } as item)::rest ->
            let derived = mapper.Ast_mapper.signature_item mapper item in
            derived :: (mapper.Ast_mapper.signature mapper rest)
        | [] -> [] in
      let open Ast_mapper in
        {
          default_mapper with
          expr = expression;
          structure =
            (fun mapper  ->
               fun items  ->
                 module_nesting := (module_from_input_name ());
                 structure { mapper with structure; signature } items);
          signature =
            (fun mapper  ->
               fun items  ->
                 module_nesting := (module_from_input_name ());
                 signature { mapper with structure; signature } items)
        }
    let hash_variant s =
      let accu = ref 0 in
      for i = 0 to (String.length s) - 1 do
        accu := ((223 * (!accu)) + (Char.code (s.[i])))
      done;
      accu := ((!accu) land ((1 lsl 31) - 1));
      if (!accu) > 1073741823 then (!accu) - (1 lsl 31) else !accu
  end

    open Longident
    open Location
    open Asttypes
    open Parsetree
    open Ast_helper
    open Ast_convenience
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
      List.map (fun { pld_name = { txt = n } }  -> (n, (pvar (argl n))))
        labels
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
                    Parsetree.pvb_loc =
                      (Pervasives.(!) Ast_helper.default_loc)
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
                                  (Asttypes.Const_string ("-26", None)));
                             Parsetree.pexp_loc =
                               (Pervasives.(!) Ast_helper.default_loc);
                             Parsetree.pexp_attributes = []
                           }, []));
                     Parsetree.pstr_loc =
                       (Pervasives.(!) Ast_helper.default_loc)
                   }]))]
        }
    let pp_type_of_decl ~options  ~path  type_decl =
      parse_options options;
      (let typ = Ppx_deriving.core_type_of_type_decl type_decl in
       Ppx_deriving.poly_arrow_of_type_decl
         (fun var  ->
            {
              Parsetree.ptyp_desc =
                (Parsetree.Ptyp_arrow
                   ("",
                     {
                       Parsetree.ptyp_desc =
                         (Parsetree.Ptyp_constr
                            ({
                               Asttypes.txt =
                                 (Longident.Ldot
                                    ((Longident.Lident "Format"),
                                      "formatter"));
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
                            ("", var,
                              {
                                Parsetree.ptyp_desc =
                                  (Parsetree.Ptyp_constr
                                     ({
                                        Asttypes.txt =
                                          (Longident.Ldot
                                             ((Longident.Lident
                                                 "Ppx_deriving_runtime"),
                                               "unit"));
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
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
                ("",
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
                         ("", typ,
                           {
                             Parsetree.ptyp_desc =
                               (Parsetree.Ptyp_constr
                                  ({
                                     Asttypes.txt =
                                       (Longident.Ldot
                                          ((Longident.Lident
                                              "Ppx_deriving_runtime"),
                                            "unit"));
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
         })
    let show_type_of_decl ~options  ~path  type_decl =
      parse_options options;
      (let typ = Ppx_deriving.core_type_of_type_decl type_decl in
       Ppx_deriving.poly_arrow_of_type_decl
         (fun var  ->
            {
              Parsetree.ptyp_desc =
                (Parsetree.Ptyp_arrow
                   ("",
                     {
                       Parsetree.ptyp_desc =
                         (Parsetree.Ptyp_constr
                            ({
                               Asttypes.txt =
                                 (Longident.Ldot
                                    ((Longident.Lident "Format"),
                                      "formatter"));
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
                            ("", var,
                              {
                                Parsetree.ptyp_desc =
                                  (Parsetree.Ptyp_constr
                                     ({
                                        Asttypes.txt =
                                          (Longident.Ldot
                                             ((Longident.Lident
                                                 "Ppx_deriving_runtime"),
                                               "unit"));
                                        Asttypes.loc =
                                          (Pervasives.(!)
                                             Ast_helper.default_loc)
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
                ("", typ,
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
                    Parsetree.ptyp_loc =
                      (Pervasives.(!) Ast_helper.default_loc);
                    Parsetree.ptyp_attributes = []
                  }));
           Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
           Parsetree.ptyp_attributes = []
         })
    let sig_of_type ~options  ~path  type_decl =
      parse_options options;
      [Sig.value
         (Val.mk
            (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl))
            (pp_type_of_decl ~options ~path type_decl));
      Sig.value
        (Val.mk
           (mknoloc
              (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl))
           (show_type_of_decl ~options ~path type_decl))]
    let rec expr_of_typ quoter typ =
      let expr_of_typ = expr_of_typ quoter in
      match attr_printer typ.ptyp_attributes with
      | Some printer ->
          {
            Parsetree.pexp_desc =
              (Parsetree.Pexp_apply
                 ((wrap_printer quoter printer),
                   [("",
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
                   ("", None,
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
                              [("",
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
                              ("",
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_constant
                                       (Asttypes.Const_string
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
                        [("",
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
                        ("", (str x))]));
                 Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
                 Parsetree.pexp_attributes = []
               } in
             let seq start finish fold typ =
               {
                 Parsetree.pexp_desc =
                   (Parsetree.Pexp_fun
                      ("", None,
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
                                         [("",
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
                                         ("", (str start))]));
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
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [("",
                                                     {
                                                       Parsetree.pexp_desc =
                                                         (Parsetree.Pexp_apply
                                                            (fold,
                                                              [("",
                                                                 {
                                                                   Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_fun
                                                                    ("",
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
                                                                    ("",
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
                                                                    ("",
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
                                                                    ("",
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                                                    ("",
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
                                                              ("",
                                                                {
                                                                  Parsetree.pexp_desc
                                                                    =
                                                                    (
                                                                    Parsetree.Pexp_construct
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
                                                                    (
                                                                    Pervasives.(!)
                                                                    Ast_helper.default_loc);
                                                                  Parsetree.pexp_attributes
                                                                    = []
                                                                });
                                                              ("",
                                                                {
                                                                  Parsetree.pexp_desc
                                                                    =
                                                                    (
                                                                    Parsetree.Pexp_ident
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
                                                                    (
                                                                    Pervasives.(!)
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
                                                    Parsetree.pexp_attributes
                                                      = []
                                                  },
                                                   [("",
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
                                                   ("", (str finish))]));
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
               } in
             let typ = Ppx_deriving.remove_pervasives ~deriver typ in
             match typ with
             | { Parsetree.ptyp_desc = Parsetree.Ptyp_any ;
                 Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _ } ->
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_fun
                        ("", None,
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
                                   [("",
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
                                   ("",
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_constant
                                            (Asttypes.Const_string
                                               ("_", None)));
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
             | { ptyp_desc = Ptyp_arrow _ } ->
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_fun
                        ("", None,
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
                                   [("",
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
                                   ("",
                                     {
                                       Parsetree.pexp_desc =
                                         (Parsetree.Pexp_constant
                                            (Asttypes.Const_string
                                               ("<fun>", None)));
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
             | { ptyp_desc = Ptyp_constr _ } ->
                 let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
                 (match (builtin, typ) with
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "unit";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_fun
                             ("", None,
                               {
                                 Parsetree.ppat_desc =
                                   (Parsetree.Ppat_construct
                                      ({
                                         Asttypes.txt =
                                           (Longident.Lident "()");
                                         Asttypes.loc =
                                           (Pervasives.(!)
                                              Ast_helper.default_loc)
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
                                        [("",
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
                                        ("",
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_constant
                                                 (Asttypes.Const_string
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
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      -> format "%d"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "int32";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                    |(true
                      ,{
                         Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                           ({
                              Asttypes.txt = Longident.Ldot
                                (Longident.Lident "Int32","t");
                              Asttypes.loc = _ },[]);
                         Parsetree.ptyp_loc = _;
                         Parsetree.ptyp_attributes = _ })
                      -> format "%ldl"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "int64";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                    |(true
                      ,{
                         Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                           ({
                              Asttypes.txt = Longident.Ldot
                                (Longident.Lident "Int64","t");
                              Asttypes.loc = _ },[]);
                         Parsetree.ptyp_loc = _;
                         Parsetree.ptyp_attributes = _ })
                      -> format "%LdL"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "nativeint";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                    |(true
                      ,{
                         Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                           ({
                              Asttypes.txt = Longident.Ldot
                                (Longident.Lident "Nativeint","t");
                              Asttypes.loc = _ },[]);
                         Parsetree.ptyp_loc = _;
                         Parsetree.ptyp_attributes = _ })
                      -> format "%ndn"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "float";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      -> format "%F"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "bool";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      -> format "%B"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "char";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      -> format "%C"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "string";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                    |(true
                      ,{
                         Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                           ({
                              Asttypes.txt = Longident.Ldot
                                (Longident.Lident "String","t");
                              Asttypes.loc = _ },[]);
                         Parsetree.ptyp_loc = _;
                         Parsetree.ptyp_attributes = _ })
                      -> format "%S"
                  | (true
                     ,{
                        Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                          ({ Asttypes.txt = Longident.Lident "bytes";
                             Asttypes.loc = _ },[]);
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                    |(true
                      ,{
                         Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                           ({
                              Asttypes.txt = Longident.Ldot
                                (Longident.Lident "Bytes","t");
                              Asttypes.loc = _ },[]);
                         Parsetree.ptyp_loc = _;
                         Parsetree.ptyp_attributes = _ })
                      ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_fun
                             ("", None,
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
                                         Parsetree.pexp_attributes = []
                                       },
                                        [("",
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
                                        ("",
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_constant
                                                 (Asttypes.Const_string
                                                    ("%S", None)));
                                            Parsetree.pexp_loc =
                                              (Pervasives.(!)
                                                 Ast_helper.default_loc);
                                            Parsetree.pexp_attributes = []
                                          });
                                        ("",
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
                                                    Parsetree.pexp_attributes
                                                      = []
                                                  },
                                                   [("",
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
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_fun
                             ("", None,
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
                                                 Parsetree.pexp_attributes =
                                                   []
                                               },
                                                [("",
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
                                                ("",
                                                  {
                                                    Parsetree.pexp_desc =
                                                      (Parsetree.Pexp_constant
                                                         (Asttypes.Const_string
                                                            ("ref (", None)));
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
                                                       ((expr_of_typ typ),
                                                         [("",
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
                                                                    (Longident.Lident
                                                                    "!");
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
                                                                    ("",
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
                                                                    (
                                                                    Longident.Ldot
                                                                    ((Longident.Lident
                                                                    "Format"),
                                                                    "pp_print_string"));
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
                                                         },
                                                          [("",
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
                                                          ("",
                                                            {
                                                              Parsetree.pexp_desc
                                                                =
                                                                (Parsetree.Pexp_constant
                                                                   (Asttypes.Const_string
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
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      ->
                      seq "@[<2>[" "@,]@]"
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_ident
                               {
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident "List"),
                                        "fold_left"));
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
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
                      ->
                      seq "@[<2>[|" "@,|]@]"
                        {
                          Parsetree.pexp_desc =
                            (Parsetree.Pexp_ident
                               {
                                 Asttypes.txt =
                                   (Longident.Ldot
                                      ((Longident.Lident "Array"),
                                        "fold_left"));
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
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
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
                                           [("",
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
                                           ("",
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_constant
                                                    (Asttypes.Const_string
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
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [("",
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
                                                  ("",
                                                    {
                                                      Parsetree.pexp_desc =
                                                        (Parsetree.Pexp_constant
                                                           (Asttypes.Const_string
                                                              ("(Some ",
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
                                         },
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_sequence
                                                 ({
                                                    Parsetree.pexp_desc =
                                                      (Parsetree.Pexp_apply
                                                         ((expr_of_typ typ),
                                                           [("",
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
                                                            [("",
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
                                                            ("",
                                                              {
                                                                Parsetree.pexp_desc
                                                                  =
                                                                  (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                        Parsetree.ptyp_loc = _; Parsetree.ptyp_attributes = _
                        })
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
                                                Parsetree.ppat_attributes =
                                                  []
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
                                                    Parsetree.pexp_attributes
                                                      = []
                                                  },
                                                   [("",
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
                                                   ("",
                                                     {
                                                       Parsetree.pexp_desc =
                                                         (Parsetree.Pexp_constant
                                                            (Asttypes.Const_string
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
                                                            [("",
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
                                                             [("",
                                                                {
                                                                  Parsetree.pexp_desc
                                                                    =
                                                                    (
                                                                    Parsetree.Pexp_ident
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
                                                                    (
                                                                    Pervasives.(!)
                                                                    Ast_helper.default_loc);
                                                                  Parsetree.pexp_attributes
                                                                    = []
                                                                });
                                                             ("",
                                                               {
                                                                 Parsetree.pexp_desc
                                                                   =
                                                                   (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [("",
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
                                                  ("",
                                                    {
                                                      Parsetree.pexp_desc =
                                                        (Parsetree.Pexp_constant
                                                           (Asttypes.Const_string
                                                              ("(Error ",
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
                                         },
                                          {
                                            Parsetree.pexp_desc =
                                              (Parsetree.Pexp_sequence
                                                 ({
                                                    Parsetree.pexp_desc =
                                                      (Parsetree.Pexp_apply
                                                         ((expr_of_typ err_t),
                                                           [("",
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
                                                            [("",
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
                                                            ("",
                                                              {
                                                                Parsetree.pexp_desc
                                                                  =
                                                                  (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                         Parsetree.ptyp_loc = _;
                         Parsetree.ptyp_attributes = _ }
                       |{
                          Parsetree.ptyp_desc = Parsetree.Ptyp_constr
                            ({
                               Asttypes.txt = Longident.Ldot
                                 (Longident.Lident "Lazy","t");
                               Asttypes.loc = _ },typ::[]);
                          Parsetree.ptyp_loc = _;
                          Parsetree.ptyp_attributes = _ }))
                      ->
                      {
                        Parsetree.pexp_desc =
                          (Parsetree.Pexp_fun
                             ("", None,
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
                                                 Parsetree.pexp_attributes =
                                                   []
                                               },
                                                [("",
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
                                       },
                                        {
                                          Parsetree.pexp_desc =
                                            (Parsetree.Pexp_apply
                                               ((expr_of_typ typ),
                                                 [("",
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
                                                             [("",
                                                                {
                                                                  Parsetree.pexp_desc
                                                                    =
                                                                    (
                                                                    Parsetree.Pexp_ident
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
                                                                    (
                                                                    Pervasives.(!)
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
                                                     Parsetree.pexp_attributes
                                                       = []
                                                   },
                                                    [("",
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
                                                    ("",
                                                      {
                                                        Parsetree.pexp_desc =
                                                          (Parsetree.Pexp_constant
                                                             (Asttypes.Const_string
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
                                    ("", None,
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
                                          (Pervasives.(!)
                                             Ast_helper.default_loc);
                                        Parsetree.ppat_attributes = []
                                      }, (expr_of_typ typ)));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!) Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             }) args in
                      let printer =
                        match attr_polyprinter typ.ptyp_attributes with
                        | Some printer -> wrap_printer quoter printer
                        | None  ->
                            let printer =
                              Exp.ident
                                (mknoloc
                                   (Ppx_deriving.mangle_lid (`Prefix "pp")
                                      lid)) in
                            Ppx_deriving.quote quoter printer in
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
                     (fun i  ->
                        fun typ  -> app (expr_of_typ typ) [evar (argn i)])
                     typs in
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_fun
                        ("", None,
                          (ptuple
                             (List.mapi (fun i  -> fun _  -> pvar (argn i))
                                typs)),
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
                                           [("",
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
                                           ("",
                                             {
                                               Parsetree.pexp_desc =
                                                 (Parsetree.Pexp_constant
                                                    (Asttypes.Const_string
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
                                                                    [
                                                                    ("",
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
                                                                    ("",
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                                     [("",
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
                                                     ("",
                                                       {
                                                         Parsetree.pexp_desc
                                                           =
                                                           (Parsetree.Pexp_constant
                                                              (Asttypes.Const_string
                                                                 ("@])",
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
                                            }));
                                     Parsetree.pexp_loc =
                                       (Pervasives.(!) Ast_helper.default_loc);
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
                                          [("",
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
                                          ("", (str ("`" ^ label)))]));
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
                                           (Pervasives.(!)
                                              Ast_helper.default_loc);
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
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [("",
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
                                                  ("",
                                                    (str
                                                       ("`" ^
                                                          (label ^
                                                             " (@[<hov>"))))]));
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
                                                           [("",
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
                                                            [("",
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
                                                            ("",
                                                              {
                                                                Parsetree.pexp_desc
                                                                  =
                                                                  (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                 }
                           | Rinherit
                               ({ ptyp_desc = Ptyp_constr (tname,_) } as typ)
                               ->
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
                                          [("",
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
                                 (Ppx_deriving.string_of_core_type typ))) in
                 Exp.function_ cases
             | { ptyp_desc = Ptyp_var name } ->
                 {
                   Parsetree.pexp_desc =
                     (Parsetree.Pexp_apply
                        ((evar ("poly_" ^ name)),
                          [("",
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
                             })]));
                   Parsetree.pexp_loc =
                     (Pervasives.(!) Ast_helper.default_loc);
                   Parsetree.pexp_attributes = []
                 }
             | { ptyp_desc = Ptyp_alias (typ,_) } -> expr_of_typ typ
             | { ptyp_loc } ->
                 raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ))
    let str_of_type ~options  ~path  ({ ptype_loc = loc } as type_decl) =
      parse_options options;
      (let quoter = Ppx_deriving.create_quoter () in
       let path = Ppx_deriving.path_of_type_decl ~path type_decl in
       let prettyprinter =
         match ((type_decl.ptype_kind), (type_decl.ptype_manifest)) with
         | (Ptype_abstract ,Some manifest) ->
             {
               Parsetree.pexp_desc =
                 (Parsetree.Pexp_fun
                    ("", None,
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
                    (fun
                       { pcd_name = { txt = name' }; pcd_args; pcd_attributes
                         }
                        ->
                       let constr_name = Ppx_deriving.expand_path ~path name' in
                       match ((attr_printer pcd_attributes), pcd_args) with
                       | (Some printer,args) ->
                           let rec range from_idx to_idx =
                             if from_idx = to_idx
                             then []
                             else from_idx :: (range (from_idx + 1) to_idx) in
                           let indices = range 0 (List.length args) in
                           let pattern_vars =
                             List.map
                               (fun i  -> pvar ("a" ^ (string_of_int i)))
                               indices in
                           let expr_vars =
                             List.map
                               (fun i  -> evar ("a" ^ (string_of_int i)))
                               indices in
                           Exp.case (pconstr name' pattern_vars)
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_apply
                                    ((wrap_printer quoter printer),
                                      [("",
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
                                      ("", (tuple expr_vars))]));
                               Parsetree.pexp_loc =
                                 (Pervasives.(!) Ast_helper.default_loc);
                               Parsetree.pexp_attributes = []
                             }
                       | (None ,typs) ->
                           let args =
                             List.mapi
                               (fun i  ->
                                  fun typ  ->
                                    app (expr_of_typ quoter typ)
                                      [evar (argn i)]) typs in
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
                                          [("",
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
                                          ("", (str constr_name))]));
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
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [("",
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
                                                  ("",
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
                                                             Parsetree.pexp_loc
                                                               =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc);
                                                             Parsetree.pexp_attributes
                                                               = []
                                                           },
                                                            [("",
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
                                                            ("",
                                                              {
                                                                Parsetree.pexp_desc
                                                                  =
                                                                  (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                                   Parsetree.pexp_attributes
                                                     = []
                                                 },
                                                  [("",
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
                                                  ("",
                                                    (str
                                                       ("(@[<2>" ^
                                                          (constr_name ^
                                                             " (@,"))))]));
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
                                                                    ("",
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
                                                                    ("",
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                                             Parsetree.pexp_loc
                                                               =
                                                               (Pervasives.(!)
                                                                  Ast_helper.default_loc);
                                                             Parsetree.pexp_attributes
                                                               = []
                                                           },
                                                            [("",
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
                                                            ("",
                                                              {
                                                                Parsetree.pexp_desc
                                                                  =
                                                                  (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                 } in
                           Exp.case (pconstr name' (pattn typs)) printer)) in
             {
               Parsetree.pexp_desc =
                 (Parsetree.Pexp_fun
                    ("", None,
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
                         { pld_name = { txt = name }; pld_type;
                           pld_attributes }
                          ->
                         let field_name =
                           if i = 0
                           then Ppx_deriving.expand_path ~path name
                           else name in
                         let pld_type =
                           {
                             pld_type with
                             ptyp_attributes =
                               (pld_attributes @ pld_type.ptyp_attributes)
                           } in
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
                                          [("",
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
                                          ("",
                                            {
                                              Parsetree.pexp_desc =
                                                (Parsetree.Pexp_constant
                                                   (Asttypes.Const_string
                                                      ("@[%s =@ ", None)));
                                              Parsetree.pexp_loc =
                                                (Pervasives.(!)
                                                   Ast_helper.default_loc);
                                              Parsetree.pexp_attributes = []
                                            });
                                          ("", (str field_name))]));
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
                                                 ((expr_of_typ quoter
                                                     pld_type),
                                                   [("",
                                                      (Exp.field (evar "x")
                                                         (mknoloc
                                                            (Lident name))))]));
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
                                                     Parsetree.pexp_attributes
                                                       = []
                                                   },
                                                    [("",
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
                                                    ("",
                                                      {
                                                        Parsetree.pexp_desc =
                                                          (Parsetree.Pexp_constant
                                                             (Asttypes.Const_string
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
                         })) in
             {
               Parsetree.pexp_desc =
                 (Parsetree.Pexp_fun
                    ("", None,
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
                             ("", None,
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
                                                 Parsetree.pexp_attributes =
                                                   []
                                               },
                                                [("",
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
                                                ("",
                                                  {
                                                    Parsetree.pexp_desc =
                                                      (Parsetree.Pexp_constant
                                                         (Asttypes.Const_string
                                                            ("@[<2>{ ", None)));
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
                                               ((fields |>
                                                   (let open Ppx_deriving in
                                                      fold_exprs
                                                        (seq_reduce
                                                           ~sep:{
                                                                  Parsetree.pexp_desc
                                                                    =
                                                                    (
                                                                    Parsetree.Pexp_apply
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
                                                                    ("",
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
                                                                    ("",
                                                                    {
                                                                    Parsetree.pexp_desc
                                                                    =
                                                                    (Parsetree.Pexp_constant
                                                                    (Asttypes.Const_string
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
                                                                    (
                                                                    Pervasives.(!)
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
                                                                    (
                                                                    Longident.Ldot
                                                                    ((Longident.Lident
                                                                    "Format"),
                                                                    "fprintf"));
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
                                                         },
                                                          [("",
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
                                                          ("",
                                                            {
                                                              Parsetree.pexp_desc
                                                                =
                                                                (Parsetree.Pexp_constant
                                                                   (Asttypes.Const_string
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
                               }));
                        Parsetree.pexp_loc =
                          (Pervasives.(!) Ast_helper.default_loc);
                        Parsetree.pexp_attributes = []
                      }));
               Parsetree.pexp_loc = (Pervasives.(!) Ast_helper.default_loc);
               Parsetree.pexp_attributes = []
             }
         | (Ptype_abstract ,None ) ->
             raise_errorf ~loc
               "%s cannot be derived for fully abstract types" deriver
         | (Ptype_open ,_) ->
             raise_errorf ~loc "%s cannot be derived for open types" deriver in
       let pp_poly_apply =
         Ppx_deriving.poly_apply_of_type_decl type_decl
           (evar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl)) in
       let stringprinter =
         {
           Parsetree.pexp_desc =
             (Parsetree.Pexp_fun
                ("", None,
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
                                          "asprintf"));
                                   Asttypes.loc =
                                     (Pervasives.(!) Ast_helper.default_loc)
                                 });
                            Parsetree.pexp_loc =
                              (Pervasives.(!) Ast_helper.default_loc);
                            Parsetree.pexp_attributes = []
                          },
                           [("",
                              {
                                Parsetree.pexp_desc =
                                  (Parsetree.Pexp_constant
                                     (Asttypes.Const_string ("%a", None)));
                                Parsetree.pexp_loc =
                                  (Pervasives.(!) Ast_helper.default_loc);
                                Parsetree.pexp_attributes = []
                              });
                           ("", pp_poly_apply);
                           ("",
                             {
                               Parsetree.pexp_desc =
                                 (Parsetree.Pexp_ident
                                    {
                                      Asttypes.txt = (Longident.Lident "x");
                                      Asttypes.loc =
                                        (Pervasives.(!)
                                           Ast_helper.default_loc)
                                    });
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
         } in
       let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
       let pp_type =
         Ppx_deriving.strong_type_of_type @@
           (pp_type_of_decl ~options ~path type_decl) in
       let show_type =
         Ppx_deriving.strong_type_of_type @@
           (show_type_of_decl ~options ~path type_decl) in
       let pp_var =
         pvar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl) in
       let show_var =
         pvar (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl) in
       [Vb.mk (Pat.constraint_ pp_var pp_type)
          (Ppx_deriving.sanitize ~quoter (polymorphize prettyprinter));
       Vb.mk (Pat.constraint_ show_var show_type)
         (polymorphize stringprinter)])
    let () =
      let open Ppx_deriving in
        register
          (create deriver
             ~core_type:(Ppx_deriving.with_quoter
                           (fun quoter  ->
                              fun typ  ->
                                {
                                  Parsetree.pexp_desc =
                                    (Parsetree.Pexp_fun
                                       ("", None,
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
                                                                 "asprintf"));
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
                                                  [("",
                                                     {
                                                       Parsetree.pexp_desc =
                                                         (Parsetree.Pexp_constant
                                                            (Asttypes.Const_string
                                                               ("%a", None)));
                                                       Parsetree.pexp_loc =
                                                         (Pervasives.(!)
                                                            Ast_helper.default_loc);
                                                       Parsetree.pexp_attributes
                                                         = []
                                                     });
                                                  ("",
                                                    {
                                                      Parsetree.pexp_desc =
                                                        (Parsetree.Pexp_fun
                                                           ("", None,
                                                             {
                                                               Parsetree.ppat_desc
                                                                 =
                                                                 (Parsetree.Ppat_var
                                                                    {
                                                                    Asttypes.txt
                                                                    = "fmt";
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
                                                             (expr_of_typ
                                                                quoter typ)));
                                                      Parsetree.pexp_loc =
                                                        (Pervasives.(!)
                                                           Ast_helper.default_loc);
                                                      Parsetree.pexp_attributes
                                                        = []
                                                    });
                                                  ("",
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
                                         }));
                                  Parsetree.pexp_loc =
                                    (Pervasives.(!) Ast_helper.default_loc);
                                  Parsetree.pexp_attributes = []
                                }))
             ~type_decl_str:(fun ~options  ->
                               fun ~path  ->
                                 fun type_decls  ->
                                   [Str.value Recursive
                                      (List.concat
                                         (List.map
                                            (str_of_type ~options ~path)
                                            type_decls))])
             ~type_decl_sig:(fun ~options  ->
                               fun ~path  ->
                                 fun type_decls  ->
                                   List.concat
                                     (List.map (sig_of_type ~options ~path)
                                        type_decls)) ())
