(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(*  This file has been modified/specialized for ocaml-migrate-parsetree *)

(* Generate code to perform a deep copy of a type into another
   identical type (in another module).  Used to generate a first
   version of migration code between two versions of the same type,
   to be then patched manually to perform actual migration. *)

let drop_prefix ~prefix s =
  let plen = String.length prefix in
  if plen > String.length s then None
  else
    try
      for i = 0 to String.length prefix - 1 do
        if not (Char.equal s.[i] prefix.[i]) then raise Exit
      done;
      Some (String.sub s plen (String.length s - plen))
    with Exit -> None

let rec find_map f = function
  | [] -> None
  | x :: xs -> ( match f x with None -> find_map f xs | Some x -> Some x )

module Main : sig end = struct
  open Types
  open Asttypes
  open Location
  open Ast_helper

  module Label = struct
    type t = Asttypes.arg_label

    type desc = Asttypes.arg_label =
      | Nolabel
      | Labelled of string
      | Optional of string

    let nolabel : t = Nolabel
  end

  let may_tuple ?loc tup = function
    | [] -> None
    | [ x ] -> Some x
    | l -> Some (tup ?loc ?attrs:None l)

  let lid ?(loc = !default_loc) s = mkloc (Longident.parse s) loc

  let constr ?loc ?attrs s args =
    Exp.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Exp.tuple args)

  let unit ?loc ?attrs () = constr ?loc ?attrs "()" []

  let tuple ?loc ?attrs = function
    | [] -> unit ?loc ?attrs ()
    | [ x ] -> x
    | xs -> Exp.tuple ?loc ?attrs xs

  let app ?loc ?attrs f l =
    if l = [] then f
    else Exp.apply ?loc ?attrs f (List.map (fun a -> (Label.nolabel, a)) l)

  let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lid ?loc s)

  let let_in ?loc ?attrs ?(recursive = false) b body =
    Exp.let_ ?loc ?attrs (if recursive then Recursive else Nonrecursive) b body

  let func ?loc ?attrs l =
    Exp.function_ ?loc ?attrs (List.map (fun (p, e) -> Exp.case p e) l)

  let lam ?loc ?attrs ?(label = Label.nolabel) ?default pat exp =
    Exp.fun_ ?loc ?attrs label default pat exp

  let pvar ?(loc = !default_loc) ?attrs s = Pat.var ~loc ?attrs (mkloc s loc)

  let pconstr ?loc ?attrs s args =
    Pat.construct ?loc ?attrs (lid ?loc s) (may_tuple ?loc Pat.tuple args)

  let selfcall m args = app (evar m) args

  (*************************************************************************)

  let env = Env.initial_safe_string

  let module_mapping = ref []

  let rec clean = function
    | [ "Location"; "t" ] -> [ "location" ]
    | [] -> []
    | [ x ] -> [ x ]
    | [ _; "t" ] as x -> x
    | _ :: xs -> clean xs

  let print_fun s =
    let lid = Longident.parse s in
    let s = Longident.flatten lid |> clean in
    String.concat "_" ("copy" :: s)

  let printed = Hashtbl.create 16

  let meths = ref []

  let rec gen ty =
    if Hashtbl.mem printed ty then ()
    else
      let tylid = Longident.parse ty in
      let td =
        try snd (Env.lookup_type tylid env ~loc:Location.none)
        with Not_found ->
          Format.eprintf "** Cannot resolve type %s@." ty;
          exit 2
      in
      let prefix, local =
        let open Longident in
        match tylid with
        | Ldot (m, s) -> (String.concat "." (Longident.flatten m) ^ ".", s)
        | Lident s -> ("", s)
        | Lapply _ -> assert false
      in
      let target_prefix =
        match
          find_map
            (fun (v1, v2) ->
              match drop_prefix ~prefix:v1 prefix with
              | None -> None
              | Some suffix -> Some (v2 ^ suffix) )
            !module_mapping
        with
        | Some x -> x
        | None -> prefix
      in
      let funname = print_fun ty in
      Hashtbl.add printed ty ();
      let params_in =
        List.mapi
          (fun i _ -> mkloc (Printf.sprintf "f%i" i) !default_loc)
          td.type_params
      in
      let params_out =
        List.mapi
          (fun i _ -> mkloc (Printf.sprintf "g%i" i) !default_loc)
          td.type_params
      in
      let env =
        List.map2 (fun s t -> (t.id, evar s.txt)) params_in td.type_params
      in
      let make_result_t tyargs_in tyargs_out =
        Typ.(
          arrow Asttypes.Nolabel
            (constr (lid (prefix ^ local)) tyargs_in)
            (constr (lid (target_prefix ^ local)) tyargs_out))
      in
      let make_t tyargs_in tyargs_out =
        List.fold_right2
          (fun arg_in arg_out t ->
            Typ.(
              arrow Asttypes.Nolabel (arrow Asttypes.Nolabel arg_in arg_out) t)
            )
          tyargs_in tyargs_out
          (make_result_t tyargs_in tyargs_out)
      in
      let tyargs_in = List.map (fun t -> Typ.var t.txt) params_in in
      let tyargs_out = List.map (fun t -> Typ.var t.txt) params_out in
      let t =
        Typ.poly (params_in @ params_out) (make_t tyargs_in tyargs_out)
      in
      let concrete e =
        let e =
          List.fold_right
            (fun x e -> lam x e)
            (List.map (fun x -> pvar x.txt) params_in)
            e
        in
        meths := Vb.mk (Pat.constraint_ (pvar funname) t) e :: !meths
      in
      let field ld =
        let s = Ident.name ld.ld_id in
        ( (lid (prefix ^ s), pvar s),
          (lid (target_prefix ^ s), tyexpr env ld.ld_type (evar s)) )
      in
      match (td.type_kind, td.type_manifest) with
      | Type_record (l, _), _ ->
          let l = List.map field l in
          concrete
            (lam
               (Pat.record (List.map fst l) Closed)
               (Exp.record (List.map snd l) None))
      | Type_variant l, _ ->
          let case cd =
            let c = Ident.name cd.cd_id in
            match cd.cd_args with
            | Cstr_tuple tys ->
                let p, args = gentuple env tys in
                (pconstr (prefix ^ c) p, constr (target_prefix ^ c) args)
            | Cstr_record _l ->
                failwith "Inline records are not yet supported."
          in
          concrete (func (List.map case l))
      | Type_abstract, Some t -> concrete (tyexpr_fun env t)
      | Type_abstract, None -> failwith ("Abstract type " ^ ty)
      | Type_open, _ ->
          Format.eprintf "** Open types are not yet supported %s@." ty;
          ()

  and gentuple env tl =
    let arg i t =
      let x = Printf.sprintf "x%i" i in
      (pvar x, tyexpr env t (evar x))
    in
    List.split (List.mapi arg tl)

  and tyexpr env ty x =
    match ty.desc with
    | Tvar _ -> (
      match List.assoc ty.id env with
      | f -> app f [ x ]
      | exception Not_found -> failwith "Existentials not supported" )
    | Ttuple tl ->
        let p, e = gentuple env tl in
        let_in [ Vb.mk (Pat.tuple p) x ] (tuple e)
    | Tconstr (path, [ t ], _) when Path.same path Predef.path_list ->
        app (evar "List.map") [ tyexpr_fun env t; x ]
    | Tconstr (path, [ t ], _) when Path.same path Predef.path_array ->
        app (evar "Array.map") [ tyexpr_fun env t; x ]
    | Tconstr (path, [ t ], _) when Path.same path Predef.path_option ->
        app (evar "Option.map") [ tyexpr_fun env t; x ]
    | Tconstr (path, [], _)
      when Path.same path Predef.path_string
           || Path.same path Predef.path_bytes
           || Path.same path Predef.path_bool
           || Path.same path Predef.path_unit
           || Path.same path Predef.path_exn
           || Path.same path Predef.path_int
           || Path.same path Predef.path_char
           || Path.same path Predef.path_int32
           || Path.same path Predef.path_int64
           || Path.same path Predef.path_nativeint
           || Path.same path Predef.path_float
           || Path.same path Predef.path_extension_constructor ->
        x
    | Tconstr (path, tl, _) ->
        let ty = Path.name path in
        gen ty;
        selfcall (print_fun ty) (List.map (tyexpr_fun env) tl @ [ x ])
    | _ ->
        Format.eprintf "** Cannot deal with type %a@." Printtyp.type_expr ty;
        x

  and tyexpr_fun env ty = lam (pvar "x") (tyexpr env ty (evar "x"))

  let simplify =
    (* (fun x -> <expr> x) ====> <expr> *)
    let open Ast_mapper in
    let super = default_mapper in
    let expr this e =
      let e = super.expr this e in
      let open Longident in
      let open Parsetree in
      match e.pexp_desc with
      | Pexp_fun
          ( Asttypes.Nolabel,
            None,
            { ppat_desc = Ppat_var { txt = id; _ }; _ },
            { pexp_desc =
                Pexp_apply
                  ( f,
                    [ ( Asttypes.Nolabel,
                        { pexp_desc = Pexp_ident { txt = Lident id2; _ }; _ }
                      )
                    ] )
            ; _
            } )
        when id = id2 ->
          f
      | _ -> e
    in
    let value_binding this (vb : Parsetree.value_binding) =
      let pvb_pat = this.pat this vb.pvb_pat in
      let pvb_expr = super.expr this vb.pvb_expr in
      let pvb_attributes = this.attributes this vb.pvb_attributes in
      let pvb_loc = this.location this vb.pvb_loc in
      { Parsetree.pvb_loc; pvb_attributes; pvb_expr; pvb_pat }
    in
    { super with expr; value_binding }

  let add_mapping s =
    let i =
      try String.index s ':'
      with Not_found -> failwith (Printf.sprintf "Cannot parse mapping %S" s)
    in
    module_mapping :=
      ( String.sub s 0 i ^ ".",
        String.sub s (i + 1) (String.length s - i - 1) ^ "." )
      :: !module_mapping

  let args =
    let open Arg in
    [ ( "-I",
        String
          (fun s ->
            Load_path.add_dir (Misc.expand_directory Config.standard_library s)
            ),
        "<dir> Add <dir> to the list of include directories" );
      ( "-map",
        String add_mapping,
        "Old_module:New_module  Map types from Old_module to types in \
         New_module" )
    ]

  let usage = Printf.sprintf "%s [options] <type names>\n" Sys.argv.(0)

  let main () =
    Load_path.init [ Config.standard_library ];
    Arg.parse (Arg.align args) gen usage;
    let from_, to_ =
      match !module_mapping with
      | [ (from_, to_) ] ->
          ( String.sub from_ 0 (String.length from_ - 1),
            String.sub to_ 0 (String.length to_ - 1) )
      | _ -> failwith "expect one and only one '-map' argument"
    in
    let s =
      [ Str.module_
          (Mb.mk
             (mkloc (Some "From") Location.none)
             (Mod.ident (mkloc (Longident.parse from_) Location.none)));
        Str.module_
          (Mb.mk (mkloc (Some "To") Location.none)
             (Mod.ident (mkloc (Longident.parse to_) Location.none)));
        Str.value Recursive !meths
      ]
    in
    Format.printf "%a@." Pprintast.structure
      (simplify.Ast_mapper.structure simplify s)

  let () =
    try main ()
    with exn ->
      Format.eprintf "%a@?" Errors.report_error exn;
      exit 1
end
