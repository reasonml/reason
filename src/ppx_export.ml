open Migrate_parsetree
open Ast_404

open Parsetree
open Asttypes
open Ast_mapper
open Ast_helper

let export =
  Reason_toolchain.To_current.copy_mapper
  {
    Ast_mapper.default_mapper with
    structure = (fun mapper structure  ->
      let processed_structure = List.map (fun structure_item ->
        match structure_item with
        | {pstr_desc = Pstr_extension (({txt = "export"; loc}, PStr str), _)} -> (
            let signature str =
              let rec acc str res =
                match str with
                | {pstr_desc = Pstr_value (r, vbs)} :: tail ->
                  let value_descriptions = List.map (fun vb ->
                    match vb with
                    | { pvb_pat = {ppat_desc = Ppat_var loc; _}; pvb_expr = {pexp_desc = Pexp_constant const; _}; pvb_attributes; pvb_loc } ->
                      let const_type =
                        match const with
                        | Pconst_integer _ -> "int"
                        | Pconst_char _ -> "char"
                        | Pconst_string _ -> "string"
                        | Pconst_float _ -> "float"
                      in
                      let value = Val.mk
                        ~attrs:pvb_attributes
                        loc
                        (Typ.constr (Location.mkloc (Longident.Lident const_type) (Location.symbol_gloc ())) [])
                      in Sig.mk (Psig_value value)
                    | { pvb_pat = {ppat_desc = Ppat_var loc; _}; pvb_expr = { pexp_desc = Pexp_constraint (_, const)}; pvb_attributes; pvb_loc} -> (
                      (* let%export a:int = 2) *)
                        let (const_type, c) =
                          match const.ptyp_desc with
                          |   Ptyp_constr (loc, ct) -> (loc.txt, ct)
                          | _ ->
                            print_string "not supported just yet";
                            assert false
                      in
                      let value = Val.mk
                        ~attrs:pvb_attributes
                        loc
                        (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
                      in Sig.mk (Psig_value value)
                      )
                    | { pvb_pat = {ppat_desc = Ppat_var loc; _}; pvb_expr = { pexp_desc = Pexp_fun _ as arrow }; pvb_attributes; pvb_loc} -> (
                      (* let%export a (b:int) (c:char) : string = "boe" *)
                      let rec funky func =
                        match func with
                        | Pexp_fun (arg_label, eo, {ppat_desc = Ppat_constraint (_, ct)}, exp) ->
                          Typ.arrow
                            arg_label
                            ct
                            (funky exp.pexp_desc)
                        | Pexp_constraint (_, ct) ->
                          let (const_type, c) =
                            match ct.ptyp_desc with
                            | Ptyp_constr (loc, ct) -> (loc.txt, ct)
                            | _ ->
                              print_string "not supported just yet";
                              assert false
                          in
                          (Typ.constr (Location.mkloc const_type (Location.symbol_gloc ())) c)
                        | _ -> print_string "Not supported sorry"; assert false
                      in
                      let arr = funky arrow
                      in
                      let value = Val.mk
                        ~attrs:pvb_attributes
                        loc
                        arr
                      in Sig.mk (Psig_value value)
                      )
                    | _ -> print_string "a2"; assert false
                  ) vbs
                  in
                  acc tail (res @ value_descriptions)
                | {pstr_desc = Pstr_type (r, t)} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_type (r, t))])
                | {pstr_desc = Pstr_typext te} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_typext te)])
                | {pstr_desc = Pstr_exception e} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_exception e)])
                (* | {pstr_desc = Pstr_module e} :: tail ->
                  Pmod_constraint makes sense
                  acc tail (res @ [Sig.mk (Psig_module e)]) *)
                (* | {pstr_desc = Pstr_recmodule e} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_module e)]) *)
                | {pstr_desc = Pstr_modtype mt} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_modtype mt)])
                (* | {pstr_desc = Pstr_class c} :: tail ->
                  let class_signature cd =
                    let rec acc2 str res =
                      match str with
                      | {pcl_desc = } *)
                      (*
                      | Pcty_constr of Longident.t loc * core_type list
                             (* c
                                ['a1, ..., 'an] c *)
                       | Pcty_signature of class_signature
                             (* object ... end *)
                       | Pcty_arrow of arg_label * core_type * class_type
                             (* T -> CT       Simple
                                ~l:T -> CT    Labelled l
                                ?l:T -> CT    Optional l
                              *)
                       | Pcty_extension of extension
                             (* [%id] *)

                      *)

                    (* in acc2 cd []
                  in
                  let cs = class_signature c in
                  acc tail (res @ [Sig.mk (Psig_class cs)]) *)


                | {pstr_desc = Pstr_class_type e} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_class_type e)])
                | {pstr_desc = Pstr_attribute a} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_attribute a)])
                | {pstr_desc = Pstr_extension (e, a)} :: tail ->
                  acc tail (res @ [Sig.mk (Psig_extension (e, a))])
                | [] -> res
                | _ -> assert false

              in acc str []
            in
            let sig_ = signature str
            in
            Str.include_  {
              pincl_mod = Mod.constraint_
                (Mod.structure str)
                (Mty.signature sig_);
              pincl_loc = loc;
              pincl_attributes = [];
            }
          )
        | _ -> structure_item
      ) structure
      in
      Ast_mapper.default_mapper.structure mapper processed_structure);
    }

  let _ = Compiler_libs.Ast_mapper.register "export"
      (fun _argv -> export)
