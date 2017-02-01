open Ast_mapper
open Asttypes
open Parsetree
open Ast_helper
open Location

type tyname = string

type ty =
  | Int
  | Float
  (* | List of ty list  *)
  | Other of tyname

let string_of_ty = function
  | Int -> "int"
  | Float -> "float"
  | Other s -> s

type tykind =
  | Ty of ty
  | Tuple of ty list
  | Variant of (tyname * ty list) list

type 'a tyal = tykind * 'a

let printers = ref []

let add_printer ty p = printers := (ty, p)::!printers
let try_get_printer ty = List.assoc ty !printers

let myvar = (Tuple [Int; Float]), (3, 3.5)

let dummy_loc () = {
  loc_start = Lexing.dummy_pos;
  loc_end = Lexing.dummy_pos;
  loc_ghost = false;
}

let make_ghost_loc loc = {
    loc with loc_ghost = true
}

let make_ghost_exp exp = {
    exp with pexp_loc = make_ghost_loc exp.pexp_loc
}

let make_ghost_pat pat = {
    pat with ppat_loc = make_ghost_loc pat.ppat_loc
}

let ghloc ?(loc=dummy_loc ()) d = { txt = d; loc = (make_ghost_loc loc) }

let make_ghost_exp e = { txt = e; loc = dummy_loc () }
let mkident s = Pexp_ident (make_ghost_exp (Longident.Lident s))
let gen_apply_printer f x = Pexp_apply ((Exp.mk (mkident f)), [x])

(* let gen_let tyname argname =
  let fnname = "show_" ^ (string_of_ty tyname) in
  let pat = make_ghost_pat { ppat_desc = Ppat_var (ghloc fnname);
                                   ppat_attributes = [];
                                   ppat_loc = dummy_loc () } in
  let prn = try_get_printer tyname in
  let exp = make_ghost_exp { pexp_desc = gen_apply_printer prn ("x" );
                                   pexp_attributes = [];
                                   pexp_loc = dummy_loc () } in
  Exp.let_ Nonrecursive [{ pvb_pat = pat; pvb_expr = exp}] *)

(* let printvar = function
  | (Ty Int, i) -> (Exp.let_ Nonrecursive 
   *)

let () =
  let () = add_printer Int "string_of_int" in
  let () = add_printer Float "string_of_float" in
  let id = mkident "f" in
  let prn = gen_apply_printer (try_get_printer Int) ("nolabel", { pexp_desc=id; pexp_loc=dummy_loc(); pexp_attributes=[]
                                                              }) in
  let exp = { pexp_desc=id; pexp_loc=dummy_loc(); pexp_attributes=[] } in
  let sitem = { pstr_desc=Pstr_eval (exp, []); pstr_loc=dummy_loc() } in
  (* Printast.implementation Format.std_formatter [sitem] *)
  let _ =
    Reason_toolchain.ML.print_canonical_implementation_with_comments Format.std_formatter ([sitem], [])
  in ()
