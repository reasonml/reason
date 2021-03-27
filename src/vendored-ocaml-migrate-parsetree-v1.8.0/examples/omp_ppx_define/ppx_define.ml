open Migrate_parsetree


(********************)

(* Define the rewriter on OCaml 4.05 AST *)
open Ast_405
let ocaml_version = Versions.ocaml_405

(* We will need to convert parsetree of the current compiler
   (which is not yet known) to 4.05 one. *)
let migrate = Versions.migrate Versions.ocaml_current ocaml_version


(********************)

(* Action of the rewriter: replace identifiers by OCaml expression.
   Here we define how bindings are parsed. *)

let bindings : (string, Parsetree.expression) Hashtbl.t = Hashtbl.create 7

let add_binding binding =
  match String.index binding '=' with
  | exception Not_found ->
      let msg = Printf.sprintf
          "Malformed binding: %S. Binding should have form name=value" binding
      in
      raise (Arg.Bad msg)
  | pos ->
      let name = String.sub binding 0 pos in
      let value =
        let len = String.length binding in
        String.sub binding (pos + 1) (len - pos - 1)
      in
      let expression =
          (* Parse the right handside of the binding *)
          let lexbuf = Lexing.from_string value in
          (* Use compiler-libs parser to get an expression
             of the current version*)
          let expression = Parse.expression lexbuf in
          (* Use migrate to turn the parsetree into a 4.05 parsetree *)
          migrate.Versions.copy_expression expression
      in
      (* If this pipeline failed, ocaml-migrate-parsetree driver will catch
         the exception and report it to the user. *)
      Hashtbl.replace bindings name expression

let args_spec = [
  "-D", Arg.String add_binding,
  "<name=expr> Replace identifier <name> by the ocaml expression <expr>"
]

let reset_args () = Hashtbl.clear bindings

(********************)

(* The rewriter itself *)

let mapper _config _cookies =
  let open Ast_mapper in
  let open Ast_helper in
  let expr mapper pexp =
    match pexp.Parsetree.pexp_desc with
    | Parsetree.Pexp_ident {Location.txt = Longident.Lident name; loc} ->
        begin match Hashtbl.find bindings name with
        | exception Not_found -> default_mapper.expr mapper pexp
        | expr' -> {expr' with Parsetree.pexp_loc = loc}
        end
    | _ -> default_mapper.expr mapper pexp
  in
  {default_mapper with expr}

(********************)

(* Registration *)

let () =
  Driver.register
    ~name:"ppx_here" ~args:args_spec ~reset_args
    ocaml_version mapper
