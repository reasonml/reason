open Migrate_parsetree.Ast_408
open Reason_toolchain_conf

(* The OCaml parser keep doc strings in the comment list.
     To avoid duplicating comments, we need to filter comments that appear
     as doc strings is the AST out of the comment list. *)
let doc_comments_filter () =
  let open Ast_mapper in
  let open Parsetree in
  let seen = Hashtbl.create 7 in
  let attribute mapper = function
    | { attr_name = { Location. txt = ("ocaml.doc" | "ocaml.text")};
        attr_payload =
          PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string(_text, None)) } , _);
                  pstr_loc = loc }]} as attribute ->
       (* Workaround: OCaml 4.02.3 kept an initial '*' in docstrings.
        * For other versions, we have to put the '*' back. *)
       Hashtbl.add seen loc ();
       default_mapper.attribute mapper attribute
    | attribute -> default_mapper.attribute mapper attribute
  in
  let mapper = {default_mapper with attribute} in
  let filter (_text, loc) = not (Hashtbl.mem seen loc) in
  (mapper, filter)

module Lexer_impl = struct
  type t = Lexing.lexbuf
  let init ?insert_completion_ident:_ lexbuf =
    Lexer.init (); lexbuf
  let token = Lexer.token

  let filtered_comments = ref []
  let filter_comments filter =
    filtered_comments := List.filter filter (Lexer.comments ())
  let get_comments _lexbuf _docstrings = !filtered_comments
end
module OCaml_parser = Parser
type token = OCaml_parser.token
type invalid_docstrings = unit

(* OCaml parser parses into compiler-libs version of Ast.
     Parsetrees are converted to Reason version on the fly. *)

let parse_and_filter_doc_comments iter fn lexbuf=
  let it, filter = doc_comments_filter () in
  let result = fn lexbuf in
  ignore (iter it result);
  Lexer_impl.filter_comments filter;
  (result, ())

let implementation lexbuf =
  parse_and_filter_doc_comments
    (fun it -> it.Ast_mapper.structure it)
    (fun lexbuf -> From_current.copy_structure
                     (Parser.implementation Lexer.token lexbuf))
    lexbuf

let core_type lexbuf =
  parse_and_filter_doc_comments
    (fun it -> it.Ast_mapper.typ it)
    (fun lexbuf -> From_current.copy_core_type
                     (Parser.parse_core_type Lexer.token lexbuf))
    lexbuf

let interface lexbuf =
  parse_and_filter_doc_comments
    (fun it -> it.Ast_mapper.signature it)
    (fun lexbuf -> From_current.copy_signature
                     (Parser.interface Lexer.token lexbuf))
    lexbuf

let filter_toplevel_phrase it = function
  | Parsetree.Ptop_def str -> ignore (it.Ast_mapper.structure it str)
  | Parsetree.Ptop_dir _ -> ()

let toplevel_phrase lexbuf =
  parse_and_filter_doc_comments
    filter_toplevel_phrase
    (fun lexbuf -> From_current.copy_toplevel_phrase
                     (Parser.toplevel_phrase Lexer.token lexbuf))
    lexbuf

let use_file lexbuf =
  parse_and_filter_doc_comments
    (fun it result -> List.map (filter_toplevel_phrase it) result)
    (fun lexbuf ->
      List.map
        From_current.copy_toplevel_phrase
        (Parser.use_file Lexer.token lexbuf))
    lexbuf

(* Skip tokens to the end of the phrase *)
(* TODO: consolidate these copy-paste skip/trys into something that works for
 * every syntax (also see [Reason_syntax_util]). *)
let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      OCaml_parser.SEMISEMI | OCaml_parser.EOF -> ()
      | _ -> skip_phrase lexbuf
  with
  | Lexer.Error (Lexer.Unterminated_comment _, _)
    | Lexer.Error (Lexer.Unterminated_string, _)
    | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
    | Lexer.Error (Lexer.Illegal_character _, _) ->
     skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead OCaml_parser.SEMISEMI
     || Parsing.is_current_lookahead OCaml_parser.EOF
  then ()
  else skip_phrase lexbuf

let safeguard_parsing lexbuf fn =
  try fn ()
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
       when !Location.input_name = "//toplevel//"->
     skip_phrase lexbuf;
     raise err
  | Syntaxerr.Error _ as err
       when !Location.input_name = "//toplevel//" ->
     maybe_skip_phrase lexbuf;
     raise err
  (* Escape error is raised as a general catchall when a syntax_error() is
       thrown in the parser.
   *)
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
     let loc = Location.curr lexbuf in
     if !Location.input_name = "//toplevel//"
     then maybe_skip_phrase lexbuf;
     raise(Syntaxerr.Error(Syntaxerr.Other loc))

(* Unfortunately we drop the comments because there doesn't exist an ML
 * printer that formats comments *and* line wrapping! (yet) *)
let format_interface_with_comments (signature, _) formatter =
  Pprintast.signature formatter
    (To_current.copy_signature signature)
let format_implementation_with_comments (structure, _) formatter =
  let structure =
    Reason_syntax_util.(apply_mapper_to_structure
      structure
      (backport_letopt_mapper remove_stylistic_attrs_mapper))
  in
  Pprintast.structure formatter
    (To_current.copy_structure structure)

module Lexer = Lexer_impl
