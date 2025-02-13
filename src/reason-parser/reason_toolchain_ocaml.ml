open Ppxlib

(* The OCaml parser keep doc strings in the comment list. To avoid duplicating
   comments, we need to filter comments that appear as doc strings is the AST
   out of the comment list. *)
let doc_comments_filter () =
  let seen = Hashtbl.create 7 in
  let mapper =
    object
      inherit Ast_traverse.map as super

      method! attribute attr =
        match attr with
        | { attr_name = { Location.txt = "ocaml.doc" | "ocaml.text"; _ }
          ; attr_payload =
              PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( { pexp_desc =
                              Pexp_constant (Pconst_string (_text, _loc, None))
                          ; _
                          }
                        , _ )
                  ; pstr_loc = loc
                  }
                ]
          ; _
          } as attribute ->
          (* Workaround: OCaml 4.02.3 kept an initial '*' in docstrings.
           * For other versions, we have to put the '*' back. *)
          Hashtbl.add seen loc ();
          super#attribute attribute
        | attribute -> super#attribute attribute
    end
  in
  let filter (_text, loc) = not (Hashtbl.mem seen loc) in
  mapper, filter

module Lexer_impl = struct
  type t = Lexing.lexbuf

  let init ?insert_completion_ident:_ lexbuf =
    Lexer.init ();
    lexbuf

  let token = Lexer.token
  let filtered_comments = ref []

  let filter_comments filter =
    filtered_comments := List.filter filter (Lexer.comments ())

  let get_comments _lexbuf _docstrings = !filtered_comments
end

module OCaml_parser = Ocaml_common.Parser

type token = OCaml_parser.token
type invalid_docstrings = unit

(* OCaml parser parses into compiler-libs version of Ast. Parsetrees are
   converted to Reason version on the fly. *)

let parse_and_filter_doc_comments iter fn lexbuf =
  let it, filter = doc_comments_filter () in
  let result = fn lexbuf in
  ignore (iter it result);
  Lexer_impl.filter_comments filter;
  result, ()

let implementation lexbuf =
  parse_and_filter_doc_comments
    (fun it stru -> it#structure stru)
    (fun lexbuf ->
       Reason_toolchain_conf.From_current.copy_structure
         (OCaml_parser.implementation Lexer.token lexbuf))
    lexbuf

let core_type lexbuf =
  parse_and_filter_doc_comments
    (fun it ty -> it#core_type ty)
    (fun lexbuf ->
       Reason_toolchain_conf.From_current.copy_core_type
         (OCaml_parser.parse_core_type Lexer.token lexbuf))
    lexbuf

let interface lexbuf =
  parse_and_filter_doc_comments
    (fun it sig_ -> it#signature sig_)
    (fun lexbuf ->
       Reason_toolchain_conf.From_current.copy_signature
         (OCaml_parser.interface Lexer.token lexbuf))
    lexbuf

let filter_toplevel_phrase it = function
  | Parsetree.Ptop_def str -> ignore (it#structure str)
  | Parsetree.Ptop_dir _ -> ()

let toplevel_phrase lexbuf =
  parse_and_filter_doc_comments
    filter_toplevel_phrase
    (fun lexbuf ->
       Reason_toolchain_conf.From_current.copy_toplevel_phrase
         (OCaml_parser.toplevel_phrase Lexer.token lexbuf))
    lexbuf

let use_file lexbuf =
  parse_and_filter_doc_comments
    (fun it result -> List.map (filter_toplevel_phrase it) result)
    (fun lexbuf ->
       List.map
         Reason_toolchain_conf.From_current.copy_toplevel_phrase
         (OCaml_parser.use_file Lexer.token lexbuf))
    lexbuf

(* Skip tokens to the end of the phrase *)
(* TODO: consolidate these copy-paste skip/trys into something that works for
 * every syntax (also see [Reason_syntax_util]). *)
let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
    | OCaml_parser.SEMISEMI | OCaml_parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
  | Lexer.Error (Lexer.Unterminated_comment _, _)
  | Lexer.Error (Lexer.Unterminated_string, _)
  | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
  | Lexer.Error (Lexer.Illegal_character _, _) ->
    skip_phrase lexbuf

let maybe_skip_phrase lexbuf =
  if
    Parsing.is_current_lookahead OCaml_parser.SEMISEMI
    || Parsing.is_current_lookahead OCaml_parser.EOF
  then ()
  else skip_phrase lexbuf

module Location = Ocaml_common.Location

let safeguard_parsing lexbuf fn =
  try fn () with
  | Lexer.Error (Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//" ->
    skip_phrase lexbuf;
    raise err
  | Syntaxerr.Error _ as err when !Location.input_name = "//toplevel//" ->
    maybe_skip_phrase lexbuf;
    raise err
  (* Escape error is raised as a general catchall when a syntax_error() is
     thrown in the parser. *)
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
    let loc = Location.curr lexbuf in
    if !Location.input_name = "//toplevel//" then maybe_skip_phrase lexbuf;
    raise (Syntaxerr.Error (Syntaxerr.Other loc))

(* Unfortunately we drop the comments because there doesn't exist an ML
 * printer that formats comments *and* line wrapping! (yet) *)
let format_interface_with_comments (signature, _) formatter =
  Ocaml_common.Pprintast.signature
    formatter
    (Reason_toolchain_conf.To_current.copy_signature signature)

let format_implementation_with_comments (structure, _) formatter =
  let structure =
    structure
    |> Reason_syntax_util.(apply_mapper_to_structure backport_letopt_mapper)
    |> Reason_syntax_util.(
         apply_mapper_to_structure remove_stylistic_attrs_mapper)
  in
  Ocaml_common.Pprintast.structure
    formatter
    (Reason_toolchain_conf.To_current.copy_structure structure)

module Lexer = Lexer_impl
