open Ppxlib

type lexing_error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Capitalized_label of string
  | Invalid_literal of string
  | Invalid_encoding of string
  | Invalid_char_in_ident of Uchar.t
  | Non_lowercase_delimiter of string
  | Capitalized_raw_identifier of string

type ast_error =
  | Not_expecting of Location.t * string
  | Other_syntax_error of string
  | Variable_in_scope of Location.t * string
  | Applicative_path of Location.t

type parsing_error = string

type reason_error =
  | Lexing_error of lexing_error
  | Parsing_error of parsing_error
  | Ast_error of ast_error

exception Reason_error of reason_error * Location.t

let catch_errors : (reason_error * Location.t) list ref option ref = ref None

let raise_error error loc =
  match !catch_errors with
  | None -> raise (Reason_error (error, loc))
  | Some caught -> caught := (error, loc) :: !caught

let raise_fatal_error error loc = raise (Reason_error (error, loc))

let recover_non_fatal_errors f =
  let catch_errors0 = !catch_errors in
  let errors = ref [] in
  catch_errors := Some errors;
  let result = match f () with x -> Ok x | exception exn -> Error exn in
  catch_errors := catch_errors0;
  result, List.rev !errors

(* Report lexing errors *)

let format_lexing_error ppf = function
  | Illegal_character c ->
    Format.fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
    Format.fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ -> Format.fprintf ppf "Comment not terminated"
  | Unterminated_string -> Format.fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
    Format.fprintf
      ppf
      "This comment contains an unterminated string literal@.%aString literal \
       begins here"
      Ocaml_util.print_loc
      loc
  | Keyword_as_label kwd ->
    Format.fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Capitalized_label lbl ->
    Format.fprintf
      ppf
      "`%s' cannot be used as label name, it must start with a lowercase letter"
      lbl
  | Invalid_literal s -> Format.fprintf ppf "Invalid literal %s" s
  | Invalid_encoding s ->
    Format.fprintf ppf "Invalid encoding of identifier %s." s
  | Invalid_char_in_ident u ->
    Format.fprintf ppf "Invalid character U+%X in identifier" (Uchar.to_int u)
  | Capitalized_raw_identifier lbl ->
    Format.fprintf
      ppf
      "`%s' cannot be used as a raw identifier, it must start with a lowercase \
       letter"
      lbl
  | Non_lowercase_delimiter name ->
    Format.fprintf
      ppf
      "`%s' cannot be used as a quoted string delimiter,@ it must contain only \
       lowercase letters."
      name

let format_parsing_error ppf msg = Format.fprintf ppf "%s" msg

let format_ast_error ppf = function
  | Not_expecting (loc, nonterm) ->
    Format.fprintf
      ppf
      "Syntax error: %a%s not expected."
      Ocaml_util.print_loc
      loc
      nonterm
  | Applicative_path loc ->
    Format.fprintf
      ppf
      "Syntax error: %aapplicative paths of the form F(X).t are not supported \
       when the option -no-app-func is set."
      Ocaml_util.print_loc
      loc
  | Variable_in_scope (loc, var) ->
    Format.fprintf
      ppf
      "%aIn this scoped type, variable '%s is reserved for the local type %s."
      Ocaml_util.print_loc
      loc
      var
      var
  | Other_syntax_error msg -> Format.fprintf ppf "%s" msg

let format_error ppf = function
  | Lexing_error err -> format_lexing_error ppf err
  | Parsing_error err -> format_parsing_error ppf err
  | Ast_error err -> format_ast_error ppf err

let report_error ppf ~loc err =
  Format.fprintf
    ppf
    "@[%a@]@."
    (Ocaml_util.print_error ~loc ~f:format_error)
    err

let recover_parser_error f loc msg =
  if !Reason_config.recoverable
  then f loc msg
  else raise_fatal_error (Parsing_error msg) loc

let () =
  Printexc.register_printer (function
    | Reason_error (err, loc) ->
      let _ = Format.flush_str_formatter () in
      report_error Format.str_formatter ~loc err;
      Some (Format.flush_str_formatter ())
    | _ -> None)

let str_eval_message text =
  { Parsetree.pstr_loc = Location.none
  ; pstr_desc =
      Pstr_eval
        ( { pexp_loc = Location.none
          ; pexp_desc =
              Pexp_constant
                (Parsetree.Pconst_string (text, Location.none, None))
          ; pexp_attributes = []
          ; pexp_loc_stack = []
          }
        , [] )
  }

(** Generate a suitable extension node for Merlin's consumption, for the
    purposes of reporting a parse error - only used in recovery mode. Parse
    error will prevent Merlin from reporting subsequent errors, as they might be
    due wrong recovery decisions and will confuse the user. *)
let error_extension_node_from_recovery loc msg =
  recover_parser_error
    (fun loc msg ->
       let str = { Location.loc; txt = "merlin.syntax-error" } in
       let payload = [ str_eval_message msg ] in
       str, Parsetree.PStr payload)
    loc
    msg

(** Generate a suitable extension node for OCaml consumption, for the purposes
    of reporting a syntax error. Contrary to
    [error_extension_node_from_recovery], these work both with OCaml and with
    Merlin. *)
let error_extension_node loc msg =
  recover_parser_error
    (fun loc msg ->
       let str = { Location.loc; txt = "ocaml.error" } in
       let payload =
         [ str_eval_message msg; (* if_highlight *) str_eval_message msg ]
       in
       str, Parsetree.PStr payload)
    loc
    msg
