(**
 * Tracks the version of Reason per file, and provides supported
 * feature lookup per version.
 *)
open Reason_migrate_parsetree
open OCaml_408.Ast
open Parsetree
open Location
open Asttypes
open Ast_helper

type file_version = {
  mutable major : int;
  mutable minor : int;
}

type package_version = {
  pkg_major : int;
  pkg_minor : int;
  pkg_patch : int;
}

type feature =
  | AngleBracketTypes
  | HashVariantsColonMethodCallStarClassTypes

(**
 * Tracks the current package version of Reason parser/printer. This is
 * primarily for printing the version with `refmt --version`, but could also
 * used for defaulting printed version in attributes if not specified.
 *)
let package_version = {
  pkg_major = 3;
  pkg_minor = 8;
  pkg_patch = 0;
}

let package_version_string =
  (string_of_int package_version.pkg_major) ^
  "." ^
  (string_of_int package_version.pkg_minor) ^
  "." ^
  (string_of_int  package_version.pkg_patch)

(**
Version to begin parsing with, absent information stating otherwise
(attributes/forced command line)
*)
let default_file_version = {major = 3; minor = 7}

(** * A combination of version_in_ast_attr, cli_arg_parse_version and
 default_file_version together make up the effective parse version. Each one
 has priority over the next.  *)

let unspecified () = {major = -1; minor = -1}

(**
Tracks the file version recorded in the AST itself.
*)
let version_in_ast_attr = {major = -1; minor = -1}

(** Records an explicit version to instruct parsing. This would mean that observing
   an attribute with [@reason.version 3.8] is not necessary to get the lexer/parser
   to begin parsing in version 3.8. *)
let cli_arg_parse_version = {major = -1; minor = -1}

(** Track use of features that would automatically "upgrade"/promote the user.
  There is a subset of features that would correctly lex/parse in an older
  version, *or* a newer version, despite only being printed in the newer
  version of Reason Syntax.
  At the end of parsing, the inferred_promote_version will map replace
  ast version nodes with the newly upgraded version so that if it was persisted
  in binary form to disk, it could be input into refmt, as if that were the explicitly
  set file version in the ast. *)
let inferred_promote_version = {major = -1; minor = -1}

(** Records an explicit version to instruct printing. This would be something
  that was *not* parsed but was explicitly set. It's kind of like
  inferred_promote_version, but explicitly set instead of being inferred by usage.
  - Command line arguments to force printing to a specific version.
  - Some future explicit tag such as [@reason.upgradeTo 3.8] *)
let cli_arg_promote_version = {major = -1; minor = -1}

(* Print version starts out as the default, but then before printing we search for
   any attributes in the AST that tell us to print differently, and if found we
   update this. *)
let print_version = default_file_version

let all_supported_file_versions = [
  default_file_version;
  {major = 3; minor = 8}
]

let latest_version_for_package =
  List.nth all_supported_file_versions (List.length all_supported_file_versions - 1)


let is_set file_version =
  file_version.major > 0 && file_version.minor > 0

let is_set_maj_min maj min =
  maj > 0 && min > 0

let set_explicit_parse_version maj min =
  cli_arg_parse_version.major <- maj;
  cli_arg_parse_version.minor <- min

let set_explicit_promote_version maj min =
  cli_arg_promote_version.major <- maj;
  cli_arg_promote_version.minor <- min

(**
 * We refine the inferred version based on feature usage.
 *)
let refine_inferred feature_used = match feature_used with
  | AngleBracketTypes
  | HashVariantsColonMethodCallStarClassTypes -> (
    let upgrade_to_maj = 3 in
    let upgrade_to_min = 8 in
    if inferred_promote_version.major < upgrade_to_maj ||
        (inferred_promote_version.major == upgrade_to_maj &&
         inferred_promote_version.minor < upgrade_to_min) then (
         inferred_promote_version.major <- upgrade_to_maj;
         inferred_promote_version.minor <- upgrade_to_min
    )
  )

let record_explicit_version_in_ast_if_not_yet major minor =
  if not (is_set version_in_ast_attr) then (
    version_in_ast_attr.major <- major;
    version_in_ast_attr.minor <- minor
  )

(* Allocationless accessor that allows previewing effective version.
   - First any observed version token in the ASt.
   - Then abscent that, any cli --parse-version.
   - Then the default parse version.
 *)
let effective_parse_version_major () =
  if version_in_ast_attr.major >= 0 then
    version_in_ast_attr.major
  else
    (if cli_arg_parse_version.major >= 0 then cli_arg_parse_version.major else default_file_version.major)

(* Allocationless accessor that allows previewing effective version.
   - First any observed version token in the ASt.
   - Then abscent that, any cli --parse-version.
   - Then the default parse version.
 *)
let effective_parse_version_minor () =
  if version_in_ast_attr.minor >= 0 then
    version_in_ast_attr.minor
  else
    (if cli_arg_parse_version.minor >= 0 then cli_arg_parse_version.minor else default_file_version.minor)

(* Effective version to promote to. Unlike effective_parse_version_major, what
 * you pass as the command line --promote-version takes precedence over what is
 * observed in the AST (such as inferred upgrades) *)
let effective_promote_version_major () =
  if cli_arg_promote_version.major >= 0 then
    cli_arg_promote_version.major
  else (
    if inferred_promote_version.major >= 0 then
      inferred_promote_version.major
    else
      effective_parse_version_major ()
  )

let effective_promote_version_minor () =
  if cli_arg_promote_version.minor >= 0 then
    cli_arg_promote_version.minor
  else (
    if inferred_promote_version.minor >= 0 then
      inferred_promote_version.minor
    else
      effective_parse_version_minor ()
  )

let version_within mjr mnr ~inclusive:low_incl (low_mjr, low_mnr) ~inclusive:up_inc (up_mjr, up_mnr) =
  let lower_meets =
    if low_incl then mjr > low_mjr || (mjr == low_mjr && mnr >= low_mnr)
    else mjr > low_mjr || (mjr == low_mjr && mnr > low_mnr)
  in
  let upper_meets =
    if up_inc then mjr < up_mjr || (mjr == up_mjr && mnr <= up_mnr)
    else mjr < up_mjr || (mjr == up_mjr && mnr < up_mnr)
  in
  lower_meets && upper_meets

let parse_version_within ~inclusive =
  let mjr = effective_parse_version_major () in
  let mnr = effective_parse_version_minor () in
  (* Since this relies on side effects, we need to not use partial application
   * without any label *)
  version_within mjr mnr ~inclusive

let print_version_within ~inclusive =
  let mjr = print_version.major in
  let mnr = print_version.minor in
  (* Since this relies on side effects, we need to not use partial application
   * without any label *)
  version_within mjr mnr ~inclusive

(* Fast version of checker to be able to use in tight lexer loops *)
let fast_parse_supports_HashVariantsColonMethodCallStarClassTypes () =
  let mjr = effective_parse_version_major () in
  let mnr = effective_parse_version_minor () in
  (mjr == 3 && mnr >= 8) || mjr > 3

let parse_version_at_least (major, minor) =
  parse_version_within ~inclusive:true (major, minor) ~inclusive:true (10000,0)

let print_version_at_least (major, minor) =
  print_version_within ~inclusive:true (major, minor) ~inclusive:true (10000,0)

let parse_supports = function
  | AngleBracketTypes -> parse_version_at_least (3, 8)
  | HashVariantsColonMethodCallStarClassTypes -> parse_version_at_least (3, 8)

let print_supports = function
  | AngleBracketTypes -> print_version_at_least (3, 8)
  | HashVariantsColonMethodCallStarClassTypes -> print_version_at_least (3, 8)

let dummy_loc () = {
  loc_start = Lexing.dummy_pos;
  loc_end = Lexing.dummy_pos;
  loc_ghost = false;
}

(* Implementation of String.split_on_char, since it's not available in older
 * OCamls *)
let _split_on_char sep_char str =
  let r = {contents = []} in
  let j = {contents = String.length str} in
  for i = String.length str - 1 downto 0 do
    if String.unsafe_get str i = sep_char then begin
      r.contents <- String.sub str (i + 1) (!j - i - 1) :: r.contents;
      j.contents <- i
    end
  done;
  String.sub str 0 j.contents :: r.contents

(**
 * A note on "promotion".
 * We will infer that we should auto-upgrade based on usage of certain
 * features.
 *
 * Promotion either upgrades the version tag during injection of the
 * (otherwise) default version tag, or it upgrades/rewrites tags during print
 * time if tags were already present.
 *)
module Ast_nodes = struct
  let parse_version v =
    match _split_on_char '.' v, "0" with
    | ([maj], mnr)
    | ([maj; ""], mnr)
    | (maj :: mnr :: _, _) ->
      let imaj, imin = int_of_string maj, int_of_string mnr in
      Some (imaj, imin)
    | _ -> None

  let is_structure_version_attribute = function
    | { pstr_desc=(
          Pstr_attribute ({
            attr_name={txt="reason.version"; _};
            attr_payload =
              PStr [
                {pstr_desc=Pstr_eval({pexp_desc=Pexp_constant(Pconst_float(v, _)); _} as b,_); _} as c
              ];
            _
          } as a)
        ); _
      } as structure_item ->
        (match parse_version v with
        | Some(imaj, imin) ->
          let updater new_maj new_min =
            let new_v = string_of_int new_maj ^ "." ^ string_of_int new_min in
            let new_payload_desc = {
              c with
              pstr_desc=Pstr_eval({b with pexp_desc=Pexp_constant(Pconst_float(new_v, None))},[])
            } in
            let new_pstr_desc = Pstr_attribute {a with attr_payload = PStr [new_payload_desc]} in
            {structure_item with pstr_desc = new_pstr_desc}
          in
          Some (updater, imaj, imin)
        | _ -> None)
    | _ -> None

  let is_sig_version_attribute = function
    | { psig_desc=(
          Psig_attribute ({
            attr_name={txt="reason.version"; _};
            attr_payload = PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant(Pconst_float(v, _)); _} as b, _); _} as c];
            _
          } as a)
        ); _
      } as sig_item ->
        (match parse_version v with
        | Some(imaj, imin) ->
          let updater new_maj new_min =
            let new_v = string_of_int new_maj ^ "." ^ string_of_int new_min in
            let new_payload_desc = {
              c with
              pstr_desc=Pstr_eval({b with pexp_desc=Pexp_constant(Pconst_float(new_v, None))},[])
            } in
            let new_psig_desc = Psig_attribute {a with attr_payload = PStr [new_payload_desc]} in
            {sig_item with psig_desc = new_psig_desc}
          in
          Some (updater, imaj, imin)
        | _ -> None)
    | _ -> None

  let mk_warning_attribute_payload ~loc msg =
    let exp = Exp.mk ~loc  (Pexp_constant (Pconst_string(msg, None))) in
    let item = { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc } in
    PStr [item]

  let mk_version_attr_payload major minor =
    let major, minor = string_of_int major, string_of_int minor in
    let loc = dummy_loc () in
    let exp = Exp.mk ~loc  (Pexp_constant (Pconst_float(major ^ "." ^ minor, None))) in
    let item = { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc } in
    PStr [item]

  (* let should_promote ~inferred_min ~inferred_maj ~explicit = *)
  (*   let {major = inf_major; minor = inf_minor} = inferred in *)
  (*   let {major = exp_major; minor = exp_minor} = explicit in *)
  (*   is_set inferred && *)
  (*     (not (is_set explicit) || *)
  (*       inf_major > exp_major || inf_major == exp_major && inf_minor > exp_minor) *)

  (*
   * splice_fallback is the splicer that will place an attribute at the best
   * possible place.  It starts out as just inserting at the head, but if a
   * better place is discovered (according to insert_between) a new splice_fallback
   * is created - then used if an update never occured.
   *)
  let replace_or_inject_item ~attribute_tester ~insert_between ~creator maj min items =
    let rec impl ~splicer ~rev_prev items =
      match (items : 'a list) with
      | [] ->
          let loc = dummy_loc () in
          let attr_payload = mk_version_attr_payload maj min in
          let created = (creator ~loc {attr_name={loc; txt="reason.version"}; attr_payload; attr_loc=loc}) in
          splicer created
      | hd :: tl ->
        (match attribute_tester hd with
        | None ->
            let splicer =
              if insert_between rev_prev items then fun itm -> List.rev rev_prev @ itm :: items else splicer
            in
            impl ~splicer ~rev_prev:(hd :: rev_prev) tl
        | Some(updater, _old_maj, _old_min) -> (List.rev rev_prev) @ updater maj min :: tl
        )
    in
    impl ~splicer:(fun itm -> itm :: items) ~rev_prev:[] items

  (** Creates an attribute to inject into the AST if it was not already present *)
  let inject_attr_for_printing ~attribute_tester ~insert_between ~creator itms =
    let major = effective_promote_version_major () in
    let minor = effective_promote_version_minor () in
    replace_or_inject_item ~attribute_tester ~insert_between ~creator major minor itms

  (* Injects a version attribute if none was present. We don't do any inferred promotion here.
   * The reason is that this will already happen in the printer if parsing and printing are done
   * within the same process (the mutable inferred version will be retained and used to inform
   * the printer which version of the syntax to print to (and how it should replace version attributes
   * with rewritten ones according to the version that was inferred. *)
  let is_floating_str_comment = function
    | {pstr_desc = Pstr_attribute {attr_name = {txt="ocaml.doc"|"ocaml.text"; _}; _}; _} -> true
    | _ -> false
  let is_floating_sig_comment = function
    | {psig_desc = Psig_attribute {attr_name = {txt="ocaml.doc"|"ocaml.text"; _}; _}; _} -> true
    | _ -> false
  let inject_attr_to_instruct_printing_impl itms =
    (* Inserts after the first one or two floating comments *)
    let insert_between rev_prev remaining = match rev_prev, remaining with
      | [second; first], _third when is_floating_str_comment second && is_floating_str_comment first -> true
      | [first], (second :: _) when is_floating_str_comment first && not (is_floating_str_comment second) -> true
      | [first], [] when is_floating_str_comment first -> true
      | _ -> false
    in
    let creator = (fun ~loc x -> Str.mk ~loc (Pstr_attribute x)) in
    inject_attr_for_printing ~attribute_tester:is_structure_version_attribute ~insert_between ~creator itms

  let inject_attr_to_instruct_printing_intf itms =
    (* Inserts after the first one or two floating comments *)
    let insert_between rev_prev remaining = match rev_prev, remaining with
      | [second; first], _third when is_floating_sig_comment second && is_floating_sig_comment first -> true
      | [first], (second :: _) when is_floating_sig_comment first && not (is_floating_sig_comment second) -> true
      | [first], [] when is_floating_sig_comment first -> true
      | _ -> false
    in
    let creator = (fun ~loc x -> Sig.mk ~loc (Psig_attribute x)) in
    inject_attr_for_printing ~attribute_tester:is_sig_version_attribute ~insert_between ~creator itms
end
