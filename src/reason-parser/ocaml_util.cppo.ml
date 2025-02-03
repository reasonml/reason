let print_loc ppf loc =
  Location.print_loc ppf loc


let print_error loc f ppf x =
#if OCAML_VERSION >= (5,3,0)
  let error =
    let f (fmt: Format_doc.formatter) err =
      let doc_f =
        Format_doc.deprecated_printer (fun fmt -> Format.fprintf fmt "%a" f err)
      in
      doc_f fmt
    in
    Location.error_of_printer ~loc f x in
  Location.print_report ppf error
#elif OCAML_VERSION >= (4,8,0)
  let error = Location.error_of_printer ~loc f x in
  Location.print_report ppf error
#else
  let error = Location.error_of_printer loc f x in
  Location.report_error ppf error
#endif

#if OCAML_VERSION >= (5,3,0)
module Utf8_lexeme = Misc.Utf8_lexeme
#else
(** {1 Minimal support for Unicode characters in identifiers} *)

module Utf8_lexeme = struct

  type t = string

  (* Non-ASCII letters that are allowed in identifiers (currently: Latin-9) *)

  type case = Upper of Uchar.t | Lower of Uchar.t
  let known_chars : (Uchar.t, case) Hashtbl.t = Hashtbl.create 32

  let _ =
    List.iter
      (fun (upper, lower) ->
        let upper = Uchar.of_int upper and lower = Uchar.of_int lower in
        Hashtbl.add known_chars upper (Upper lower);
        Hashtbl.add known_chars lower (Lower upper))
  [
    (0xc0, 0xe0); (* À, à *)    (0xc1, 0xe1); (* Á, á *)
    (0xc2, 0xe2); (* Â, â *)    (0xc3, 0xe3); (* Ã, ã *)
    (0xc4, 0xe4); (* Ä, ä *)    (0xc5, 0xe5); (* Å, å *)
    (0xc6, 0xe6); (* Æ, æ *)    (0xc7, 0xe7); (* Ç, ç *)
    (0xc8, 0xe8); (* È, è *)    (0xc9, 0xe9); (* É, é *)
    (0xca, 0xea); (* Ê, ê *)    (0xcb, 0xeb); (* Ë, ë *)
    (0xcc, 0xec); (* Ì, ì *)    (0xcd, 0xed); (* Í, í *)
    (0xce, 0xee); (* Î, î *)    (0xcf, 0xef); (* Ï, ï *)
    (0xd0, 0xf0); (* Ð, ð *)    (0xd1, 0xf1); (* Ñ, ñ *)
    (0xd2, 0xf2); (* Ò, ò *)    (0xd3, 0xf3); (* Ó, ó *)
    (0xd4, 0xf4); (* Ô, ô *)    (0xd5, 0xf5); (* Õ, õ *)
    (0xd6, 0xf6); (* Ö, ö *)    (0xd8, 0xf8); (* Ø, ø *)
    (0xd9, 0xf9); (* Ù, ù *)    (0xda, 0xfa); (* Ú, ú *)
    (0xdb, 0xfb); (* Û, û *)    (0xdc, 0xfc); (* Ü, ü *)
    (0xdd, 0xfd); (* Ý, ý *)    (0xde, 0xfe); (* Þ, þ *)
    (0x160, 0x161); (* Š, š *)  (0x17d, 0x17e); (* Ž, ž *)
    (0x152, 0x153); (* Œ, œ *)  (0x178, 0xff); (* Ÿ, ÿ *)
    (0x1e9e, 0xdf); (* ẞ, ß *)
  ]

  (* NFD to NFC conversion table for the letters above *)

  let known_pairs : (Uchar.t * Uchar.t, Uchar.t) Hashtbl.t = Hashtbl.create 32

  let _ =
    List.iter
      (fun (c1, n2, n) ->
        Hashtbl.add known_pairs
          (Uchar.of_char c1, Uchar.of_int n2) (Uchar.of_int n))
  [
    ('A', 0x300, 0xc0); (* À *)    ('A', 0x301, 0xc1); (* Á *)
    ('A', 0x302, 0xc2); (* Â *)    ('A', 0x303, 0xc3); (* Ã *)
    ('A', 0x308, 0xc4); (* Ä *)    ('A', 0x30a, 0xc5); (* Å *)
    ('C', 0x327, 0xc7); (* Ç *)    ('E', 0x300, 0xc8); (* È *)
    ('E', 0x301, 0xc9); (* É *)    ('E', 0x302, 0xca); (* Ê *)
    ('E', 0x308, 0xcb); (* Ë *)    ('I', 0x300, 0xcc); (* Ì *)
    ('I', 0x301, 0xcd); (* Í *)    ('I', 0x302, 0xce); (* Î *)
    ('I', 0x308, 0xcf); (* Ï *)    ('N', 0x303, 0xd1); (* Ñ *)
    ('O', 0x300, 0xd2); (* Ò *)    ('O', 0x301, 0xd3); (* Ó *)
    ('O', 0x302, 0xd4); (* Ô *)    ('O', 0x303, 0xd5); (* Õ *)
    ('O', 0x308, 0xd6); (* Ö *)
    ('U', 0x300, 0xd9); (* Ù *)    ('U', 0x301, 0xda); (* Ú *)
    ('U', 0x302, 0xdb); (* Û *)    ('U', 0x308, 0xdc); (* Ü *)
    ('Y', 0x301, 0xdd); (* Ý *)    ('Y', 0x308, 0x178);  (* Ÿ *)
    ('S', 0x30c, 0x160); (* Š *)   ('Z', 0x30c, 0x17d); (* Ž *)
    ('a', 0x300, 0xe0); (* à *)    ('a', 0x301, 0xe1); (* á *)
    ('a', 0x302, 0xe2); (* â *)    ('a', 0x303, 0xe3); (* ã *)
    ('a', 0x308, 0xe4); (* ä *)    ('a', 0x30a, 0xe5); (* å *)
    ('c', 0x327, 0xe7); (* ç *)    ('e', 0x300, 0xe8); (* è *)
    ('e', 0x301, 0xe9); (* é *)    ('e', 0x302, 0xea); (* ê *)
    ('e', 0x308, 0xeb); (* ë *)    ('i', 0x300, 0xec); (* ì *)
    ('i', 0x301, 0xed); (* í *)    ('i', 0x302, 0xee); (* î *)
    ('i', 0x308, 0xef); (* ï *)    ('n', 0x303, 0xf1); (* ñ *)
    ('o', 0x300, 0xf2); (* ò *)    ('o', 0x301, 0xf3); (* ó *)
    ('o', 0x302, 0xf4); (* ô *)    ('o', 0x303, 0xf5); (* õ *)
    ('o', 0x308, 0xf6); (* ö *)
    ('u', 0x300, 0xf9); (* ù *)    ('u', 0x301, 0xfa); (* ú *)
    ('u', 0x302, 0xfb); (* û *)    ('u', 0x308, 0xfc); (* ü *)
    ('y', 0x301, 0xfd); (* ý *)    ('y', 0x308, 0xff); (* ÿ *)
    ('s', 0x30c, 0x161); (* š *)   ('z', 0x30c, 0x17e); (* ž *)
  ]

  let normalize_generic ~keep_ascii transform s =
    let rec norm check buf prev i =
      if i >= String.length s then begin
        Buffer.add_utf_8_uchar buf (transform prev)
      end else begin
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        check d u;
        let i' = i + Uchar.utf_decode_length d in
        match Hashtbl.find_opt known_pairs (prev, u) with
        | Some u' ->
            norm check buf u' i'
        | None ->
            Buffer.add_utf_8_uchar buf (transform prev);
            norm check buf u i'
      end in
    let ascii_limit = 128 in
    if s = ""
    || keep_ascii && String.for_all (fun x -> Char.code x < ascii_limit) s
    then Ok s
    else
      let buf = Buffer.create (String.length s) in
      let valid = ref true in
      let check d u =
        valid := !valid && Uchar.utf_decode_is_valid d && u <> Uchar.rep
      in
      let d = String.get_utf_8_uchar s 0 in
      let u = Uchar.utf_decode_uchar d in
      check d u;
      norm check buf u (Uchar.utf_decode_length d);
      let contents = Buffer.contents buf in
      if !valid then
        Ok contents
      else
        Error contents

  let normalize s =
    normalize_generic ~keep_ascii:true (fun u -> u) s

  (* Capitalization *)

  let uchar_is_uppercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then c >= 65 && c <= 90 else
      match Hashtbl.find_opt known_chars u with
      | Some(Upper _) -> true
      | _ -> false

  let uchar_lowercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then
      if c >= 65 && c <= 90 then Uchar.of_int (c + 32) else u
    else
      match Hashtbl.find_opt known_chars u with
      | Some(Upper u') -> u'
      | _ -> u

  let uchar_uppercase u =
    let c = Uchar.to_int u in
    if c < 0x80 then
      if c >= 97 && c <= 122 then Uchar.of_int (c - 32) else u
    else
      match Hashtbl.find_opt known_chars u with
      | Some(Lower u') -> u'
      | _ -> u

  let capitalize s =
    let first = ref true in
    normalize_generic ~keep_ascii:false
      (fun u -> if !first then (first := false; uchar_uppercase u) else u)
      s

  let uncapitalize s =
    let first = ref true in
    normalize_generic ~keep_ascii:false
      (fun u -> if !first then (first := false; uchar_lowercase u) else u)
      s

  let is_capitalized s =
    s <> "" &&
    uchar_is_uppercase (Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0))

  (* Characters allowed in identifiers after normalization is applied.
     Currently:
       - ASCII letters, underscore
       - Latin-9 letters, represented in NFC
       - ASCII digits, single quote (but not as first character)
       - dot if [with_dot] = true
  *)
  let uchar_valid_in_identifier ~with_dot u =
    let c = Uchar.to_int u in
    if c < 0x80 then
         c >= 97 (* a *) && c <= 122 (* z *)
      || c >= 65 (* A *) && c <= 90 (* Z *)
      || c >= 48 (* 0 *) && c <= 57 (* 9 *)
      || c = 95 (* underscore *)
      || c = 39 (* single quote *)
      || (with_dot && c = 46) (* dot *)
    else
      Hashtbl.mem known_chars u

  let uchar_not_identifier_start u =
    let c = Uchar.to_int u in
       c >= 48 (* 0 *) && c <= 57 (* 9 *)
    || c = 39  (* single quote *)

  (* Check whether a normalized string is a valid OCaml identifier. *)

  type validation_result =
    | Valid
    | Invalid_character of Uchar.t   (** Character not allowed *)
    | Invalid_beginning of Uchar.t   (** Character not allowed as first char *)

  let validate_identifier ?(with_dot=false) s =
    let rec check i =
      if i >= String.length s then Valid else begin
        let d = String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        let i' = i + Uchar.utf_decode_length d in
        if not (uchar_valid_in_identifier ~with_dot u) then
          Invalid_character u
        else if i = 0 && uchar_not_identifier_start u then
          Invalid_beginning u
        else
          check i'
      end
    in check 0

  let is_valid_identifier s =
    validate_identifier s = Valid

  let starts_like_a_valid_identifier s =
    s <> "" &&
    (let u = Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0) in
     uchar_valid_in_identifier ~with_dot:false u
     && not (uchar_not_identifier_start u))

  let is_lowercase s =
    let rec is_lowercase_at len s n =
      if n >= len then true
      else
        let d = String.get_utf_8_uchar s n in
        let u = Uchar.utf_decode_uchar d in
        (uchar_valid_in_identifier ~with_dot:false  u)
        && not (uchar_is_uppercase u)
        && is_lowercase_at len s (n+Uchar.utf_decode_length d)
    in
    is_lowercase_at (String.length s) s 0
end
#endif
