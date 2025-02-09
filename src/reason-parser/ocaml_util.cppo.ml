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

#if OCAML_VERSION < (4,13,0)
module Uchar = struct
  include Uchar

  let valid_bit = 27
  let decode_bits = 24

  let[@inline] utf_decode_is_valid d = (d lsr valid_bit) = 1
  let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111
  let[@inline] utf_decode_uchar d = unsafe_of_int (d land 0xFFFFFF)
  let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor (to_int u)
  let rep = 0xFFFD
  let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor rep

  let rep = Uchar.rep
end

module Bytes = struct
  include Bytes

  external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
  external unsafe_get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16u"
  external get_uint8 : bytes -> int -> int = "%bytes_safe_get"
  external get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16"
  external get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32"
  external get_int64_ne : bytes -> int -> int64 = "%caml_bytes_get64"

  external unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
  external unsafe_set_uint16_ne : bytes -> int -> int -> unit
                                = "%caml_bytes_set16u"
  external set_int8 : bytes -> int -> int -> unit = "%bytes_safe_set"
  external set_int16_ne : bytes -> int -> int -> unit = "%caml_bytes_set16"
  external swap16 : int -> int = "%bswap16"
  external swap32 : int32 -> int32 = "%bswap_int32"
  external swap64 : int64 -> int64 = "%bswap_int64"

  let unsafe_get_uint16_le b i =
    if Sys.big_endian
    then swap16 (unsafe_get_uint16_ne b i)
    else unsafe_get_uint16_ne b i

  let unsafe_get_uint16_be b i =
    if Sys.big_endian
    then unsafe_get_uint16_ne b i
    else swap16 (unsafe_get_uint16_ne b i)

  let get_int8 b i =
    ((get_uint8 b i) lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

  let get_uint16_le b i =
    if Sys.big_endian then swap16 (get_uint16_ne b i)
    else get_uint16_ne b i

  let get_uint16_be b i =
    if not Sys.big_endian then swap16 (get_uint16_ne b i)
    else get_uint16_ne b i

  let get_int16_ne b i =
    ((get_uint16_ne b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let get_int16_le b i =
    ((get_uint16_le b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let get_int16_be b i =
    ((get_uint16_be b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let get_int32_le b i =
    if Sys.big_endian then swap32 (get_int32_ne b i)
    else get_int32_ne b i

  let get_int32_be b i =
    if not Sys.big_endian then swap32 (get_int32_ne b i)
    else get_int32_ne b i

  let get_int64_le b i =
    if Sys.big_endian then swap64 (get_int64_ne b i)
    else get_int64_ne b i

  let get_int64_be b i =
    if not Sys.big_endian then swap64 (get_int64_ne b i)
    else get_int64_ne b i

  let unsafe_set_uint16_le b i x =
    if Sys.big_endian
    then unsafe_set_uint16_ne b i (swap16 x)
    else unsafe_set_uint16_ne b i x

  let unsafe_set_uint16_be b i x =
    if Sys.big_endian
    then unsafe_set_uint16_ne b i x else
    unsafe_set_uint16_ne b i (swap16 x)

  let set_int16_le b i x =
    if Sys.big_endian then set_int16_ne b i (swap16 x)
    else set_int16_ne b i x

  let set_int16_be b i x =
    if not Sys.big_endian then set_int16_ne b i (swap16 x)
    else set_int16_ne b i x

  let set_uint8 = set_int8
  let set_uint16_ne = set_int16_ne
  let set_uint16_be = set_int16_be
  let set_uint16_le = set_int16_le

  (* UTF codecs and validations *)

  let dec_invalid = Uchar.utf_decode_invalid
  let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)


  let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10
  let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101
  let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100
  let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b
  let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8

  let[@inline] utf_8_uchar_2 b0 b1 =
    ((b0 land 0x1F) lsl 6) lor
    ((b1 land 0x3F))

  let[@inline] utf_8_uchar_3 b0 b1 b2 =
    ((b0 land 0x0F) lsl 12) lor
    ((b1 land 0x3F) lsl 6) lor
    ((b2 land 0x3F))

  let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18) lor
    ((b1 land 0x3F) lsl 12) lor
    ((b2 land 0x3F) lsl 6) lor
    ((b3 land 0x3F))

  let get_utf_8_uchar b i =
    let b0 = get_uint8 b i in (* raises if [i] is not a valid index. *)
    let get = unsafe_get_uint8 in
    let max = length b - 1 in
    match Char.unsafe_chr b0 with (* See The Unicode Standard, Table 3.7 *)
    | '\x00' .. '\x7F' -> dec_ret 1 b0
    | '\xC2' .. '\xDF' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
        dec_ret 2 (utf_8_uchar_2 b0 b1)
    | '\xE0' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_xA0_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xED' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_x9F b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xF0' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x90_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        let i = i + 1 in if i > max then dec_invalid 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF1' .. '\xF3' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_xBF b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        let i = i + 1 in if i > max then dec_invalid 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF4' ->
        let i = i + 1 in if i > max then dec_invalid 1 else
        let b1 = get b i in if not_in_x80_to_x8F b1 then dec_invalid 1 else
        let i = i + 1 in if i > max then dec_invalid 2 else
        let b2 = get b i in if not_in_x80_to_xBF b2 then dec_invalid 2 else
        let i = i + 1 in if i > max then dec_invalid 3 else
        let b3 = get b i in if not_in_x80_to_xBF b3 then dec_invalid 3 else
        dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | _ -> dec_invalid 1

  let set_utf_8_uchar b i u =
    let set = unsafe_set_uint8 in
    let max = length b - 1 in
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0x007F ->
        set_uint8 b i u;
        1
    | u when u <= 0x07FF ->
        let last = i + 1 in
        if last > max then 0 else
        (set_uint8 b i (0xC0 lor (u lsr 6));
         set b last (0x80 lor (u land 0x3F));
         2)
    | u when u <= 0xFFFF ->
        let last = i + 2 in
        if last > max then 0 else
        (set_uint8 b i (0xE0 lor (u lsr 12));
         set b (i + 1) (0x80 lor ((u lsr 6) land 0x3F));
         set b last (0x80 lor (u land 0x3F));
         3)
    | u when u <= 0x10FFFF ->
        let last = i + 3 in
        if last > max then 0 else
        (set_uint8 b i (0xF0 lor (u lsr 18));
         set b (i + 1) (0x80 lor ((u lsr 12) land 0x3F));
         set b (i + 2) (0x80 lor ((u lsr 6) land 0x3F));
         set b last (0x80 lor (u land 0x3F));
         4)
    | _ -> assert false

  let is_valid_utf_8 b =
    let rec loop max b i =
      if i > max then true else
      let get = unsafe_get_uint8 in
      match Char.unsafe_chr (get b i) with
      | '\x00' .. '\x7F' -> loop max b (i + 1)
      | '\xC2' .. '\xDF' ->
          let last = i + 1 in
          if last > max
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | '\xE0' ->
          let last = i + 2 in
          if last > max
          || not_in_xA0_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
          let last = i + 2 in
          if last > max
          || not_in_x80_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | '\xED' ->
          let last = i + 2 in
          if last > max
          || not_in_x80_to_x9F (get b (i + 1))
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | '\xF0' ->
          let last = i + 3 in
          if last > max
          || not_in_x90_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | '\xF1' .. '\xF3' ->
          let last = i + 3 in
          if last > max
          || not_in_x80_to_xBF (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | '\xF4' ->
          let last = i + 3 in
          if last > max
          || not_in_x80_to_x8F (get b (i + 1))
          || not_in_x80_to_xBF (get b (i + 2))
          || not_in_x80_to_xBF (get b last)
          then false
          else loop max b (last + 1)
      | _ -> false
    in
    loop (length b - 1) b 0

  (* UTF-16BE *)

  let get_utf_16be_uchar b i =
    let get = unsafe_get_uint16_be in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    if i = max then dec_invalid 1 else
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
    | u when u > 0xDBFF -> dec_invalid 2
    | hi -> (* combine [hi] with a low surrogate *)
        let last = i + 3 in
        if last > max then dec_invalid (max - i + 1) else
        match get b (i + 2) with
        | u when u < 0xDC00 || u > 0xDFFF -> dec_invalid 2 (* retry here *)
        | lo ->
            let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
            dec_ret 4 u

  let set_utf_16be_uchar b i u =
    let set = unsafe_set_uint16_be in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0xFFFF ->
        let last = i + 1 in
        if last > max then 0 else (set b i u; 2)
    | u when u <= 0x10FFFF ->
        let last = i + 3 in
        if last > max then 0 else
        let u' = u - 0x10000 in
        let hi = (0xD800 lor (u' lsr 10)) in
        let lo = (0xDC00 lor (u' land 0x3FF)) in
        set b i hi; set b (i + 2) lo; 4
    | _ -> assert false

  let is_valid_utf_16be b =
    let rec loop max b i =
      let get = unsafe_get_uint16_be in
      if i > max then true else
      if i = max then false else
      match get b i with
      | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
      | u when u > 0xDBFF -> false
      | _hi ->
          let last = i + 3 in
          if last > max then false else
          match get b (i + 2) with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | _lo -> loop max b (i + 4)
    in
    loop (length b - 1) b 0

  (* UTF-16LE *)

  let get_utf_16le_uchar b i =
    let get = unsafe_get_uint16_le in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    if i = max then dec_invalid 1 else
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
    | u when u > 0xDBFF -> dec_invalid 2
    | hi -> (* combine [hi] with a low surrogate *)
        let last = i + 3 in
        if last > max then dec_invalid (max - i + 1) else
        match get b (i + 2) with
        | u when u < 0xDC00 || u > 0xDFFF -> dec_invalid 2 (* retry here *)
        | lo ->
            let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
            dec_ret 4 u

  let set_utf_16le_uchar b i u =
    let set = unsafe_set_uint16_le in
    let max = length b - 1 in
    if i < 0 || i > max then invalid_arg "index out of bounds" else
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0xFFFF ->
        let last = i + 1 in
        if last > max then 0 else (set b i u; 2)
    | u when u <= 0x10FFFF ->
        let last = i + 3 in
        if last > max then 0 else
        let u' = u - 0x10000 in
        let hi = (0xD800 lor (u' lsr 10)) in
        let lo = (0xDC00 lor (u' land 0x3FF)) in
        set b i hi; set b (i + 2) lo; 4
    | _ -> assert false

  let is_valid_utf_16le b =
    let rec loop max b i =
      let get = unsafe_get_uint16_le in
      if i > max then true else
      if i = max then false else
      match get b i with
      | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
      | u when u > 0xDBFF -> false
      | _hi ->
          let last = i + 3 in
          if last > max then false else
          match get b (i + 2) with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | _lo -> loop max b (i + 4)
    in
    loop (length b - 1) b 0
end

module String = struct
  include String

  module B = struct
    include Bytes
    let for_all p s =
      let n = length s in
      let rec loop i =
        if i = n then true
        else if p (unsafe_get s i) then loop (succ i)
        else false in
      loop 0
  end

  let bos = B.unsafe_of_string

  let for_all f s =
    B.for_all f (bos s)

  let get_utf_8_uchar s i = B.get_utf_8_uchar (bos s) i
  let is_valid_utf_8 s = B.is_valid_utf_8 (bos s)

  let get_utf_16be_uchar s i = B.get_utf_16be_uchar (bos s) i
  let is_valid_utf_16be s = B.is_valid_utf_16be (bos s)

  let get_utf_16le_uchar s i = B.get_utf_16le_uchar (bos s) i
  let is_valid_utf_16le s = B.is_valid_utf_16le (bos s)

  (** {6 Binary encoding/decoding of integers} *)

  external get_uint8 : string -> int -> int = "%string_safe_get"
  external get_uint16_ne : string -> int -> int = "%caml_string_get16"
  external get_int32_ne : string -> int -> int32 = "%caml_string_get32"
  external get_int64_ne : string -> int -> int64 = "%caml_string_get64"

  let get_int8 s i = B.get_int8 (bos s) i
  let get_uint16_le s i = B.get_uint16_le (bos s) i
  let get_uint16_be s i = B.get_uint16_be (bos s) i
  let get_int16_ne s i = B.get_int16_ne (bos s) i
  let get_int16_le s i = B.get_int16_le (bos s) i
  let get_int16_be s i = B.get_int16_be (bos s) i
  let get_int32_le s i = B.get_int32_le (bos s) i
  let get_int32_be s i = B.get_int32_be (bos s) i
  let get_int64_le s i = B.get_int64_le (bos s) i
  let get_int64_be s i = B.get_int64_be (bos s) i
end
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
