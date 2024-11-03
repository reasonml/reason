#if OCAML_VERSION >= (5,3,0)
include Format_doc

#else
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Doc = struct

  type box_type =
    | H
    | V
    | HV
    | HoV
    | B

  type stag =
#if OCAML_VERSION >= (4,08,0)
    Format.stag
#else
    Format.tag
#endif

  type element =
    | Text of string
    | With_size of int
    | Open_box of { kind: box_type ; indent:int }
    | Close_box
    | Open_tag of
#if OCAML_VERSION >= (4,08,0)
    Format.stag
#else
    Format.tag
#endif
    | Close_tag
    | Open_tbox
    | Tab_break of { width : int; offset : int }
    | Set_tab
    | Close_tbox
    | Simple_break of { spaces : int; indent: int }
    | Break of { fits : string * int * string as 'a; breaks : 'a }
    | Flush of { newline:bool }
    | Newline
    | If_newline

    | Deprecated of (Format.formatter -> unit)

  type t = { rev:element list } [@@unboxed]

  let empty = { rev = [] }

  let to_list doc = List.rev doc.rev
  let add doc x = { rev = x :: doc.rev }
  let fold f acc doc = List.fold_left f acc (to_list doc)
  let append left right = { rev = right.rev @ left.rev }

  let format_open_box_gen ppf kind indent =
    match kind with
    | H-> Format.pp_open_hbox ppf ()
    | V -> Format.pp_open_vbox ppf indent
    | HV -> Format.pp_open_hvbox ppf indent
    | HoV -> Format.pp_open_hovbox ppf indent
    | B -> Format.pp_open_box ppf indent

  let interpret_elt ppf = function
    | Text x -> Format.pp_print_string ppf x
    | Open_box { kind; indent } -> format_open_box_gen ppf kind indent
    | Close_box -> Format.pp_close_box ppf ()
    | Open_tag tag ->
#if OCAML_VERSION >= (4,08,0)
        Format.pp_open_stag ppf tag
#else
        Format.pp_open_tag ppf tag
#endif
    | Close_tag ->
#if OCAML_VERSION >= (4,08,0)
        Format.pp_close_stag ppf ()
#else
        Format.pp_close_tag ppf ()
#endif
    | Open_tbox -> Format.pp_open_tbox ppf ()
    | Tab_break {width;offset} -> Format.pp_print_tbreak ppf width offset
    | Set_tab -> Format.pp_set_tab ppf ()
    | Close_tbox -> Format.pp_close_tbox ppf ()
    | Simple_break {spaces;indent} -> Format.pp_print_break ppf spaces indent
    | Break {fits;breaks} ->
#if OCAML_VERSION >= (4,08,0)
        Format.pp_print_custom_break ppf ~fits ~breaks
#else
        let (_, width, _) = fits in
        let (_, offset, _) = breaks in
        Format.pp_print_break ppf width offset
#endif
    | Flush {newline=true} -> Format.pp_print_newline ppf ()
    | Flush {newline=false} -> Format.pp_print_flush ppf ()
    | Newline -> Format.pp_force_newline ppf ()
    | If_newline -> Format.pp_print_if_newline ppf ()
    | With_size _ ->  ()
    | Deprecated pr -> pr ppf

  let rec interpret ppf = function
    | [] -> ()
    | With_size size :: Text text :: l ->
        Format.pp_print_as ppf size text;
        interpret ppf l
    | x :: l ->
        interpret_elt ppf x;
        interpret ppf l

  let format ppf doc = interpret ppf (to_list doc)



  let open_box kind indent doc = add doc (Open_box {kind;indent})
  let close_box doc = add doc Close_box

  let string s doc = add doc (Text s)
  let bytes b doc = add doc (Text (Bytes.to_string b))
  let with_size size doc = add doc (With_size size)

  let int n doc = add doc (Text (string_of_int n))
  let float f doc = add doc (Text (string_of_float f))
  let char c doc = add doc (Text (String.make 1 c))
#if OCAML_VERSION >= (4,08,0)
  let bool c doc = add doc (Text (Bool.to_string c))
#else
  let bool c doc = add doc (Text (string_of_bool c))
#endif

  let break ~spaces ~indent doc = add doc (Simple_break {spaces; indent})
  let space doc = break ~spaces:1 ~indent:0 doc
  let cut = break ~spaces:0 ~indent:0

  let custom_break ~fits ~breaks doc = add doc (Break {fits;breaks})

  let force_newline doc = add doc Newline
  let if_newline doc = add doc If_newline

  let flush doc = add doc (Flush {newline=false})
  let force_stop doc = add doc (Flush {newline=true})

  let open_tbox doc = add doc Open_tbox
  let set_tab doc = add doc Set_tab
  let tab_break ~width ~offset doc = add doc (Tab_break {width;offset})
  let tab doc = tab_break ~width:0 ~offset:0 doc
  let close_tbox doc = add doc Close_tbox

  let open_tag stag doc = add doc (Open_tag stag)
  let close_tag doc = add doc Close_tag

  let iter ?(sep=Fun.id) ~iter:iterator elt l doc =
    let first = ref true in
    let rdoc = ref doc in
    let print x =
      if !first then (first := false; rdoc := elt x !rdoc)
      else rdoc := !rdoc |> sep |> elt x
    in
    iterator print l;
    !rdoc

  let rec list ?(sep=Fun.id) elt l doc = match l with
    | [] -> doc
    | [a] -> elt a doc
    | a :: ((_ :: _) as q) ->
        doc |> elt a |> sep |> list ~sep elt q

  let array ?sep elt a doc = iter ?sep ~iter:Array.iter elt a doc
  let seq ?sep elt s doc = iter ?sep ~iter:Seq.iter elt s doc

  let option ?(none=Fun.id) elt o doc = match o with
    | None -> none doc
    | Some x -> elt x doc

#if OCAML_VERSION >= (4,12,0)
  let either ~left ~right x doc = match x with
    | Either.Left x -> left x doc
    | Either.Right x -> right x doc
#endif

  let result ~ok ~error x doc = match x with
    | Ok x -> ok x doc
    | Error x -> error x doc

  (* To format free-flowing text *)
  let rec subtext len left right s doc =
    let flush doc =
      doc |> string (String.sub s left (right - left))
    in
    let after_flush doc = subtext len (right+1) (right+1) s doc in
    if right = len then
      if left <> len then flush doc else doc
    else
      match s.[right] with
      | '\n' ->
          doc |> flush |> force_newline |> after_flush
      | ' ' ->
          doc |> flush |> space |> after_flush
      (* there is no specific support for '\t'
         as it is unclear what a right semantics would be *)
      | _ -> subtext len left (right + 1) s doc

  let text s doc =
    subtext (String.length s) 0 0 s doc

  type ('a,'b) fmt = ('a, t, t, 'b) format4
  type printer0 = t -> t
  type 'a printer = 'a -> printer0

  let output_formatting_lit fmting_lit doc =
    let open CamlinternalFormatBasics in
    match fmting_lit with
    | Close_box    -> close_box doc
    | Close_tag                 -> close_tag doc
    | Break (_, width, offset)  -> break ~spaces:width ~indent:offset doc
    | FFlush                    -> flush doc
    | Force_newline             -> force_newline doc
    | Flush_newline             -> force_stop doc
    | Magic_size (_, n)         -> with_size n doc
    | Escaped_at                -> char '@' doc
    | Escaped_percent           -> char '%' doc
    | Scan_indic c              -> doc |> char '@' |> char c

  let to_string doc =
    let b = Buffer.create 20 in
    let convert = function
      | Text s -> Buffer.add_string b s
      | _ -> ()
    in
    fold (fun () x -> convert x) () doc;
    Buffer.contents b

  let box_type =
    let open CamlinternalFormatBasics in
    function
    | Pp_fits -> H
    | Pp_hbox -> H
    | Pp_vbox -> V
    | Pp_hovbox -> HoV
    | Pp_hvbox -> HV
    | Pp_box -> B

  let rec compose_acc acc doc =
    let open CamlinternalFormat in
    match acc with
    | CamlinternalFormat.Acc_formatting_lit (p, f) ->
        doc |> compose_acc p |> output_formatting_lit f
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
        let tag = to_string (compose_acc acc' empty) in
        let doc = compose_acc p doc in
        doc |> open_tag (Format.String_tag tag)
    | Acc_formatting_gen (p, Acc_open_box acc') ->
        let doc = compose_acc p doc in
        let box = to_string (compose_acc acc' empty) in
        let (indent, bty) = CamlinternalFormat.open_box_of_string box in
        doc |> open_box (box_type bty) indent
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   ->
        doc |> compose_acc p |> string s
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> doc |> compose_acc p |> char c
    | Acc_delay (p, f)         -> doc |> compose_acc p |> f
    | Acc_flush p              -> doc |> compose_acc p |> flush
    | Acc_invalid_arg (_p, msg) ->  invalid_arg msg;
    | End_of_acc               -> doc

  let kprintf k (CamlinternalFormatBasics.Format (fmt, _))  =
    CamlinternalFormat.make_printf
      (fun acc doc -> doc |> compose_acc acc |> k )
      End_of_acc fmt

  let printf doc = kprintf Fun.id doc
  let kmsg k  (CamlinternalFormatBasics.Format (fmt, _)) =
    CamlinternalFormat.make_printf
      (fun acc -> k (compose_acc acc empty))
      End_of_acc fmt

  let msg fmt = kmsg Fun.id fmt

end

(** Compatibility interface *)

type doc = Doc.t
type t = doc
type formatter = doc ref
type 'a printer = formatter -> 'a -> unit

let formatter d = d

(** {1 Primitive functions }*)

let pp_print_string ppf s = ppf := Doc.string s !ppf

let pp_print_as ppf size s =
  ppf := !ppf |> Doc.with_size size |> Doc.string s

let pp_print_substring ~pos ~len ppf s =
 ppf := Doc.string (String.sub s pos len) !ppf

let pp_print_substring_as ~pos ~len ppf size s =
  ppf :=
  !ppf
  |> Doc.with_size size
  |> Doc.string (String.sub s pos len)

let pp_print_bytes ppf s = ppf := Doc.string (Bytes.to_string s) !ppf
let pp_print_text ppf s = ppf := Doc.text s !ppf
let pp_print_char ppf c = ppf := Doc.char c !ppf
let pp_print_int ppf c = ppf := Doc.int c !ppf
let pp_print_float ppf f = ppf := Doc.float f !ppf
let pp_print_bool ppf b = ppf := Doc.bool b !ppf
let pp_print_nothing _ _ = ()

let pp_close_box ppf () = ppf := Doc.close_box !ppf
let pp_close_stag ppf () = ppf := Doc.close_tag !ppf

let pp_print_break ppf spaces indent = ppf := Doc.break ~spaces ~indent !ppf

let pp_print_custom_break ppf ~fits ~breaks =
  ppf := Doc.custom_break ~fits ~breaks !ppf

let pp_print_space ppf () = pp_print_break ppf 1 0
let pp_print_cut ppf () = pp_print_break ppf 0 0

let pp_print_flush ppf () = ppf := Doc.flush !ppf
let pp_force_newline ppf () = ppf := Doc.force_newline !ppf
let pp_print_newline ppf () = ppf := Doc.force_stop !ppf
let pp_print_if_newline ppf () =ppf := Doc.if_newline !ppf

let pp_open_stag ppf stag = ppf := !ppf |> Doc.open_tag stag

let pp_open_box_gen ppf indent bxty =
  let box_type = Doc.box_type bxty in
   ppf := !ppf |> Doc.open_box box_type indent

let pp_open_box ppf indent = pp_open_box_gen ppf indent Pp_box


let pp_open_tbox ppf () = ppf := !ppf |> Doc.open_tbox

let pp_close_tbox ppf () = ppf := !ppf |> Doc.close_tbox

let pp_set_tab ppf () = ppf := !ppf |> Doc.set_tab

let pp_print_tab ppf () = ppf := !ppf |> Doc.tab

let pp_print_tbreak ppf width offset =
  ppf := !ppf |> Doc.tab_break ~width ~offset

let pp_doc ppf doc = ppf := Doc.append !ppf doc

module Driver = struct
  (* Interpret a formatting entity on a formatter. *)
  let output_formatting_lit ppf
      (fmting_lit:CamlinternalFormatBasics.formatting_lit)
    = match fmting_lit with
    | Close_box                 -> pp_close_box ppf ()
    | Close_tag                 ->
#if OCAML_VERSION >= (4,08,0)
        pp_close_stag ppf ()
#else
        pp_close_tag ppf ()
#endif
    | Break (_, width, offset)  -> pp_print_break ppf width offset
    | FFlush                    -> pp_print_flush ppf ()
    | Force_newline             -> pp_force_newline ppf ()
    | Flush_newline             -> pp_print_newline ppf ()
    | Magic_size (_, _)         -> ()
    | Escaped_at                -> pp_print_char ppf '@'
    | Escaped_percent           -> pp_print_char ppf '%'
    | Scan_indic c              -> pp_print_char ppf '@'; pp_print_char ppf c



  let compute_tag output tag_acc =
    let buf = Buffer.create 16 in
    let buf_fmt = Format.formatter_of_buffer buf in
    let ppf = ref Doc.empty in
    output ppf tag_acc;
    pp_print_flush ppf ();
    Doc.format buf_fmt !ppf;
    let len = Buffer.length buf in
    if len < 2 then Buffer.contents buf
    else Buffer.sub buf 1 (len - 2)

  (* Recursively output an "accumulator" containing a reversed list of
     printing entities (string, char, flus, ...) in an output_stream. *)
  (* Differ from Printf.output_acc by the interpretation of formatting. *)
  (* Used as a continuation of CamlinternalFormat.make_printf. *)
  let rec output_acc ppf (acc: _ CamlinternalFormat.acc) =
    match acc with
    | Acc_string_literal (Acc_formatting_lit (p, Magic_size (_, size)), s)
    | Acc_data_string (Acc_formatting_lit (p, Magic_size (_, size)), s) ->
        output_acc ppf p;
        pp_print_as ppf size s;
    | Acc_char_literal (Acc_formatting_lit (p, Magic_size (_, size)), c)
    | Acc_data_char (Acc_formatting_lit (p, Magic_size (_, size)), c) ->
        output_acc ppf p;
        pp_print_as ppf size (String.make 1 c);
    | Acc_formatting_lit (p, f) ->
        output_acc ppf p;
        output_formatting_lit ppf f;
    | Acc_formatting_gen (p, Acc_open_tag acc') ->
        output_acc ppf p;
#if OCAML_VERSION >= (4,08,0)
        pp_open_stag ppf (Format.String_tag (compute_tag output_acc acc'))
#else
        pp_open_stag ppf (compute_tag output_acc acc')
#endif
    | Acc_formatting_gen (p, Acc_open_box acc') ->
        output_acc ppf p;
        let (indent, bty) =
          let box_info = compute_tag output_acc acc' in
          CamlinternalFormat.open_box_of_string box_info
        in
        pp_open_box_gen ppf indent bty
    | Acc_string_literal (p, s)
    | Acc_data_string (p, s)   -> output_acc ppf p; pp_print_string ppf s;
    | Acc_char_literal (p, c)
    | Acc_data_char (p, c)     -> output_acc ppf p; pp_print_char ppf c;
    | Acc_delay (p, f)         -> output_acc ppf p; f ppf;
    | Acc_flush p              -> output_acc ppf p; pp_print_flush ppf ();
    | Acc_invalid_arg (p, msg) -> output_acc ppf p; invalid_arg msg;
    | End_of_acc               -> ()
end

let kfprintf k ppf (CamlinternalFormatBasics.Format (fmt, _))  =
  CamlinternalFormat.make_printf
    (fun acc -> Driver.output_acc ppf acc; k ppf)
    End_of_acc fmt
let fprintf doc fmt = kfprintf ignore doc fmt


let kdprintf k (CamlinternalFormatBasics.Format (fmt, _)) =
  CamlinternalFormat.make_printf
    (fun acc -> k (fun ppf -> Driver.output_acc ppf acc))
    End_of_acc fmt

let dprintf fmt = kdprintf (fun i -> i) fmt

let doc_printf fmt =
  let ppf = ref Doc.empty in
  kfprintf (fun _ -> let doc = !ppf in ppf := Doc.empty; doc) ppf fmt

let kdoc_printf k fmt =
  let ppf = ref Doc.empty in
  kfprintf (fun ppf ->
      let doc = !ppf in
      ppf := Doc.empty;
      k doc
    )
    ppf fmt

let doc_printer f x doc =
  let r = ref doc in
  f r x;
  !r

type 'a format_printer = Format.formatter -> 'a -> unit

let format_printer f ppf x =
  let doc = doc_printer f x Doc.empty in
  Doc.format ppf doc
let compat = format_printer
let compat1 f p1 = compat (f p1)
let compat2 f p1 p2 = compat (f p1 p2)

let kasprintf k fmt =
  kdoc_printf (fun doc -> k (Format.asprintf "%a" Doc.format doc)) fmt
let asprintf fmt = kasprintf Fun.id fmt

let pp_print_iter ?(pp_sep=pp_print_cut) iter elt ppf c =
      let sep = doc_printer pp_sep () in
      ppf:= Doc.iter ~sep ~iter (doc_printer elt) c !ppf

let pp_print_list ?(pp_sep=pp_print_cut) elt ppf l =
  ppf := Doc.list ~sep:(doc_printer pp_sep ()) (doc_printer elt) l !ppf

let pp_print_array ?pp_sep elt ppf a =
  pp_print_iter ?pp_sep Array.iter elt ppf a
let pp_print_seq ?pp_sep elt ppf s = pp_print_iter ?pp_sep Seq.iter elt ppf s

let pp_print_option  ?(none=fun _ () -> ()) elt ppf o =
  ppf := Doc.option ~none:(doc_printer none ()) (doc_printer elt) o !ppf

let pp_print_result  ~ok ~error ppf r =
   ppf := Doc.result ~ok:(doc_printer ok) ~error:(doc_printer error) r !ppf

#if OCAML_VERSION >= (4,12,0)
let pp_print_either  ~left ~right ppf e =
  ppf := Doc.either ~left:(doc_printer left) ~right:(doc_printer right) e !ppf
#endif

let comma ppf () = fprintf ppf ",@ "

let pp_two_columns ?(sep = "|") ?max_lines ppf (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  fprintf ppf "@[<v>";
  List.iteri (fun k (line_l, line_r) ->
      if k = ellipsed_first then fprintf ppf "...@,";
      if ellipsed_first <= k && k <= ellipsed_last then ()
      else fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r
    ) lines;
  fprintf ppf "@]"

let deprecated_printer pr ppf = ppf := Doc.add !ppf (Doc.Deprecated pr)
#endif
