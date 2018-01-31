module Comment = Reason_comment

type easyFormatLabelFormatter = Easy_format.t -> Easy_format.t -> Easy_format.t

(* Make a standard list *)
type whenToDoSomething =
  | Never
  | IfNeed
  | Always
  (* Always_rec not only will break, it will break recursively up to the root *)
  | Always_rec

(*
 Modeling separators:
  Special ability to render the final separator distinctly. This is so we can
  replace them when they do/don't occur next to newlines.

    If sepLeft:true
    {
      final item1
      sep   item2
      sep   item3
    }

    If sepLeft:false
    {
      item1 sep
      item2 sep
      item3 final
    }
*)
(* You can't determine the final separator unless you specify a separator *)
type separator =
  | NoSep
  | Sep of string
  | SepFinal of string * string

(**
 * These represent "intent to format" the AST, with some parts being annotated
 * with original source location. The benefit of tracking this in an
 * intermediate structure, is that we can then interleave comments throughout
 * the tree before generating the final representation. That prevents the
 * formatting code from having to thread comments everywhere.
 *
 * The final representation is rendered using Easy_format.
 *)
type t =
  | SourceMap of Location.t * t (* a layout with location info *)
  | Sequence of config * (t list)
  | Label of easyFormatLabelFormatter * t * t
  | Easy of Easy_format.t

and config = {
  (* Newlines above items that do not have any comments immediately above it.
     Only really useful when used with break:Always/Always_rec *)
  newlinesAboveItems: int;
  (* Newlines above regular comments *)
  newlinesAboveComments: int;
  (* Newlines above doc comments *)
  newlinesAboveDocComments: int;
  (* If you are only grouping something for the sake of visual appearance, and
   * not forming an actual conceptual sequence of items, then this is often
   * useful. For example, if you're appending a semicolon etc. *)
  interleaveComments: bool;
  break: whenToDoSomething;
  (* Break setting that becomes activated if a comment becomes interleaved into
   * this list. Typically, if not specified, the behavior from [break] will be
   * used.
   *)
  wrap: string * string;
  inline: bool * bool;
  sep: separator;
  indent: int;
  sepLeft: bool;
  preSpace: bool;
  (* Really means space_after_separator *)
  postSpace: bool;
  pad: bool * bool;
  (* A function, because the system might rearrange your previous settings, and
   * a function allows you to not be locked into some configuration that is made
   * out of date by the formatting system (suppose it removes the separator
   * token etc.) Having a function allows you to instruct our formatter how to
   * extend the "freshest" notion of the list config when comments are
   * interleaved. *)
  listConfigIfCommentsInterleaved: (config -> config) option;

  (* Formatting to use if an item in a list had an end-of-line comment appended *)
  listConfigIfEolCommentsInterleaved: (config -> config) option;
}

let string_of_easy = function
  | Easy_format.Atom (s,_) -> s
  | Easy_format.List (_,_) -> "list"
  | Easy_format.Label (_,_) -> "label"
  | Easy_format.Custom _ -> "custom"

let indent_more indent = "  " ^ indent

let dump_easy ppf easy =
  let printf fmt = Format.fprintf ppf fmt in
  let rec traverse indent = function
    | Easy_format.Atom (s,_) ->
      printf "%s Atom:'%s'\n" indent s
    | Easy_format.List ((opening, sep, closing, config), items) ->
      let break = (match config.wrap_body with
          | `No_breaks -> "No_breaks"
          | `Wrap_atoms -> "Wrap_atoms"
          | `Never_wrap -> "Never_wrap"
          | `Force_breaks -> "Force_breaks"
          | `Force_breaks_rec -> "Force_breaks_rec"
          | `Always_wrap -> "Always_wrap") in
      printf "%s List: open %s close %s sep %s break %s \n"
        indent opening closing sep break;
      let _ = List.map (traverse (indent_more indent)) items in
      ()
    | Easy_format.Label ((left, config), right) ->
      let break = match config.label_break with
        | `Never -> "Never"
        | `Always_rec -> "Always_rec"
        | `Auto -> "Auto"
        | `Always -> "Always" in
      printf "%s Label (break = %s): \n" indent break;
      printf "  %s left \n" indent;
      let indent' = indent_more indent in
      traverse indent' left;
      printf "  %s right \n" indent;
      traverse indent' right;
    | Easy_format.Custom _ ->
      printf "custom \n"
  in
  traverse "" easy

let dump ppf layout =
  let printf fmt = Format.fprintf ppf fmt in
  let rec traverse indent = function
    | SourceMap (loc, layout) ->
      printf "%s SourceMap [(%d:%d)-(%d:%d)]\n" indent
        loc.loc_start.Lexing.pos_lnum
        (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
        loc.loc_end.Lexing.pos_lnum
        (loc.loc_end.Lexing.pos_cnum - loc.loc_end.Lexing.pos_bol);
      traverse (indent_more indent) layout
    | Sequence (config, layout_list) ->
      let break = match config.break with
        | Never  -> "Never"
        | IfNeed  -> "if need"
        | Always  -> "Always"
        | Always_rec  -> "Always_rec" in
      let sep = match config.sep with
        | NoSep -> "NoSep"
        | Sep s -> "Sep '" ^ s ^ "'"
        | SepFinal (s, finalSep) -> "SepFinal ('" ^ s ^ "', '" ^ finalSep ^ "')" in
      printf "%s Sequence of %d, sep: %s, stick_to_left: %s break: %s\n"
        indent (List.length layout_list) sep (string_of_bool config.sepLeft) break;
      List.iter (traverse (indent_more indent)) layout_list
    | Label (_, left, right) ->
      printf "%s Label: \n" indent;
      printf "  %s left \n" indent;
      let indent' = indent_more (indent_more indent) in
      traverse indent' left;
      printf "  %s right \n" indent;
      traverse indent' right;
    | Easy e ->
      printf "%s Easy: '%s' \n" indent (string_of_easy e)
  in
  traverse "" layout

let source_map ?(loc=Location.none) layout =
  if loc = Location.none then layout
  else SourceMap (loc, layout)
