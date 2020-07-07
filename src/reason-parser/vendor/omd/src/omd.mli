(** A markdown parser in OCaml. *)

type attributes =
  (string * string) list

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type inline =
  {
    il_desc: inline_desc;
    il_attributes: attributes;
  }

and inline_desc =
  | Concat of inline list
  | Text of string
  | Emph of inline
  | Strong of inline
  | Code of string
  | Hard_break
  | Soft_break
  | Link of inline link_def
  | Image of inline link_def
  | Html of string

type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

type def_elt =
  {
    term: inline;
    defs: inline list;
  }

and block =
  {
    bl_desc: block_desc;
    bl_attributes: attributes;
  }

and block_desc =
  | Paragraph of inline
  | List of list_type * list_spacing * block list list
  | Blockquote of block list
  | Thematic_break
  | Heading of int * inline
  | Code_block of string * string
  | Html_block of string
  | Link_def of string link_def
  | Definition_list of def_elt list

type doc = block list
(** A markdown document *)

val of_channel: in_channel -> doc

val of_string: string -> doc

val to_html: doc -> string

val to_sexp: doc -> string

val to_ocamldoc: doc -> string
