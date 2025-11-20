module Easy_format = Reason_easy_format

type break_criterion =
  | Never
  | IfNeed
  | Always
  (* Always_rec not only will break, it will break recursively up to the root *)
  | Always_rec

(* Modeling separators: Special ability to render the final separator
   distinctly. This is so we can replace them when they do/don't occur next to
   newlines.

   If sepLeft:true { final item1 sep item2 sep item3 }

   If sepLeft:false { item1 sep item2 sep item3 final } *)
(* You can't determine the final separator unless you specify a separator *)
type separator =
  | NoSep
  | Sep of string
  | SepFinal of string * string

(** * Module concerning info to correctly interleave whitespace above a layout
    node. *)
module WhitespaceRegion : sig
  type t =
    { (* range of the region *)
      range : Reason_location.Range.t
    ; (* inserted comments into the whitespace region *)
      comments : Reason_comment.t list
    ; (* amount of newlines to be interleaved *)
      newlines : int
    }

  val make : range:Reason_location.Range.t -> newlines:int -> t
  val newlines : t -> int
  val range : t -> Reason_location.Range.t
  val comments : t -> Reason_comment.t list
  val addComment : t -> Reason_comment.t -> t
  val modifyNewlines : t -> int -> t
end

(** * These represent "intent to format" the AST, with some parts being
    annotated * with original source location. The benefit of tracking this in
    an * intermediate structure, is that we can then interleave comments
    throughout * the tree before generating the final representation. That
    prevents the * formatting code from having to thread comments everywhere. *
    * The final representation is rendered using Easy_format. *)
type t =
  | SourceMap of Location.t * t (* a layout with location info *)
  | Sequence of config * t list
  | Label of (Easy_format.t -> Easy_format.t -> Easy_format.t) * t * t
  | Easy of Easy_format.t
  (* Extra variant representing "intent to interleave whitespace" above a
   * layout node. Why the extra representation?
   * Since comments get interleaved after formatting the ast,
   * the inserting of actual newlines has to happen after the comments
   * have been formatted/inserted. *)
  | Whitespace of WhitespaceRegion.t * t

and config =
  { break : break_criterion
  ; (* Break setting that becomes activated if a comment becomes interleaved into
     * this list. Typically, if not specified, the behavior from [break] will be
     * used.
     *)
    wrap : string * string
  ; inline : bool * bool
  ; sep : separator
  ; indent : int
  ; sepLeft : bool
  ; preSpace : bool
  ; (* Really means space_after_separator *)
    postSpace : bool
  ; pad : bool * bool
  ; (* A function, because the system might rearrange your previous settings, and
     * a function allows you to not be locked into some configuration that is made
     * out of date by the formatting system (suppose it removes the separator
     * token etc.) Having a function allows you to instruct our formatter how to
     * extend the "freshest" notion of the list config when comments are
     * interleaved. *)
    listConfigIfCommentsInterleaved : (config -> config) option
  ; (* Formatting to use if an item in a list had an end-of-line comment
       appended *)
    listConfigIfEolCommentsInterleaved : (config -> config) option
  }

val to_easy_format : t -> Easy_format.t
val get_location : t -> Warnings.loc option
val source_map : ?loc:Warnings.loc -> t -> t
val contains_location : t -> location:Warnings.loc -> bool
val is_before : location:Warnings.loc -> t -> bool
