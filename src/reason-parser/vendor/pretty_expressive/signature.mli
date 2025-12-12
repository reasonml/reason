(** This module defines types for the pretty printer. *)

type renderer = string -> unit
(** The type for a function to print from a pretty printer. *)

module type CostFactory =
sig
  (** The cost factory interface.

      A valid cost factory should also satisfy the following contracts.

      {ul {- [le] is a total ordering.}
          {- If [le a b] and [le c d] then [le (combine a c) (combine b d)]}
          {- If [a] <= [b], then [le (text a l) (text b l)]}
          {- If [a] <= [b], then [le (newline a) (newline b)]}
          {- [text c (a + b) = combine (text c a) (text (c + a) b)]}
          {- [combine] is associative and has the identity equal to [text 0 0]}
          {- [text c 0] = [text 0 0] for any [c]}
          {- If [a] <= [b], then [le (two_columns_overflow a) (two_columns_overflow b)]}
          {- If [a] <= [b], then [le (two_columns_bias a) (two_columns_bias b)]}}

      See {{!Printer.default_cost_factory}[default_cost_factory]},
      {{!page-index.factory}the cost factory section},
      and {{: https://dl.acm.org/doi/abs/10.1145/3622837 }the paper}
      for examples of cost factories. *)

  type t
  (** A type for cost *)

  val text : int -> int -> t
  (** [text c l] calculates a cost for a text placement at column position [c]
      with length [l] *)

  val newline : int -> t
  (** [newline i] calculates a cost for a newline and indentation at level [i] *)

  val combine : t -> t -> t
  (** [combine x y] combines the costs [x] and [y] together *)

  val le : t -> t -> bool
  (** [le x y] tests if the cost [x] is less than or equal to the cost [y]. *)

  val two_columns_bias : int -> t
  (** [two_columns_bias i] is the bias cost, added to each possible
      column separator so that the leftmost column separator is preferred. *)

  val two_columns_overflow : int -> t
  (** [two_columns_overflow i] is the cost due to exceeding the column separator.
      Make this cost greater than the usual overflow cost
      (for exceeding the page width limit) to prefer going over the
      page width limit instead of going over the column separator. *)

  val limit: int
  (** [limit] is {{!page-index.complimit}the computation width limit}. *)

  val string_of_cost : t -> string
  (** [string_of_cost c] produces a string representation of a cost [c] *)

  val debug_format : string -> bool -> string -> string
  (** [debug_format s is_tainted cost] produces a debugging string
      from the output of the core printer. *)
end

module type PrinterT =
sig

  type doc
  (** The [doc] type *)

  type cost
  (** The [cost] type *)

  (** {2 Pretty printing functions} *)

  val pretty_print_info : ?init_c:int -> renderer -> doc -> cost Util.info
  (** [pretty_print_info renderer d] prints the document [d]
      by repeatedly calling [renderer] and outputs the debugging information
      as an [info] record.The optional [~init_c] can be used to indicate
      that the printing begins at a non-zero column position. *)

  val pretty_format_info : ?init_c:int -> doc -> string * cost Util.info
  (** [pretty_format_info] is similar to {{!Printer.Make.pretty_print_info}[pretty_print_info]},
      but it prints to a string instead of a renderer. *)

  val pretty_print : ?init_c:int -> renderer -> doc -> unit
  (** [pretty_print d] is similar to {{!Printer.Make.pretty_print_info}[pretty_print_info]}
      without debugging information. *)

  val pretty_format : ?init_c:int -> doc -> string
  (** [pretty_format] is similar to {{!Printer.Make.pretty_format}[pretty_format]},
      without debugging information. *)

  val pretty_format_debug : ?init_c:int -> doc -> string
  (** [pretty_format_debug] is similar to
      {{!Printer.Make.pretty_format_info}[pretty_format_info]},
      but the debugging information is included as a part of the output string. *)

  (** {2 Text document} *)

  val text : string -> doc
  (** [text s] is a document for textual content [s];
      [s] must not contain a newline. *)

  (** {2 Newline documents} *)

  val newline : (string option) -> doc
  (** [newline s] is a document for a newline. *)

  val nl : doc
  (** [nl] is a document for a newline that {!flatten}s to a single space. *)

  val break : doc
  (** [break] is a document for a newline that {!flatten}s to an empty string. *)

  val hard_nl : doc
  (** [hard_nl] is a document for a newline that {!fail}s to {!flatten}. *)

  (** {2 Concatenation document} *)

  val (^^) : doc -> doc -> doc
  (** [a ^^ b] is a document for concatenation of documents [a] and [b]
      {i without} alignment. *)

  (** {2 Choice document} *)

  val (<|>) : doc -> doc -> doc
  (** [a <|> b] is a document for a choice between document [a] and [b]. *)

  (** {2 Indentation documents} *)

  val align : doc -> doc
  (** [align d] is a document that aligns [d] at the column position. *)

  val nest : int -> doc -> doc
  (** [nest n d] is a document that increments the indentation level by [n]
      when rendering [d]. *)

  val reset : doc -> doc
  (** [reset d] is a document that resets indentation level to 0 in [d]. *)

  (** {2 Cost document} *)

  val cost : cost -> doc -> doc
  (** [cost c d] is a document that artificially adds cost [c] to [d]. *)

  (** {2 Filler documents} *)

  val two_columns : (doc * doc) list -> doc
  (** ({b experimental}) [two_columns ds] is a document that lays out
      the documents in [ds] in two columns. *)

  (** {2 Failure document} *)

  val fail : doc
  (** A document that always fails. *)

  (** {2 Other derived combinators} *)

  val flatten : doc -> doc
  (** [flatten d] is a document that replaces newlines and indentation spaces
      with what's specified in [newline] when rendering [d]. *)

  val group : doc -> doc
  (** [group d] is a shorthand for [d <|> flatten d]. *)

  val (<+>) : doc -> doc -> doc
  (** [a <+> b] is a shorthand for [a ^^ align b]. *)

  val (<$>) : doc -> doc -> doc
  (** [a <$> b] is a shorthand for [a ^^ hard_nl ^^ b]. *)

  val (<->) : doc -> doc -> doc
  (** [a <-> b] is a shorthand for [flatten a <+> b]. *)

  val fold_doc : (doc -> doc -> doc) -> doc list -> doc
  (** [fold_doc (++) ds] is a shorthand for [d_1 ++ d_2 ++ ... ++ d_n]. *)

  val vcat : doc list -> doc
  (** [vcat ds] is a shorthand for [d_1 <$> d_2 <$> ... <$> d_n]. *)

  val hcat : doc list -> doc
  (** [hcat ds] is a shorthand for [d_1 <-> d_2 <-> ... <-> d_n]. *)

  val empty : doc
  (** Equivalent to [text ""] *)

  val space : doc
  (** Equivalent to [text " "] *)

  val comma : doc
  (** Equivalent to [text ","] *)

  val lbrack : doc
  (** Equivalent to [text "\["] *)

  val rbrack: doc
  (** Equivalent to [text "\]"] *)

  val lbrace : doc
  (** Equivalent to [text "{"] *)

  val rbrace : doc
  (** Equivalent to [text "}"] *)

  val lparen : doc
  (** Equivalent to [text "("] *)

  val rparen : doc
  (** Equivalent to [text ")"] *)

  val dquote : doc
  (** Equivalent to [text "\""] *)
end

module type PrinterCompatT =
sig
  include PrinterT
  (** @closed *)

  val (<>) : doc -> doc -> doc
  (** [<>] is the same as {!(^^)} *)
end

