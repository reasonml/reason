(* Attributes guide the recovery .

   Some information can be passed to Menhir-recover via attributes. These are
   pieces of string that are ignored by Menhir itself and are transmitted to
   Menhir-recover.

   The attributes that are relevant to Menhir-recover are always prefixed with
   `recover.`. An attribute with the same prefix and that is not understood by
   Menhir-recover will produce a warning message (to detect a typo or a
   misplaced attribute). *)

(** Specification of attributes that are meaningful for recovery *)
module type ATTRIBUTES = sig
  module G : MenhirSdk.Cmly_api.GRAMMAR

  (** The Menhir grammar to which these apply *)

  (** Recovery cost

      When the parser is in an error state, Menhir-recover will invent some
      input that recovers from this error. In most grammars, this problem has
      many solutions, often an infinity.

      But not all solutions are equally nice. Some will have repetitions, some
      will generate undesirable AST nodes or trigger error reductions...

      To guide this process, a cost can be associated to each symbol (terminal
      or non-terminal), and the cost of the recovery will be the sum of the cost
      of all symbols in the generated sentence. *)

  (** Symbol cost

      The `recover.cost` attribute is attached to the definition of symbols
      (terminals and non-terminals) and takes a floating point value.

      %token PLUS [@recover.cost 1.0]

      expr [@recover.cost 1.0]: ... ; *)

  val cost_of_symbol : G.symbol -> Cost.t
  (** Cost of a grammar symbol *)

  (** Item cost

      The cost can be applied to a specific item (an occurrence of a symbol in a
      rule).

      In this case, the more specific cost will replace the global cost for this
      specific occurrence.

      expr: | INT PLUS [@recover.cost 0.0] INT \{ ... \} | INT TIMES
      [@recover.cost 10.0] INT \{ ... \} ;

      In this example, if an error happens just after an integer in an
      expression, the `PLUS` rule will be favored over the `TIMES` rule because
      the first token is more expensive. *)

  val penalty_of_item : G.production * int -> Cost.t
  (** Penalty (added cost) for shifting an item *)

  (** Reduction cost

      The last place where a `recover.cost` is accepted is in a production. This
      is convenient to prevent the recovery to trigger some semantic actions.

      expr: LPAREN expr error \{ ... \} [@recover.cost infinity] ;

      It would not make much sense for the recovery to select an error rule.
      Associating an infinite cost to the production ensures that this never
      happen. *)

  val cost_of_prod : G.production -> Cost.t
  (** Cost of reducing a production *)

  (** Meaning of costs

      The cost should be a positive floating-point value. +∞ and 0.0 are
      accepted.

      If not specified, the default cost depends on the presence of a semantic
      value:
      - for a terminal without semantic value (such as `%token DOT`) it is 0.0.
      - for a terminal with a semantic value (such as `%token<int> INT`) or a
        non-terminal it is +∞.

      If the attribute happens multiple times, the sum of all occurrences is
      used.

      **TODO**: specify how null values are treated with respect to minimal
      cost, can the algorithm diverge? *)

  (** Recovery expressions

      Symbols with a semantic value cannot be picked by the recovery algorithm
      if it does not know how to produce this value.

      The `recover.expr` attribute associates an ocaml expression to a symbol.
      This expression should evaluate to a semantic value for this symbol.

      %token<string> IDENT [@recover.expr "invalid-identifier"]

      When applied to non-terminals, it is particularly useful to produce a
      value that could not be the result of a normal parse.

      expr [@recover.expr Invalid_expression]: ... ;

      Here `Invalid_expression` is a node added to the AST for the purpose of
      identifying parts that were recovered.

      Furthermore, specifying fallback values for non-terminals prevents
      Menhir-recover from generating a hardly predictable sequence of tokens
      just for filling holes in the AST. *)

  val default_terminal : G.terminal -> string option
  (** An optional ocaml expression that should evaluate to a semantic value
      valid for this terminal. *)

  val default_nonterminal : G.nonterminal -> string option
  (** An optional ocaml expression that should evaluate to a semantic value
      valid for this non-terminal. *)

  (** The expressions are evaluated every time a new instance of a symbol is
      needed, although it is not specified whether every evaluation will be kept
      in the final solution (at run time, the algorithm is free to explore
      different branches and throw them away as needed).

      **TODO**: decide how information can be communicated with recovery
      expressions (for instance the current location of the parser) *)

  (** Recovery prelude

      The `recover.prelude` attribute is attached to the grammar.

      It is an arbitrary piece of OCaml code that will be inserted before the
      code of `recover.expr` expressions.

      It is useful for defining definitions shared by the recovery expressions,
      in the same way as `%\{ ... %\}` is used to share definitions in semantic
      actions of the grammar. *)

  val default_prelude : Format.formatter -> unit
  (** Output the grammar prelude in this formatter *)
end

module Recover_attributes (G : MenhirSdk.Cmly_api.GRAMMAR) :
  ATTRIBUTES with module G = G
