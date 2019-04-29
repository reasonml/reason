open MenhirSdk.Cmly_api
open Utils

(* Attributes guide the recovery .

   Some information can be passed to Menhir-recover via attributes. These are
   pieces of string that are ignored by Menhir itself and are transmitted to
   Menhir-recover.

   The attributes that are relevant to Menhir-recover are always prefixed with
   `recover.`.
   An attribute with the same prefix and that is not understood by
   Menhir-recover will produce a warning message (to detect a typo or a
   misplaced attribute).
*)

(** Specification of attributes that are meaningful for recovery *)
module type ATTRIBUTES = sig
  (** The Menhir grammar to which these apply *)
  module G : GRAMMAR

  (** Recovery cost

      When the parser is in an error state, Menhir-recover will invent some input
      that recovers from this error. In most grammars, this problem has many
      solutions, often an infinity.

      But not all solutions are equally nice. Some will have repetitions, some
      will generate undesirable AST nodes or trigger error reductions...

      To guide this process, a cost can be associated to each symbol (terminal
      or non-terminal), and the cost of the recovery will be the sum of the
      cost of all symbols in the generated sentence.
  *)

  (** Symbol cost

      The `recover.cost` attribute is attached to the definition of symbols
      (terminals and non-terminals) and takes a floating point value.

      %token PLUS [@recover.cost 1.0]

      expr [@recover.cost 1.0]:
        ...
      ;
   *)

  (** Cost of a grammar symbol *)
  val cost_of_symbol  : G.symbol -> Cost.t

  (** Item cost

      The cost can be applied to a specific item (an occurrence of a symbol in
      a rule).

      In this case, the more specific cost will replace the global cost for
      this specific occurrence.

      expr:
      | INT PLUS [@recover.cost 0.0] INT { ... }
      | INT TIMES [@recover.cost 10.0] INT { ... }
      ;

      In this example, if an error happens just after an integer in an
      expression, the `PLUS` rule will be favored over the `TIMES` rule because
      the first token is more expensive.
  *)

  (** Penalty (added cost) for shifting an item *)
  val penalty_of_item : G.production * int -> Cost.t

  (** Reduction cost

      The last place where a `recover.cost` is accepted is in a production.
      This is convenient to prevent the recovery to trigger some semantic
      actions.

      expr:
        LPAREN expr error { ... } [@recover.cost infinity]
      ;

      It would not make much sense for the recovery to select an error rule.
      Associating an infinite cost to the production ensures that this never
      happen.
  *)

  (** Cost of reducing a production *)
  val cost_of_prod    : G.production -> Cost.t

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
                cost, can the algorithm diverge?
      *)

  (** Recovery expressions

      Symbols with a semantic value cannot be picked by the recovery algorithm
      if it does not know how to produce this value.

      The `recover.expr` attribute associates an ocaml expression to a symbol.
      This expression should evaluate to a semantic value for this symbol.

          %token<string> IDENT [@recover.expr "invalid-identifier"]

      When applied to non-terminals, it is particularly useful to produce a
      value that could not be the result of a normal parse.

          expr [@recover.expr Invalid_expression]:
            ...
          ;

      Here `Invalid_expression` is a node added to the AST for the purpose of
      identifying parts that were recovered.

      Furthermore, specifying fallback values for non-terminals prevents
      Menhir-recover from generating a hardly predictable sequence of tokens
      just for filling holes in the AST.
  *)

  (** An optional ocaml expression that should evaluate to a
      semantic value valid for this terminal. *)
  val default_terminal    : G.terminal -> string option

  (** An optional ocaml expression that should evaluate to a
      semantic value valid for this non-terminal. *)
  val default_nonterminal : G.nonterminal -> string option

  (** The expressions are evaluated every time a new instance of a symbol is
      needed, although it is not specified whether every evaluation will be
      kept in the final solution (at run time, the algorithm is free to explore
      different branches and throw them away as needed).

      **TODO**: decide how information can be communicated with recovery
                expressions (for instance the current location of the parser)
  *)

  (** Recovery prelude

      The `recover.prelude` attribute is attached to the grammar.

      It is an arbitrary piece of OCaml code that will be inserted before the
      code of `recover.expr` expressions.

      It is useful for defining definitions shared by the recovery expressions,
      in the same way as `%{ ... %}` is used to share definitions in semantic
      actions of the grammar.
  *)

  (** Output the grammar prelude in this formatter *)
  val default_prelude     : Format.formatter -> unit
end (* module type ATTRIBUTES *)

module Recover_attributes (G : GRAMMAR)
  : ATTRIBUTES with module G = G =
struct
  module G = G
  open G

  let string_starts_with str ~prefix =
    let len = String.length prefix in
    (String.length str >= len) &&
    (try
       for i = 0 to len - 1 do
         if str.[i] <> prefix.[i] then raise Exit;
       done;
       true
     with Exit -> false)

  let prefix = "recover."

  let all_attributes = [
    "recover.cost";
    "recover.expr";
    "recover.prelude";
  ]

  let validate_attribute accepted kind attr =
    let label = Attribute.label attr in
    if string_starts_with ~prefix label &&
       not (List.mem label accepted) then (
      let split_pos pos =
        (pos.Lexing.pos_fname,
         pos.Lexing.pos_lnum,
         pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      in
      let range () range =
        let s = Printf.sprintf in
        let sf, sl, sc = split_pos (Range.startp range) in
        let ef, el, ec = split_pos (Range.endp range) in
        if sf <> ef then
          s "%s:%d.%d-%s:%d.%d" sf sl sc ef el ec
        else if sl <> el then
          s "%s:%d.%d-%d.%d" sf sl sc el ec
        else if sc <> ec then
          s "%s:%d.%d-%d" sf sl sc ec
        else
          s "%s:%d.%d" sf sl sc
      in
      let f fmt = Printf.ksprintf prerr_endline fmt in
      if List.mem label all_attributes then
        f "%a: attribute %S cannot be put in %s"
          range (Attribute.position attr) label kind
      else
        f "%a: attribute %S is not recognized (found in %s)"
          range (Attribute.position attr) label kind
    )

  let validate_attributes accepted kind attrs =
    List.iter (validate_attribute accepted kind) attrs

  let () =
    validate_attributes
      ["recover.prelude"] "grammar attributes"
      Grammar.attributes;
    let symbol prj attrs =
      validate_attributes
        ["recover.cost"; "recover.expr"] "symbol attributes"
        (prj attrs)
    in
    Nonterminal.iter (symbol G.Nonterminal.attributes);
    Terminal.iter (symbol G.Terminal.attributes);
    Production.iter (fun p ->
      validate_attributes
        ["recover.cost"] "production attributes"
        (Production.attributes p);
      Array.iter
        (fun (_,_,attrs) ->
           validate_attributes ["recover.cost"] "item attributes" attrs)
        (Production.rhs p)
    )

  let cost_of_attributes prj attrs =
    Cost.of_int (
      List.fold_left
        (fun total attr ->
           if Attribute.has_label "recover.cost" attr then
             total + int_of_string (Attribute.payload attr)
           else total)
        0 (prj attrs)
    )

  let cost_of_symbol =
    let measure ~has_default prj attrs =
      if List.exists (Attribute.has_label "recover.expr") (prj attrs) || has_default
      then cost_of_attributes prj attrs
      else Cost.infinite
    in
    let ft = Terminal.tabulate
        (fun t -> measure ~has_default:(Terminal.typ t = None) Terminal.attributes t)
    in
    let fn =
      Nonterminal.tabulate (measure ~has_default:false Nonterminal.attributes)
    in
    function
    | T t -> begin match Terminal.kind t with
        | `ERROR -> Cost.infinite
        | _ -> ft t
      end
    | N n -> fn n

  let cost_of_prod =
    Production.tabulate (cost_of_attributes Production.attributes)

  let penalty_of_item =
    let f = Production.tabulate @@ fun p ->
      Array.map (cost_of_attributes (fun (_,_,a) -> a))
        (Production.rhs p)
    in
    fun (p,i) ->
      let costs = f p in
      if i < Array.length costs then costs.(i) else cost_of_prod p

  let default_prelude ppf =
    List.iter (fun a ->
        if Attribute.has_label "recover.prelude" a then
          Format.fprintf ppf "%s\n" (Attribute.payload a)
      ) Grammar.attributes

  let default_expr ?(fallback="raise Not_found") attrs =
    match List.find (Attribute.has_label "recover.expr") attrs with
    | exception Not_found -> fallback
    | attr -> Attribute.payload attr

  let default_terminal t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF ->
        let fallback = match Terminal.typ t with
          | None -> Some "()"
          | Some _ -> None
        in
        Some (default_expr ?fallback (Terminal.attributes t))
    | `PSEUDO -> None

  let default_nonterminal n =
    match Nonterminal.kind n with
    | `REGULAR -> Some (default_expr (Nonterminal.attributes n))
    | `START -> None
end
