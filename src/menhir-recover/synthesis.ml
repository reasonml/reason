open MenhirSdk.Cmly_api
open Attributes
open Utils

(** Specification of synthesized tactics *)

module type SYNTHESIZER = sig
  module G : GRAMMAR

  (* Specification of problems

     There are two situations we want to synthesize solution for:

     - `Head` is when the dot is just in front of some non-terminal,
       and we would like to find a way to move the dot to the right of
       this symbol (by executing a sequence of actions that results in
       this non-terminal being pushed on the stack)

     - `Tail` is when the dot is in some production that we would like
       to reduce.
  *)

  type variable =
    | Head of G.lr1 * G.nonterminal
    | Tail of G.lr1 * G.production * int

  (* The integer parameter in `Tail` is the position of the dot in
     the production we are trying to reduce. This is necessary to
     uniquely identify a production that occurs multiple time in a
     state.

     For instance, in the grammar:

     %token<int> INT
     %token PLUS

     expr:
     | INT { $1 } (*const*)
     | expr PLUS expr { $1 + $2 } (*add*)

     Synthesizing `Head (st0, expr)` when `expr PLUS . expr` is in
     `st0` will output the actions to get to the state `st'`
     containing `expr PLUS expr .`.

     Synthesizing `Tail (st1, add, 1)` when `expr . PLUS expr` is in
     `st1` will output the actions that end up reducing `add` (which
     will likely be shifting `PLUS`, synthesizing `Head (st0, expr)`
     and reducing add).
  *)

  val variable_to_string : variable -> string
  (** A human readable representation of a [variable]. *)

  (** Specification of solutions

      A successful synthesis results in a list of actions. *)

  type action =
    | Abort
    | Reduce of G.production
    | Shift  of G.symbol
    | Seq    of action list

  (* `Abort` is issued if there is no solution. This is the case for instance
     if there is a semantic value that the synthesizer cannot produce, or a
     production with an infinite cost.

     `Shift` and `Reduce` are direct actions to execute on the parser.

     `Seq` is a sequence of action.
  *)

  val action_to_string : action -> string
  (** A human readable representation of an action. *)

  val solve : variable -> Cost.t * action list
  (** Give the solution found for a variable as a list of action. *)

  val report : Format.formatter -> unit
  (** Print the solutions or absence thereof for the whole grammar. *)
end

(** Synthesizer implementation *)

module Synthesizer (G : GRAMMAR) (A : ATTRIBUTES with module G = G)
  : SYNTHESIZER with module G := G =
struct
  open G

  type variable =
    | Head of lr1 * nonterminal
    | Tail of lr1 * production * int

  let variable_to_string = function
    | Head (st, n) ->
      Printf.sprintf "Head (#%d, %s)"
        (Lr1.to_int st) (Nonterminal.name n)
    | Tail (st, prod, pos) ->
      Printf.sprintf "Tail (#%d, p%d, %d)"
        (Lr1.to_int st) (Production.to_int prod) pos

  type action =
    | Abort
    | Reduce of production
    | Shift  of symbol
    | Seq    of action list

  let rec action_to_string = function
    | Abort -> "Abort"
    | Reduce prod -> "Reduce p" ^ string_of_int (Production.to_int prod)
    | Shift  sym -> "Shift " ^ (symbol_name sym)
    | Seq actions ->
      "Seq [" ^ String.concat "; " (List.map action_to_string actions) ^ "]"


  (** The synthesizer specify the cost as a system of equations of the form
      $$
      x_i = \min_{j} ({\kappa_{i,j} + \sum_{k}x_{i,j,k}})
      $$
      which can be read as follow:

      - $x_i$ are variables, the thing we would like to know the cost of (the
        `Head` and `Tail` defined above)

      - $j$ ranges over the different branches, the different candidates (for
        instance, to synthesize a _non-terminal_, each production that reduces
        to this _non-terminal_ is a valid candidate)

      - each of these candidates is made of a constant and the sum of a
        possibly empty list of other variables

      Variables are valued in $\left[0,+\infin\right]$ (and the empty $\sum$
      defaults to $0$, the empty $min$ to $+\infin$).

      The solution is the least fixed point of this system computed by
      [Fix](https://gitlab.inria.fr/fpottier/fix) library.

      $$
      \begin{align}
        \text{head}_{st,nt} = & \min \left\{ \begin{array}{ll}
        \text{cost}(\text{empty-reductions}(st,nt))\\
        \text{tail-reductions}(st,nt)
      \end{array}
      \right.
      \\
      \text{empty-reductions}(st,nt) = &
      \\
      \text{tail}_{st,prod,i} = &
      \end{align}
      $$

      For a variable `Head (st, nt)` , the branches are the different
      productions that can reduce to `nt` and starts from state `st`. The
      constant is the same for all branches, $\kappa_{i,j} = \kappa_i$,
  *)

  let cost_of_prod p = Cost.add (Cost.of_int 1) (A.cost_of_prod p)
  let cost_of_symbol s = Cost.add (Cost.of_int 1) (A.cost_of_symbol s)
  let penalty_of_item i = A.penalty_of_item i

  let app var v = v var

  let bottom = (Cost.infinite, [Abort])

  let var var = match var with
    | Head _ -> app var
    | Tail (_,prod,pos) ->
      let prod_len = Array.length (Production.rhs prod) in
      assert (pos <= prod_len);
      if pos < prod_len
      then app var
      else const (cost_of_prod prod, [Reduce prod])

  let productions =
    let table = Array.make Nonterminal.count [] in
    Production.iter (fun p ->
        let nt = Nonterminal.to_int (Production.lhs p) in
        table.(nt) <- p :: table.(nt)
      );
    (fun nt -> table.(Nonterminal.to_int nt))

  let cost_of = function
    | Head (st, nt) ->
      begin fun v ->
        let minimize_over_prod (cost,_ as solution) prod =
          let (cost', _) as solution' = v (Tail (st, prod, 0)) in
          if cost <= cost' then solution else solution'
        in
        List.fold_left minimize_over_prod bottom (productions nt)
      end

    | Tail (st, prod, pos) ->
      let prod_len = Array.length (Production.rhs prod) in
      assert (pos <= prod_len);
      let penalty = penalty_of_item (prod, pos) in
      if Cost.is_infinite penalty then
        const bottom
      else if pos = prod_len then
        let can_reduce = List.exists
            (fun (_,prods) -> List.mem prod prods) (Lr1.reductions st)
        in
        const (if can_reduce
               then (cost_of_prod prod, [Reduce prod])
               else (Cost.infinite, [Abort]))
      else
        let head =
          let sym, _, _ = (Production.rhs prod).(pos) in
          let cost = cost_of_symbol sym in
          if Cost.is_infinite cost
          then match sym with
            | T _ -> const bottom
            | N n -> var (Head (st, n))
          else const (cost, [Shift sym])
        in
        let tail =
          let sym, _, _ = (Production.rhs prod).(pos) in
          match List.assoc sym (Lr1.transitions st) with
          | st' -> var (Tail (st', prod, pos + 1))
          | exception Not_found ->
            (*report "no transition: #%d (%d,%d)\n"
              st.lr1_index prod.p_index pos;*)
            const bottom
        in
        (fun v ->
           let costh, actionh = head v in
           let costt, actiont = tail v in
           (Cost.add costh costt, Seq actionh :: actiont)
        )

  let solve =
    let module Solver = Fix.Fix.ForType (struct
        type t = variable
      end) (struct
        type property = Cost.t * action list
        let bottom = (Cost.infinite, [Abort])
        let equal (x, _ : property) (y, _ : property) : bool =
          Cost.compare x y = 0
        let is_maximal _ = false
      end)
    in
    Solver.lfp cost_of

  let report ppf =
    let open Format in
    let solutions = Lr1.fold
        (fun st acc ->
           match
             List.fold_left (fun (item, (cost, _ as solution)) (prod, pos) ->
                 let cost', _ as solution' = solve (Tail (st, prod, pos)) in
                 if cost' < cost then
                   (Some (prod, pos), solution')
                 else
                   (item, solution)
               ) (None, bottom) (Lr0.items (Lr1.lr0 st))
           with
           | None, _ ->
             fprintf ppf "no synthesis from %d\n" (Lr1.to_int st);
             acc
           | Some item, cost -> (item, (cost, st)) :: acc
        ) []
    in
    let fprintf = Format.fprintf in
    let rec print_action ppf = function
      | Abort -> fprintf ppf "Abort"
      | Reduce prod  -> fprintf ppf "Reduce %d" (Production.to_int prod)
      | Shift  (T t) -> fprintf ppf "Shift (T %s)" (Terminal.name t)
      | Shift  (N n) -> fprintf ppf "Shift (N %s)" (Nonterminal.mangled_name n)
      | Seq    actions -> fprintf ppf "Seq %a" print_actions actions
    and print_actions ppf = Utils.pp_list print_action ppf
    in
    List.iter (fun (item, states) ->
        fprintf ppf "# Item (%d,%d)\n"
          (Production.to_int (fst item)) (snd item);
        Print.item ppf item;
        List.iter (fun ((cost, actions), states) ->
            fprintf ppf "at cost %d from states %a:\n%a\n\n"
              (cost : Cost.t :> int)
              (Utils.pp_list (fun ppf st ->
                   fprintf ppf "#%d" (Lr1.to_int st))) states
              print_actions actions
          ) (group_assoc states)
      ) (group_assoc solutions)
end
