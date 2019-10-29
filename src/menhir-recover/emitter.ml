open MenhirSdk.Cmly_api
open Utils
open Attributes
open Synthesis
open Recovery_intf

let menhir = "MenhirInterpreter"

(* Generation scheme doing checks and failing at runtime, or not ... *)
let safe = false

type var = int

module Codesharing
    (G : GRAMMAR)
    (S : SYNTHESIZER with module G := G)
    (R : RECOVERY with module G := G) :
sig

  type instr =
    | IRef of var
    | IAbort
    | IReduce of G.production
    | IShift  of G.symbol

  val compile : R.item list -> instr list list * (R.item -> instr list)

end = struct

  open S

  (* Rewrite trivial indirections:
       Seq [x] => x
       ys @ [Seq xs] => ys @ xs
   *)

  let rec normalize_actions = function
    | [] -> []
    | [Seq v] -> normalize_actions v
    | (x :: xs) -> normalize_action x :: normalize_actions xs

  and normalize_action = function
    | Abort | Reduce _ | Shift _ as a -> a
    | Seq [v] -> normalize_action v
    | Seq v ->
        match normalize_actions v with
        | [x] -> x
        | xs -> Seq xs

  (* Find sharing opportunities.
     If the same sequence of action occurs multiple times, the funtion
     will associate a unique identifier to the sequence.

     [share actions] returns a pair
     [(bindings, lookup) : action list array * (action list -> int option)]

     The [bindings] array contains all action lists that are worth sharing.
     The [lookup] function returns the index of an action list if is
     is in the array.
   *)

  let share actions =
    let occurrence_table = Hashtbl.create 113 in
    begin
      let order = ref 0 in
      let rec iter_list = function
        | [] | [_] -> ()
        | (x :: xs) as xxs ->
          match Hashtbl.find occurrence_table xxs with
          | occurrences, _index -> incr occurrences
          | exception Not_found ->
            let index = ref (-1) in
            Hashtbl.add occurrence_table xxs (ref 1, index);
            iter x; iter_list xs;
            index := !order;
            incr order
      and iter = function
        | Abort | Reduce _ | Shift _ -> ()
        | Seq xs -> iter_list xs
      in
      List.iter iter_list actions;
    end;
    let bindings =
      let register actions (occurrences, index) to_share =
        if !occurrences > 1
        then (!index, actions) :: to_share
        else to_share
      in
      let to_share = Hashtbl.fold register occurrence_table [] in
      let order_actions (o1, _) (o2, _) = compare o1 (o2 : int) in
      List.map snd (List.sort order_actions to_share)
    in
    let binding_table = Hashtbl.create 113 in
    List.iteri
      (fun idx actions -> Hashtbl.add binding_table actions idx)
      bindings;
    let lookup actions =
      match Hashtbl.find binding_table actions with
      | exception Not_found -> None
      | index -> Some index
    in
    (bindings, lookup)

  let item_to_actions (st, prod, pos) =
    normalize_actions (snd (S.solve (Tail (st, prod, pos))))

  type instr =
    | IRef of int
    | IAbort
    | IReduce of G.production
    | IShift  of G.symbol

  let rec compile_one ~sharing = function
    | Abort    -> [IAbort]
    | Reduce p -> [IReduce p]
    | Shift s  -> [IShift s]
    | Seq xs   -> share_seq ~sharing xs

  and share_seq ~sharing seq =
    match sharing seq with
    | None -> compile_seq ~sharing seq
    | Some index -> [IRef index]

  and compile_seq ~sharing = function
    | [] -> []
    | x :: xs ->
      let x' = compile_one ~sharing x in
      let xs' = share_seq ~sharing xs in
      x' @ xs'

  let compile items =
    let actions = List.map item_to_actions items in
    let bindings, sharing = share actions in
    let bindings = List.map (compile_seq ~sharing) bindings in
    let compile_item item = share_seq ~sharing (item_to_actions item) in
    (bindings, compile_item)
end

module Make
    (G : GRAMMAR)
    (A : ATTRIBUTES with module G := G)
    (S : SYNTHESIZER with module G := G)
    (R : RECOVERY with module G := G) :
sig
  val emit : Format.formatter -> unit
end = struct

  open G
  open Format

  let emit_default_value ppf =
    fprintf ppf "open %s\n\n"
      (String.capitalize (Filename.basename Grammar.basename))
      [@ocaml.warning "-3"];
    fprintf ppf "module Default = struct\n";
    A.default_prelude ppf;

    fprintf ppf "  let value (type a) : a %s.symbol -> a = function\n" menhir;
    Terminal.iter (fun t ->
        match A.default_terminal t with
        | None -> ()
        | Some str ->
          fprintf ppf "    | %s.T %s.T_%s -> %s\n"
            menhir menhir (Terminal.name t) str
      );
    Nonterminal.iter (fun n ->
        match A.default_nonterminal n with
        | None -> ()
        | Some str ->
          fprintf ppf "    | %s.N %s.N_%s -> %s\n"
            menhir menhir (Nonterminal.mangled_name n) str
      );
    (*fprintf ppf "    | _ -> raise Not_found\n"; should be exhaustive*)
    fprintf ppf "end\n\n";
    fprintf ppf "let default_value = Default.value\n\n"

  let emit_defs ppf =
    fprintf ppf "open %s\n\n" menhir;
    fprintf ppf "type action =\n\
                \  | Abort\n\
                \  | R of int\n\
                \  | S : 'a symbol -> action\n\
                \  | Sub of action list\n\n";
    fprintf ppf "type decision =\n\
                \  | Nothing\n\
                \  | One of action list\n\
                \  | Select of (int -> action list)\n\n"

  let emit_depth ppf =
    let open Format in
    fprintf ppf "let depth =\n  [|";
    Lr1.iter (fun st ->
        let items = G.Lr0.items (G.Lr1.lr0 st) in
        let positions = List.map snd items in
        let depth = List.fold_left max 0 positions in
        fprintf ppf "%d;" depth
      );
    fprintf ppf "|]\n\n"

  let emit_can_pop ppf =
    Format.fprintf ppf "let can_pop (type a) : a terminal -> bool = function\n";
    G.Terminal.iter (fun t ->
        if G.Terminal.kind t = `REGULAR && G.Terminal.typ t = None then
          Format.fprintf ppf "  | T_%s -> true\n" (G.Terminal.name t));
    Format.fprintf ppf "  | _ -> false\n\n"

  module C = Codesharing(G)(S)(R)

  let emit_recoveries ppf =
    let all_cases =
      Lr1.fold (fun st acc ->
          try let {R. cases; _} = R.recover st in
            let cases = List.map (fun (st', items) ->
                (list_last items),
                (match st' with None -> -1 | Some st' -> Lr1.to_int st')
              ) cases
            in
            let cases = match group_assoc cases with
              | [] -> `Nothing
              | [(instr, _)] -> `One instr
              | xs -> `Select xs
            in
            (cases, (Lr1.to_int st)) :: acc
          with _ -> acc
        )
[]
in
    let all_cases = group_assoc all_cases in
    let all_items =
      let items_in_case (case, _states) =
        match case with
        | `Nothing -> []
        | `One item -> [item]
        | `Select items -> List.map fst items
      in
      List.flatten (List.map items_in_case all_cases)
    in
    let globals, get_instr = C.compile all_items in
    let open Format in
    fprintf ppf "let recover =\n";
    let emit_instr ppf = function
      | C.IAbort -> fprintf ppf "Abort"
      | C.IReduce prod -> fprintf ppf "R %d" (Production.to_int prod)
      | C.IShift (T t) -> fprintf ppf "S (T T_%s)" (Terminal.name t)
      | C.IShift (N n) -> fprintf ppf "S (N N_%s)" (Nonterminal.mangled_name n)
      | C.IRef r -> fprintf ppf "r%d" r
    in
    let emit_instrs ppf = Utils.pp_list emit_instr ppf in
    let emit_shared index instrs =
      fprintf ppf "  let r%d = Sub %a in\n" index emit_instrs instrs
    in
    List.iteri emit_shared globals;
    let emit_item ppf item = emit_instrs ppf (get_instr item) in
    fprintf ppf "  function\n";
    List.iter (fun (cases, states) ->
        fprintf ppf "  ";
        List.iter (fprintf ppf "| %d ") states;
        fprintf ppf "-> ";
        match cases with
        | `Nothing -> fprintf ppf "Nothing\n";
        | `One item -> fprintf ppf "One %a\n" emit_item item
        | `Select xs ->
          fprintf ppf "Select (function\n";
          if safe then (
            List.iter (fun (item, cases) ->
                fprintf ppf "    ";
                List.iter (fprintf ppf "| %d ") cases;
                fprintf ppf "-> %a\n" emit_item item;
              ) xs;
            fprintf ppf "    | _ -> raise Not_found)\n"
          ) else (
            match List.sort
                    (fun (_,a) (_,b) -> compare (List.length b) (List.length a))
                    xs
            with
            | (item, _) :: xs ->
              List.iter (fun (item, cases) ->
                  fprintf ppf "    ";
                  List.iter (fprintf ppf "| %d ") cases;
                  fprintf ppf "-> %a\n" emit_item item;
                ) xs;
              fprintf ppf "    | _ -> %a)\n" emit_item item
            | [] -> assert false
          )
      ) all_cases;

    fprintf ppf "  | _ -> raise Not_found\n"

  let emit_token_of_terminal ppf =
    let case t =
      match Terminal.kind t with
      | `REGULAR | `EOF ->
        fprintf ppf "  | %s.T_%s -> %s%s\n"
          menhir (Terminal.name t)
          (Terminal.name t) (if Terminal.typ t <> None then " v" else "")
      | `ERROR ->
        fprintf ppf "  | %s.T_%s -> assert false\n"
          menhir (Terminal.name t)
      | `PSEUDO -> ()
    in
    fprintf ppf
      "let token_of_terminal (type a) (t : a %s.terminal) (v : a) : token =\n\
      \  match t with\n"
      menhir;
    Terminal.iter case

  let emit_nullable ppf =
    let print_n n =
      if Nonterminal.nullable n then
        fprintf ppf "  | N_%s -> true\n" (Nonterminal.mangled_name n)
    in
    fprintf ppf "let nullable (type a) : a MenhirInterpreter.nonterminal -> bool =\n\
           \  let open MenhirInterpreter in function\n";
    Nonterminal.iter print_n;
    fprintf ppf "  | _ -> false\n"

  let emit ppf =
    emit_default_value ppf;
    emit_defs ppf;
    emit_depth ppf;
    emit_can_pop ppf;
    emit_recoveries ppf;
    emit_token_of_terminal ppf;
    emit_nullable ppf

end
