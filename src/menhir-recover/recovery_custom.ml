open MenhirSdk.Cmly_api
open Synthesis

module type RECOVERY = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int

  type recovery = {
    prefix: int;
    cases: (G.lr1 option * item list) list;
  }
  (** [prefix] is the size of the known prefix of the stack.
      It means that in the kernel of current state, there is an item whose dot
      is at position [prefix].
      (we know the incoming symbols for these stack frames and we can enumerate
      the possible state numbers).

      [cases] is a mapping that associates to each possible state found at
      stack.[-prefix]
      (or None if the stack is empty) a list of reductions to execute.

      The actual list of actions to reduce an item [(state, prod, pos)] is
      given by
          [Synthesizer.solution (Trail (state, prod, pos))]
    *)

  val recover : G.lr1 -> recovery
  val report : Format.formatter -> unit
end

module Recover (G : GRAMMAR) (S : SYNTHESIZER with module G := G)
  : RECOVERY with module G := G =
struct
  open G
  open Utils

  type item = lr1 * production * int

  type recovery = {
    prefix: int;
    cases: (G.lr1 option * item list) list;
  }

  type trace = Trace of { cost : Cost.t; items : item list }

  module Trace = struct
    type t = trace
    let min tr1 tr2 =
      Cost.arg_min (fun (Trace t) -> t.cost) tr1 tr2

    let cat (Trace tr1) (Trace tr2) =
      Trace { cost = Cost.add tr1.cost tr2.cost; items = tr1.items @ tr2.items }
  end

  module State = struct
    type level = (nonterminal * Trace.t) list
    type t = level list

    let rec merge_level l1 l2 : level = match l1, l2 with
      | [], l -> l
      | l, [] -> l
      | ((nt1, c1) :: xs1), (x2 :: xs2) ->
          let (nt2, c2) = x2 in
          match compare nt1 nt2 with
          | 0 ->
              let x = (nt1, Trace.min c1 c2) in
              x :: merge_level xs1 xs2
          | n when n > 0 -> x2 :: merge_level l1 xs2
          | _ -> (nt1, c1) :: merge_level xs1 l2

    let rec merge l1 l2 : t = match l1, l2 with
      | [], l -> l
      | l, [] -> l
      | (x1 :: l1), (x2 :: l2) ->
          let x' = merge_level x1 x2 in
          x' :: merge l1 l2
  end

  let synthesize =
    let rec add_nt tr nt = function
      | [] -> [(nt, tr)]
      | x :: xs ->
        match compare nt (fst x) with
        | 0            -> (nt, Trace.min tr (snd x)) :: xs
        | c when c < 0 -> (nt, tr) :: xs
        | _            -> x :: add_nt tr nt xs
    in
    let add_item cost item stack =
      let (_, prod, pos) = item in
      if Cost.is_infinite cost then stack else
        let stack_hd = function
          | [] -> []
          | x :: _ -> x
        and stack_tl = function
          | [] -> []
          | _ :: xs -> xs
        in
        let rec aux stack = function
          | 0 -> add_nt (Trace {cost; items = [item]}) (Production.lhs prod)
                   (stack_hd stack) :: stack_tl stack
          | n -> stack_hd stack :: aux (stack_tl stack) (n - 1)
        in
        aux stack pos
    in
    Lr1.tabulate (fun st ->
        List.fold_left (fun acc (prod, pos) ->
            if pos = 0 then acc else (
              let cost, _actions = S.solve (S.Tail (st, prod, pos)) in
              add_item cost (st, prod, pos) acc
            )
          )
          [] (Lr0.items (Lr1.lr0 st))
      )

  let step st ntss =
    let seen = ref Bytes.empty in
    let mem n =
      let off = n lsr 3 and mask = 1 lsl (n land 7) in
      Bytes.length !seen > off &&
      (Char.code (Bytes.get !seen off) land mask <> 0)
    in
    let mark_seen n =
      let off = n lsr 3 and mask = 1 lsl (n land 7) in
      let len = Bytes.length !seen in
      (if len <= off then
         seen := Bytes.cat !seen (Bytes.make (off + 1 - len) '\000'));
      let code = Char.code (Bytes.get !seen off) lor mask in
      Bytes.set !seen off (Char.chr code)
    in
    let rec aux = function
      | [] -> []
      | ((nt, tr) :: x) :: xs
        when not (mem (Nonterminal.to_int nt)) &&
             not (Nonterminal.kind nt = `START) ->
          mark_seen (Nonterminal.to_int nt);
          let st' = List.assoc (N nt) (Lr1.transitions st) in
          let xs' = synthesize st' in
          let xs' = match xs' with
            | [] -> []
            | _ :: xs -> xs
          in
          let merge_trace (nt,tr') = (nt, Trace.cat tr' tr) in
          let xs' = List.map (List.map merge_trace) xs' in
          aux (State.merge xs' (x :: xs))
      | (_ :: x) :: xs -> aux (x :: xs)
      | [] :: xs -> xs
    in
    aux ntss

  let init st = ((st, [st]), step st (synthesize st))

  let pred =
    (* Compute lr1 predecessor relation *)
    let tbl1 = Array.make Lr1.count [] in
    let revert_transition s1 (sym,s2) =
      assert (match Lr0.incoming (Lr1.lr0 s2) with
          | None -> false
          | Some sym' -> sym = sym');
      tbl1.(Lr1.to_int s2) <- s1 :: tbl1.(Lr1.to_int s2)
    in
    Lr1.iter
      (fun lr1 -> List.iter (revert_transition lr1) (Lr1.transitions lr1));
    (fun lr1 -> tbl1.(Lr1.to_int lr1))

  let expand stuck_states ((st, sts), nts) =
    List.map (fun st' ->
        let nts' = step st' nts in
        if nts' = [] then (stuck_states := st' :: !stuck_states);
        ((st', st' :: sts), nts')
      )
      (pred st)

  let all_stuck_states : (Lr1.t, int ref) Hashtbl.t =
    Hashtbl.create 7

  let recover st : recovery =
    (* How big is the known prefix of the stack *)
    let known_prefix =
      let items = Lr0.items (Lr1.lr0 st) in
      List.fold_left (fun pos (_, pos') -> max pos pos')
        (snd (List.hd items)) (List.tl items)
    in
    (* Walk this prefix *)
    let stuck = ref false in
    let stuck_states = ref [] in
    let traces =
      let acc = ref [init st] in
      for _i = 1 to known_prefix - 1 do
        acc := List.concat (List.map (expand stuck_states) !acc)
      done;
      !acc
    in
    (*Printf.printf "trace(%d): %d items\n%!"
      (Lr1.to_int st) (List.length traces);*)
    (* Last step *)
    let process_trace trace =
      match expand stuck_states trace with
      | [] -> (* Initial state *)
          assert (snd trace = []); []
      | states ->
          let select_trace traces =
            (* Pick a trace with minimal cost, somewhat arbitrary *)
            match List.flatten traces with
            | [] ->
              List.iter (fun st ->
                 let r =
                   try Hashtbl.find all_stuck_states st
                   with Not_found ->
                     let r = ref 0 in
                     Hashtbl.add all_stuck_states st r;
                     r
                 in
                 incr r
              ) !stuck_states;
              stuck := true;
              stuck_states := [];
              None
            | (_, trace) :: alternatives ->
              Some (List.fold_left (fun tr1 (_,tr2) -> Trace.min tr1 tr2) trace alternatives)
          in
          let select_expansion = function
            | (_, []) ->
              (* Reached stack bottom *)
              (None, select_trace (snd trace))
            | ((st, _sts), trace') ->
              (Some st, select_trace trace')
          in
          List.map select_expansion states
    in
    let cases =
      List.flatten @@ List.map (fun trace ->
          List.fold_right
            (fun (st, tr') acc -> match tr' with
              | Some (Trace { items ; _ }) -> (st, items) :: acc
              | None -> acc)
            (process_trace trace) []
        ) traces;
    in
    if !stuck then
        Format.printf "Not enough annotation to recover from state %d:\n%a\n%!"
          (Lr1.to_int st) Print.itemset (Lr0.items (Lr1.lr0 st));
    { prefix = known_prefix; cases }

  let recover = Lr1.tabulate recover

  let () =
    let all_stuck_states = Hashtbl.fold (fun k v acc -> (k, !v) :: acc) all_stuck_states [] in
    let all_stuck_states = List.sort (fun (_,v1) (_,v2) -> compare v2 v1) all_stuck_states in
    List.iter (fun (st, count) ->
      Format.printf "# State %d is preventing recovery from %d states:\n%a\n\n%!"
        (Lr1.to_int st) count
        Print.itemset (Lr0.items (Lr1.lr0 st))
    ) all_stuck_states

  let report _ppf = ()
end
