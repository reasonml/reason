let split_pos {Lexing. pos_lnum; pos_bol; pos_cnum; _} =
  (pos_lnum, pos_cnum - pos_bol)

let rev_filter ~f xs =
  let rec aux f acc = function
    | x :: xs when f x -> aux f (x :: acc) xs
    | _ :: xs -> aux f acc xs
    | [] -> acc
  in
  aux f [] xs

let rec rev_scan_left acc ~f ~init = function
  | [] -> acc
  | x :: xs ->
    let init = f init x in
    rev_scan_left (init :: acc) ~f ~init xs

module Make
    (Parser : MenhirLib.IncrementalEngine.EVERYTHING)
    (Recovery : sig
       val default_value : Location.t -> 'a Parser.symbol -> 'a

       type action =
         | Abort
         | R of int
         | S : 'a Parser.symbol -> action
         | Sub of action list

       type decision =
         | Nothing
         | One of action list
         | Select of (int -> action list)

       val depth : int array

       val recover : int -> decision

       val guide : 'a Parser.symbol -> bool

       val token_of_terminal : 'a Parser.terminal -> 'a -> Parser.token

       val nullable : 'a Parser.nonterminal -> bool
     end) =
struct

  type 'a candidate = {
    line: int;
    min_col: int;
    max_col: int;
    env: 'a Parser.env;
  }

  type 'a candidates = {
    shifted: Parser.xsymbol option;
    final: 'a option;
    candidates: 'a candidate list;
  }

  module T = struct
    [@@@ocaml.warning "-37"]

    type 'a checkpoint =
      | InputNeeded of 'a Parser.env
      | Shifting of 'a Parser.env * 'a Parser.env * bool
      | AboutToReduce of 'a Parser.env * Parser.production
      | HandlingError of 'a Parser.env
      | Accepted of 'a
      | Rejected
    external inj : 'a checkpoint -> 'a Parser.checkpoint = "%identity"
  end

  let feed_token ~allow_reduction token env =
    let rec aux allow_reduction = function
      | Parser.HandlingError _ | Parser.Rejected -> `Fail
      | Parser.AboutToReduce _ when not allow_reduction -> `Fail
      | Parser.Accepted v -> `Accept v
      | Parser.Shifting _ | Parser.AboutToReduce _ as checkpoint ->
        aux true (Parser.resume checkpoint)
      | Parser.InputNeeded env as checkpoint -> `Recovered (checkpoint, env)
    in
    aux allow_reduction (Parser.offer (T.inj (T.InputNeeded env)) token)

  let rec follow_guide col env = match Parser.top env with
    | None -> col
    | Some (Parser.Element (state, _, pos, _)) ->
      if Recovery.guide (Parser.incoming_symbol state) then
        match Parser.pop env with
        | None -> col
        | Some env -> follow_guide (snd (split_pos pos)) env
      else
        col

  let candidate env =
    let line, min_col, max_col =
      match Parser.top env with
      | None -> 1, 0, 0
      | Some (Parser.Element (state, _, pos, _)) ->
        let depth = Recovery.depth.(Parser.number state) in
        let line, col = split_pos pos in
        if depth = 0 then
          line, col, col
        else
          let col' = match Parser.pop_many depth env with
            | None -> max_int
            | Some env ->
              match Parser.top env with
              | None -> max_int
              | Some (Parser.Element (_, _, pos, _)) ->
                follow_guide (snd (split_pos pos)) env
          in
          line, min col col', max col col'
    in
    { line; min_col; max_col; env }

  let attempt r token =
    let _, startp, _ = token in
    let line, col = split_pos startp in
    let more_indented candidate =
      line <> candidate.line && candidate.min_col > col in
    let recoveries =
      let rec aux = function
        | x :: xs when more_indented x -> aux xs
        | xs -> xs
      in
      aux r.candidates
    in
    let same_indented candidate =
      line = candidate.line ||
      (candidate.min_col <= col && col <= candidate.max_col)
    in
    let recoveries =
      let rec aux = function
        | x :: xs when same_indented x -> x :: aux xs
        | _ -> []
      in
      aux recoveries
    in
    let rec aux = function
      | [] -> `Fail
      | x :: xs -> match feed_token ~allow_reduction:true token x.env with
        | `Fail ->
          aux xs
        | `Recovered (checkpoint, _) -> `Ok (checkpoint, x.env)
        | `Accept v ->
          begin match aux xs with
            | `Fail -> `Accept v
            | x -> x
          end
    in
    aux recoveries

  let decide env =
    let rec nth_state env n =
      if n = 0 then
        match Parser.top env with
        | None -> -1 (*allow giving up recovery on empty files*)
        | Some (Parser.Element (state, _, _, _)) -> Parser.number state
      else
        match Parser.pop env with
        | None -> assert (n = 1); -1
        | Some env -> nth_state env (n - 1)
    in
    let st = nth_state env 0 in
    match Recovery.recover st with
    | Recovery.Nothing -> []
    | Recovery.One actions -> actions
    | Recovery.Select f -> f (nth_state env Recovery.depth.(st))

  let generate (type a) (env : a Parser.env) =
    let module E = struct
      exception Result of a
    end in
    let shifted = ref None in
    let rec aux acc env =
      match Parser.top env with
      | None -> None, acc
      | Some (Parser.Element (_state, _, _startp, endp)) ->
        let actions = decide env in
        let candidate0 = candidate env in
        let rec eval (env : a Parser.env) : Recovery.action -> a Parser.env = function
          | Recovery.Abort ->
            raise Not_found
          | Recovery.R prod ->
            let prod = Parser.find_production prod in
            Parser.force_reduction prod env
          | Recovery.S (Parser.N n as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None && not (Recovery.nullable n) then
              shifted := Some xsym;
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            Parser.feed sym endp v endp env
          | Recovery.S (Parser.T t as sym) ->
            let xsym = Parser.X sym in
            if !shifted = None then shifted := Some xsym;
            let loc = {Location. loc_start = endp; loc_end = endp; loc_ghost = true} in
            let v = Recovery.default_value loc sym in
            let token = (Recovery.token_of_terminal t v, endp, endp) in
            begin match feed_token ~allow_reduction:true token env with
              | `Fail -> assert false
              | `Accept v -> raise (E.Result v)
              | `Recovered (_,env) -> env
            end
          | Recovery.Sub actions ->
            List.fold_left eval env actions
        in
        match
          rev_scan_left [] ~f:eval ~init:env actions
          |> List.map (fun env -> {candidate0 with env})
        with
        | exception Not_found -> None, acc
        | exception (E.Result v) -> Some v, acc
        | [] -> None, acc
        | (candidate :: _) as candidates ->
          aux (candidates @ acc) candidate.env
    in
    let final, candidates = aux [] env in
    (!shifted, final, candidates)

  let generate env =
    let shifted, final, candidates = generate env in
    let candidates = rev_filter candidates
        ~f:(fun t -> not (Parser.env_has_default_reduction t.env))
    in
    { shifted; final; candidates = (candidate env) :: candidates }

end
