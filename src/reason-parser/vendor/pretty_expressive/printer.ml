(* Pretty Expressive - vendored and patched for runtime page_width configuration
   Original: https://github.com/sorawee/pretty-expressive-ocaml
   
   Patch: Changed page_width from int to (unit -> int) thunk to allow
   runtime configuration of page width. *)

(* magic numbers *)
let param_memo_limit = 7

type 's treeof =
  | One of 's
  | Cons of 's treeof * 's treeof

let render_tree (renderer : Signature.renderer) (t: string treeof): unit =
  let rec loop (t: string treeof) =
    match t with
    | One v -> renderer v
    | Cons (x, y) -> loop x; loop y
  in loop t

let hashtbl_ref_and_set (tbl : ('a, 'b) Hashtbl.t) (key : 'a) (thk : (unit -> 'b)) : 'b =
  if Hashtbl.mem tbl key then
    Hashtbl.find tbl key
  else
    let v = thk () in
    Hashtbl.add tbl key v;
    v

module Core (C : Signature.CostFactory) = struct
  let global_id = ref 0
  let next_id () : int =
    let id = !global_id in
    global_id := id + 1;
    id

  type measure = { last: int; cost: C.t; layout: Signature.renderer -> unit }

  let (<==) (m1 : measure) (m2 : measure): bool =
    m1.last <= m2.last && C.le m1.cost m2.cost

  type measure_set =
    | MeasureSet of measure list (* sorted by last in the decreasing order *)
    | Tainted of (unit -> measure)

  type doc =
    { dc: doc_case;
      id: int;
      memo_w: int;
      nl_cnt: int;
      table: ((int, measure_set) Hashtbl.t) option }
  (* invariant: none of the docs contains Fail, unless it's a Fail itself. *)
  and doc_case =
    | Text of string treeof * int
    | Newline of string option
    | Concat of doc * doc
    | Choice of doc * doc
    | Nest of int * doc
    | Align of doc
    | Reset of doc
    | Cost of C.t * doc
    | Context of (int -> int -> doc)
    (* invariant: the list length >= 2 *)
    | TwoColumns of (doc * doc) list
    | Blank of int
    | Evaled of measure_set
    | Fail

  type cost = C.t

  let init_memo_w = param_memo_limit - 1
  let calc_weight (d : doc) : int =
    if d.memo_w = 0 then init_memo_w else d.memo_w - 1
  let init_table (w : int) = if w = 0 then Some (Hashtbl.create 5) else None

  let fail : doc =
    { dc = Fail;
      id = next_id ();
      memo_w = init_memo_w;
      nl_cnt = 0;
      table = None }

  let newline (v : string option) : doc =
    { dc = Newline v;
      id = next_id ();
      memo_w = init_memo_w;
      nl_cnt = 1;
      table = None }

  let make_text (s : string treeof) (l : int) : doc =
    { dc = Text (s, l);
      id = next_id ();
      memo_w = init_memo_w;
      nl_cnt = 0;
      table = None }

  let text (s : string) : doc =
    make_text (One s) (String.length s)

  let blank (i : int) : doc =
    { dc = Blank i;
      id = next_id ();
      nl_cnt = 0;
      table = None;
      memo_w = init_memo_w }

  let rec cost (c : C.t) (d : doc) : doc =
    match d.dc with
    | Fail -> fail
    | Cost (c2, d) -> cost (C.combine c c2) d
    | _ ->
      let memo_w = calc_weight d in
      { dc = Cost (c, d);
        id = next_id ();
        memo_w;
        nl_cnt = d.nl_cnt;
        table = init_table memo_w }

  let rec (^^) (d1 : doc) (d2 : doc) : doc =
    match d1.dc, d2.dc with
    | Fail, _ | _, Fail -> fail
    | Text (_, 0), _ -> d2
    | _, Text (_, 0) -> d1
    | Text (s1, l1), Text (s2, l2) -> make_text (Cons (s1, s2)) (l1 + l2)
    | _, Cost (c, d2) -> cost c (d1 ^^ d2)
    | Cost (c, d1), _ -> cost c (d1 ^^ d2)
    | _ ->
      let memo_w = min (calc_weight d1) (calc_weight d2) in
      { dc = Concat (d1, d2);
        id = next_id ();
        memo_w;
        nl_cnt = d1.nl_cnt + d2.nl_cnt;
        table = init_table memo_w }

  let rec nest (n : int) (d : doc) : doc =
    match d.dc with
    | Fail | Align _ | Reset _ | Text _ -> d
    | Cost (c, d) -> cost c (nest n d)
    | _ ->
      let memo_w = calc_weight d in
      { dc = Nest (n, d);
        id = next_id ();
        memo_w;
        nl_cnt = d.nl_cnt;
        table = init_table memo_w }

  let rec reset (d : doc) : doc =
    match d.dc with
    | Fail | Align _ | Reset _ | Text _ -> d
    | Cost (c, d) -> cost c (reset d)
    | _ ->
      let memo_w = calc_weight d in
      { dc = Reset d;
        id = next_id ();
        memo_w;
        nl_cnt = d.nl_cnt;
        table = init_table memo_w }

  let rec align (d : doc) : doc =
    match d.dc with
    | Fail | Align _ | Reset _ | Text _ -> d
    | Cost (c, d) -> cost c (align d)
    | _ ->
      let memo_w = calc_weight d in
      { dc = Align d;
        id = next_id ();
        memo_w = memo_w;
        nl_cnt = d.nl_cnt;
        table = init_table memo_w }

  (* Only for internal use. Don't provide it.
     Invariant: the returned doc must not be fail *)
  let context (f : int -> int -> doc) (nl_cnt : int) : doc =
    { dc = Context f;
      id = next_id ();
      memo_w = 0;
      nl_cnt;
      table = init_table 0 }

  (* Only for internal use. Don't provide it.
     This is a super unsafe construct when used arbitrarily.
     Only use it when we know exactly that it will be resolved under
     a specific column position and indentation level that it was
     previously resolved for *)
  let evaled (ms : measure_set) (nl_cnt : int) : doc =
    { dc = Evaled ms;
      id = next_id ();
      memo_w = 0;
      nl_cnt;
      table = init_table 0 }

  let (<|>) (d1 : doc) (d2 : doc) : doc =
    if d1 == fail then d2
    else if d2 == fail then d1
    else
      let memo_w = min (calc_weight d1) (calc_weight d2) in
      { dc = Choice (d1, d2);
        id = next_id ();
        memo_w;
        nl_cnt = max d1.nl_cnt d2.nl_cnt;
        table = init_table memo_w }

  let empty : doc = text ""

  let hard_nl : doc = newline None

  let two_columns (ds : (doc * doc) list) : doc =
    match ds with
    | [] -> empty
    | [dl, dr] -> align (dl ^^ dr)
    | _ ->
      let any_fail =
        List.exists (fun (d1, d2) -> d1 == fail || d2 == fail) ds in
      if any_fail then
        fail
      else
        let nl_cnt =
          List.fold_left
            (fun acc (d1, d2) -> acc + d1.nl_cnt + d2.nl_cnt)
            0
            ds in
        { dc = TwoColumns ds;
          id = next_id ();
          memo_w = 0;
          nl_cnt;
          table = init_table 0 }

  let merge (ml1 : measure_set) (ml2 : measure_set): measure_set =
    match ml1, ml2 with
    | _, Tainted _ -> ml1
    | Tainted _, _ -> ml2
    | MeasureSet ms1, MeasureSet ms2 ->
      let rec loop ms1 ms2 = match (ms1, ms2) with
        | [], _ -> ms2
        | _, [] -> ms1
        | m1 :: ms1p, m2 :: ms2p ->
          if m1 <== m2 then loop ms1 ms2p
          else if m2 <== m1 then loop ms1p ms2
          else if m1.last > m2.last then m1 :: loop ms1p ms2
          else (* m2.last < m1.last *) m2 :: loop ms1 ms2p
      in MeasureSet (loop ms1 ms2)

  let (++) (m1 : measure) (m2 : measure): measure =
    { last = m2.last;
      cost = C.combine m1.cost m2.cost;
      layout = fun renderer ->
        m1.layout renderer;
        m2.layout renderer }

  let process_concat
      (process_left : measure -> measure_set)
      (ml1 : measure_set) : measure_set =
    match ml1 with
    | Tainted mt1 ->
      Tainted (fun () ->
          let m1 = mt1 () in
          match process_left m1 with
          | Tainted mt2 -> m1 ++ mt2 ()
          | MeasureSet (m2 :: _) -> m1 ++ m2
          | _ -> failwith "unreachable")
    | MeasureSet ms1 ->
      let do_one (m1 : measure): measure_set =
        let rec loop ms2 result current_best =
          match ms2 with
          | [] -> List.rev (current_best :: result)
          | m2 :: ms2 ->
            let current = m1 ++ m2 in
            if C.le current.cost current_best.cost then loop ms2 result current
            else loop ms2 (current_best :: result) current
        in match process_left m1 with
        | Tainted m2 -> Tainted (fun () -> m1 ++ m2 ())
        | MeasureSet (m2 :: ms2) -> MeasureSet (loop ms2 [] (m1 ++ m2))
        | _ -> failwith "unreachable" in
      let rec fold_right (ms: measure list): measure_set =
        match ms with
        | [m] -> do_one m
        | m :: ms -> merge (do_one m) (fold_right ms)
        | [] -> failwith "unreachable"
      in fold_right ms1

  let memoize f: doc -> int -> int -> measure_set =
    let all_slots = C.limit + 1 in
    let rec g ({ memo_w; table; _ } as d) (c : int) (i : int) =
      if c <= C.limit && i <= C.limit && memo_w = 0 then
        let key = i * all_slots + c in
        match table with
        | None -> failwith "unreachable"
        | Some tbl -> hashtbl_ref_and_set tbl key (fun () -> f g d c i)
      else f g d c i
    in g

  let choose_one (ml : measure_set): measure =
    match ml with
    | Tainted mt -> mt ()
    | MeasureSet (m :: _) -> m
    | _ -> failwith "unreachable"

  let do_two_columns self (ds : (doc * doc) list) (c : int) : measure_set =
    let cache_row = Hashtbl.create 16 in
    let cache_before = Hashtbl.create 16 in
    let cache_after = Hashtbl.create 16 in
    let left_ms = List.map (fun (d1, _) -> self d1 c c) ds in
    let left_any_tainted = List.exists
        (fun ms ->
           match ms with
           | Tainted _ -> true
           | _ -> false)
        left_ms in
    let build_choice c_sep before cur_left cur_right after =
      let build_row d1 d2 =
        d1 ^^
        context (fun c_in _ ->
            if c_sep >= c_in then
              blank (c_sep - c_in)
            else
              cost (C.two_columns_overflow (c_in - c_sep)) empty) 0 ^^
        d2
      in
      let build_cached_row i left right =
        hashtbl_ref_and_set cache_row (c_sep, i)
          (fun () -> build_row left right)
      in
      let rec build_before (i, _, left, right) before =
        hashtbl_ref_and_set cache_before (c_sep, i)
          (fun () ->
             (match before with
              | [] -> empty
              | before_hd :: before ->
                build_before before_hd before) ^^
             build_cached_row i left right ^^ hard_nl)
      in
      let rec build_after (i, _, left, right) after =
        hashtbl_ref_and_set cache_after (c_sep, i)
          (fun () ->
             hard_nl ^^ build_cached_row i left right ^^
             (match after with
              | [] -> empty
              | after_hd :: after -> build_after after_hd after))
      in
      (match before with
       | [] -> empty
       | before_hd :: before_tl -> build_before before_hd before_tl) ^^
      build_row cur_left cur_right ^^
      (match after with
       | [] -> empty
       | after_hd :: after_tl -> build_after after_hd after_tl)
    in
    let rec loop_limit
        (before : (int * measure_set * doc * doc) list)
        (after : (int * measure_set * doc * doc) list) =
      match after with
      | [] -> fail
      | ((_, ms, left, right) as tup) :: after ->
        let build_choice c_sep ms =
          cost (C.two_columns_bias (c_sep - c))
            (build_choice c_sep before (evaled ms left.nl_cnt) right after)
        in
        (match ms with
         | Tainted mt ->
           let m = mt () in
           build_choice m.last ms
         | MeasureSet ms ->
           let rec loop_inner ms =
             match ms with
             | [] -> fail
             | m :: ms -> build_choice m.last (MeasureSet [m]) <|> loop_inner ms
           in
           loop_inner ms <|> loop_limit (tup :: before) after)
    in
    let make_doc ms (d1, d2) (i, acc) =
      let ms = match ms with
        (* We might share the measure set, but sharing a Tainted is bad,
          since it contains a thunk. We want to share answers,
          not a promise to produce an answer! So here, we force the evaluation
          right away, and stuff the answer back to a thunk again, so that
          the answers are shared. Note that we can do this since
          we are already in a delayed context, if there is indeed a Tainted. *)
        | Tainted mt -> let m = mt () in Tainted (fun () -> m)
        | MeasureSet _ -> ms
      in
      (* NOTE: we can get the nl_cnt here to be precise with some
        additional tracking. Do we want to do that? *)
      i + 1, (i, ms, evaled ms d1.nl_cnt, d2) :: acc
    in
    let get_measure_set () =
      let _, after = List.fold_right2 make_doc left_ms ds (0, []) in
      let d = loop_limit [] after in
      self d c c
    in
    if left_any_tainted then
      Tainted (fun () -> get_measure_set () |> choose_one)
    else
      get_measure_set ()

  let pretty_print_info
      ?(init_c = 0)
      (renderer : Signature.renderer)
      (d : doc): cost Util.info =
    let resolve self { dc; _ } (c : int) (i : int) : measure_set =
      let core () =
        match dc with
        | Text (s, len_s) ->
          MeasureSet [{ last = c + len_s;
                        cost = C.text c len_s;
                        layout = fun renderer -> render_tree renderer s }]
        | Newline _ ->
          MeasureSet [{ last = i;
                        cost = C.newline i;
                        layout = fun renderer ->
                          renderer "\n";
                          renderer (String.make i ' ') }]
        | Concat (d1, d2) ->
          process_concat (fun (m1 : measure) -> self d2 m1.last i) (self d1 c i)
        | Choice (d1, d2) ->
          if d1.nl_cnt < d2.nl_cnt
          then merge (self d2 c i) (self d1 c i)
          else merge (self d1 c i) (self d2 c i)
        | Nest (n, d) -> self d c (i + n)
        | Align d -> self d c c
        | Reset d -> self d c 0
        | Cost (co, d) ->
          let add_cost (m : measure) = { m with cost = C.combine co m.cost } in
          (match self d c i with
           | MeasureSet ms -> MeasureSet (List.map add_cost ms)
           | Tainted mt -> Tainted (fun () -> add_cost (mt ())))
        | Context f -> self (f c i) c i
        | TwoColumns ds -> do_two_columns self ds c
        | Blank i ->
          MeasureSet [{ last = c + i;
                        cost = C.text 0 0;
                        layout = fun renderer -> renderer (String.make i ' ') }]
        | Evaled ms -> ms
        | Fail -> failwith "fails to render"
      in
      let exceeds = match dc with
        | Text (_, len) -> c + len > C.limit || i > C.limit
        | _ -> c > C.limit || i > C.limit in
      if exceeds then
        Tainted (fun () -> choose_one (core ()))
      else core () in
    let m, is_tainted = match memoize resolve d init_c 0 with
      | MeasureSet (m :: _) -> m, false
      | Tainted m -> m (), true
      | _ -> failwith "unreachable" in
    (* In Racket, a doc can be printed with many cost factories, *)
    (* so the memoization tables should be cleared. *)
    (* However, in OCaml, there is no need to do the same, *)
    (* since a doc is tied to a cost factory. *)
    m.layout renderer;
    { is_tainted ; cost = m.cost }
end

(* ----------------------------------------------------------------------0---- *)

module Make (C : Signature.CostFactory): (Signature.PrinterT with type cost = C.t) = struct
  include Core (C)

  (* Constants *)

  let comma = text ","
  let lbrack = text "["
  let rbrack = text "]"
  let lbrace = text "{"
  let rbrace = text "}"
  let lparen = text "("
  let rparen = text ")"
  let dquote = text "\""
  let space = text " "

  let nl = newline (Some " ")
  let break = newline (Some "")

  let (<$>) d1 d2 = d1 ^^ hard_nl ^^ d2

  let flatten : doc -> doc =
    let cache = Hashtbl.create 1000 in
    let rec flatten ({ dc = dc; id = id; _ } as d) =
      hashtbl_ref_and_set cache id (fun () ->
          match dc with
          | Fail | Text _ -> d
          | Newline None -> fail
          | Newline (Some s) -> text s
          | Concat (({ id = a_id; _ } as a), ({ id = b_id; _ } as b)) ->
            let { id = a_idp; _ } as ap = flatten a in
            let { id = b_idp; _ } as bp = flatten b in
            if a_idp = a_id && b_idp = b_id then d else ap ^^ bp
          | Choice (({ id = a_id; _ } as a), ({ id = b_id; _ } as b)) ->
            let { id = a_idp; _ } as ap = flatten a in
            let { id = b_idp; _ } as bp = flatten b in
            if a_idp = a_id && b_idp = b_id then d else ap <|> bp
          | Nest (_, d) | Align d | Reset d -> flatten d
          | Cost (c, ({ id = id; _ } as d)) ->
            let { id = idp; _ } as dp = flatten d in
            if idp = id then d else cost c dp
          (* There are at least two lines, so it can't be flattened *)
          | TwoColumns _ -> fail
          | Blank _ -> d
          | Context _ | Evaled _ -> failwith "unreachable")
    in flatten

  let (<+>) d1 d2 = d1 ^^ align d2
  let group d = d <|> (flatten d)

  let (<->) x y = (flatten x) <+> y

  let fold_doc f ds =
    match ds with
    | [] -> empty
    | x :: xs -> List.fold_left f x xs

  let hcat = fold_doc (<->)
  let vcat = fold_doc (<$>)

  let pretty_format_info ?(init_c = 0) (d : doc): string * C.t Util.info =
    let buf = Buffer.create 16 in
    let info = pretty_print_info ~init_c:init_c (Buffer.add_string buf) d in
    Buffer.contents buf, info

  let pretty_print ?(init_c = 0) (renderer : Signature.renderer) (d : doc): unit =
    let _ = pretty_print_info ~init_c:init_c renderer d in
    ()

  let pretty_format ?(init_c = 0) (d : doc): string =
    let s, _ = pretty_format_info ~init_c:init_c d in s

  let pretty_format_debug ?(init_c = 0) (d : doc): string =
    let content, info = pretty_format_info ~init_c:init_c d in
    C.debug_format content info.is_tainted (C.string_of_cost info.cost)
end

module MakeCompat (C : Signature.CostFactory): (Signature.PrinterCompatT with type cost = C.t) = struct
  include Make (C)

  let (<>) = (^^)
end

let make_debug_format
    (page_width : int)
    (content : string)
    (is_tainted : bool)
    (cost : string) : string =
  let lines = String.split_on_char '\n' content in
  let zero_code = Char.code '0' in
  let header = String.init
      page_width
      (fun i -> ((i + 1) mod 10 + zero_code) |> Char.chr) in
  let content =
    List.map (fun l ->
        if String.length l > page_width then
          String.sub l 0 page_width ^
          "│" ^
          String.sub l page_width (String.length l - page_width)
        else
          l ^ String.make (page_width - String.length l) ' '  ^ "│") lines
    |> String.concat "\n"
  in
  Printf.sprintf "%s\n%s\n\nis_tainted: %b\ncost: %s"
    header
    content
    is_tainted
    cost

(* PATCHED: Changed page_width from int to (unit -> int) thunk for runtime configuration *)
let default_cost_factory ~(page_width : unit -> int) ?computation_width () =
  (module struct
    type t = int * int * int

    let limit = match computation_width with
      | None -> (float_of_int (page_width ())) *. 1.2 |> int_of_float
      | Some computation_width -> computation_width ()

    let text pos len =
      let pw = page_width () in
      let stop = pos + len in
      if stop > pw then
        let maxwc = max pw pos in
        let a = maxwc - pw in
        let b = stop - maxwc in
        b * (2*a + b), 0, 0
      else
        0, 0, 0

    let newline _ = 0, 0, 1

    let combine (o1, ot1, h1) (o2, ot2, h2) =
      o1 + o2, ot1 + ot2, h1 + h2

    let le c1 c2 = c1 <= c2

    let two_columns_overflow w = 0, w, 0

    let two_columns_bias _ = 0, 0, 0

    let string_of_cost (o, ot, h) = Printf.sprintf "(%d %d %d)" o ot h

    let debug_format = make_debug_format (page_width ())
  end: Signature.CostFactory with type t = int * int * int)

let version = "0.5-patched"

