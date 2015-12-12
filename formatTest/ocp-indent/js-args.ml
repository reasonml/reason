let () =
  foo.bar <-
    f x
      y z

let should_check_can_sell_and_marking regulatory_regime =
  match z with
  | `foo
    -> some_function
         argument
(* The above typically occurs in a multi-pattern match clause, so the clause
   expression is on a line by itself.  This is the more typical way a long
   single-pattern match clause would be written: *)
let should_check_can_sell_and_marking regulatory_regime =
  match z with
  | `foo ->
    some_function
      argument

let f = fun x ->
  ghi
    x

(* common *)
let x =
  try x with
  | a -> b
  | c -> d
let x = try x with
  | a -> b
  | c -> d
let x =
  try x
  with
  | a -> b
  | c -> d

let z =
  some_function
    argument



let () =
  f a b ~c:c
    d

let () =
  f a b ~c:1.
    d

let () =
  My_module.f a b ~c:c
    d

(* This last case is where Tuareg is inconsistent with the others. *)
let () =
  My_module.f a b ~c:1.
    d



let () =
  messages :=
    Message_store.create (Session_id.of_string "")
      (* Tuareg indents these lines too far to the left. *)
      "herd-retransmitter"
      Message_store.Message_size.Byte



let () =
  raise (Bug ("foo"
              (* In this and similar cases, we want the subsequent lines to
                 align with the first expression. *)
              ^ "bar"));
  raise (Bug ("foo" ^ "quux"
              ^ "bar"));
  raise (Bug (foo + quux
              ^ "bar"));
  raise (Bug ((foo + quux)
              ^ "bar"))

(* Except in specific cases, we want the argument indented relative to the
   function being called.  (Exceptions include "fun" arguments where the line
   ends with "->" and subsequent lines beginning with operators, like above.) *)
let () =
  Some (Message_store.create s
          "herd-retransmitter" ~unlink:true Message_store.Message_size.Byte)



(* We like the indentation of most arguments, but want to get back towards the
   left margin in a few special cases: *)
let _ =
  foo (bar (fun x ->                    (* special: "fun _ ->" at EOL *)
    baz))                               (* assume no more arguments to "bar" *)
let _ =
  foo
    ~a_long_field_name:(check (fun bar ->
      baz))
let _ =
  foo ~a_long_field_name:(check (fun bar ->
    baz))
let _ =
  foo (bar (quux (fnord (fun x ->       (* any depth *)
    baz))))

(* We also wanted to tweak the operator indentation, making operators like <=
   not special cases in contexts like this:  *)
let _ =
  assert (foo (bar + baz
               <= quux))                (* lined up under left argument to op,
                                           sim. to ^ above *)
(* Sim. indentation of if conditions: *)
let _ =
  if (a
      <= b)
  then ()
let _ =
  (* Comparisons are different than conditionals; we don't regard them as
     conceptually part of the [if] expression. *)
  if a
     <= b
  then ()
let _ =
  (* We regard the outermost condition terms as conceptually part of the [if]
     expression and indent accordingly.  Whether [&&] or [||], conditionals
     effectively state lists of conditions for [then]. *)
  if Edge_adjustment.is_zero arb.cfg.extra_edge
  && 0. = sys.plugs.edge_backoff
  && 0. = zero_acvol_edge_backoff
  then 0.
  else 1.
let _ =
  if
    Edge_adjustment.is_zero arb.cfg.extra_edge
    && 0. = sys.plugs.edge_backoff
    && 0. = zero_acvol_edge_backoff
  then 0.
  else 1.
let _ =
  let entries = List.filter (Lazy.force transferstati) ~f:(fun ts ->
    Pcre.pmatch ~pat ts.RQ.description
  ) in
  x

(* combination of operator at BOL and -> at EOL: *)
let _ =
  Shell.ssh_lines x
  |! List.map ~f:(f (g (fun x ->
       let name, path = String.lsplit2_exn ~on:'|' x in
       String.strip name, String.strip path)))

(* open paren ending line like begin *)
let _ =
  if a (p ^/ "s") [ e ] = Ok () then `S (
    let label count =
      sprintf "%d s" c ^ if c = 1 then ":" else "s"
    in
    x
  )
