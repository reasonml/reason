open MenhirSdk
open Cmly_api
open Printf

module G = Cmly_read.Read(struct let filename = Sys.argv.(1) end)
open G

let print fmt = Printf.ksprintf print_endline fmt

(* We want to detect any state where an identifier is admissible.
   That way, we can assume that if a keyword is used and rejceted, the user was
   intending to put an identifier. *)
let states_transitioning_on_ident =
  (* A terminal is either "LIDENT" or "UIDENT" *)
  let term_is_ident term = match Terminal.name term with
    | "LIDENT" | "UIDENT" -> true
    | _ -> false
  in
  let sym_is_ident = function
    | T term -> term_is_ident term
    | N _ -> false
  in
  let keep_state lr1 =
    (* There are two kind of transitions (leading to SHIFT or REDUCE), detect
       those who accept identifiers *)
    List.exists (fun (term, prod) -> term_is_ident term) (Lr1.reductions lr1) ||
    List.exists (fun (sym, _) -> sym_is_ident sym) (Lr1.transitions lr1)
  in
  (* Now we filter the list of all states and keep the interesting ones *)
  G.Lr1.fold (fun lr1 acc -> if keep_state lr1 then lr1 :: acc else acc) []

let () =
  (* Produce a function that will be linked into the reason parser to recognize
     these states at runtime.
     TODO: a more compact encoding could be used, for now we don't care and
     just pattern matches on states.
  *)
  print "let transitions_on_ident = function";
  List.iter
    (fun lr1 -> print "  | %d" (Lr1.to_int lr1))
    states_transitioning_on_ident;
  print "      -> true";
  print "  | _ -> false"
