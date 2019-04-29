open MenhirSdk
open Recovery_custom

let name = ref ""
let verbose = ref false

let usage () =
  Printf.eprintf "Usage: %s [-v] file.cmly\n"
    Sys.argv.(0);
  exit 1

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-v" -> verbose := true
    | arg -> if !name = "" then name := arg else usage ()
  done;
  if !name = "" then
    usage ()

module G = Cmly_read.Read (struct let filename = !name end)
module A = Attributes.Recover_attributes(G)

let () =
  let open Format in
  let ppf = Format.err_formatter in
  if !verbose then begin
    let open G in
    Lr1.iter (fun (st : lr1) ->
        fprintf ppf "\n# LR(1) state #%d\n\n" (st :> int);
        fprintf ppf "Items:\n";
        Print.itemset ppf (Lr0.items (Lr1.lr0 st));
        fprintf ppf "Transitions:\n";
        List.iter (fun (sym,(st' : lr1)) ->
            fprintf ppf " - on %a, goto #%d\n"
              Print.symbol sym
              (st' :> int)
          ) (Lr1.transitions st);
        fprintf ppf "Reductions:\n";
        List.iter (fun (t,ps) ->
            let p : production = List.hd ps in
            fprintf ppf " - on %a, reduce %d:\n  %a\n"
              Print.terminal t
              (p :> int) Print.production p
          ) (Lr1.reductions st);
      );
    Production.iter (fun (p : production) ->
        fprintf ppf "\n# Production p%d\n%a"
          (p :> int) Print.production p
      );
  end

module S = Synthesis.Synthesizer(G)(A)

let () = if !verbose then S.report Format.err_formatter

module R = Recover(G)(S)

(*let () = if !verbose then R.report Format.err_formatter*)

module E = Emitter.Make(G)(A)(S)(R)

let () = E.emit Format.std_formatter
