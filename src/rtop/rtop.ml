let getenv_opt env =
  try Some(Sys.getenv env)
  with Not_found -> None
in

(* Try to read the HOME environment
 * if it is not set we assume it's windows and fallback to USERPROFILE *)
let home = match getenv_opt "HOME" with
| Some(p) -> p
| None -> Sys.getenv "USERPROFILE"
in

let () = Unix.openfile (home ^ "/.utoprc") [] 0o640
|> Unix.close in

let () = Unix.openfile (home ^ "/.utop-history") [] 0o640
|> Unix.close in

(* Get the dir of the current executable *)
let dir = Filename.dirname Sys.argv.(0) in

(* If there is only 1 arg it's the exeuctable and we can remove that *)
let argv = match Array.length Sys.argv with
| 1 -> [||]
| argLength -> Array.sub Sys.argv 1 argLength
in

let args = if Array.length argv == 1 && argv.(0) = "stdin" then
  let refmted = Pervasives.input_line (Unix.open_process_in "refmt --parse re --print ml --interface false") in

  Array.concat [[|"utop-full";|]; argv; [|refmted|]]
else
  Array.concat [[|"utop-full"; "-init"; dir ^ "/rtop_init.ml";|]; argv; [|"-I"; home; "-safe-string"|]]
in

Unix.execvp "utop-full" args
