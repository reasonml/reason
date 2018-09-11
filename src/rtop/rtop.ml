let getenv_opt env =
  try Some(Sys.getenv env)
  with Not_found -> None

(* Try to read the HOME environment
 * if it is not set we assume it's windows and fallback to USERPROFILE *)
let home = match getenv_opt "HOME" with
  | Some(p) -> p
  | None -> Sys.getenv "USERPROFILE"

let () = Unix.openfile (Filename.concat home ".utoprc") [] 0o640
         |> Unix.close

let () = Unix.openfile (Filename.concat home ".utop-history") [] 0o640
         |> Unix.close

(* Get the dir of the current executable *)
let dir = Filename.dirname Sys.executable_name

(* If there is only 1 arg it's the exeuctable and we can remove that *)
let argv = match Array.length Sys.argv with
  | 1 -> [||]
  | argLength -> Array.sub Sys.argv 1 argLength

let args = if Array.length argv == 1 && argv.(0) = "stdin" then
    let refmted = Pervasives.input_line (Unix.open_process_in "refmt --parse re --print ml --interface false") in

    Array.concat [[|"utop-full";|]; argv; [|refmted|]]
  else
    Array.concat [[|"utop-full"; "-init"; Filename.concat dir "rtop_init.ml";|]; argv; [|"-I"; home; "-safe-string"|]];;

Unix.execvp "utop-full" args
