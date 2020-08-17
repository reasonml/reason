let error_of_exn exn =
  match Location.error_of_exn exn with
  | Some (`Ok exn) -> Some exn
  | Some `Already_displayed -> None
  | None -> None

let get_load_paths () =
  !Config.load_path

let load_path_init l =
  Config.load_path := l

let get_unboxed_types () =
  !Clflags.unboxed_types

let set_unboxed_types b =
  Clflags.unboxed_types := b

let may_map = Misc.may_map
