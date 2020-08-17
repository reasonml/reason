let error_of_exn = Location.error_of_exn

let get_load_paths () =
  !Config.load_path

let load_path_init l =
  Config.load_path := l

let get_unboxed_types () =
  false

let set_unboxed_types _b =
  ()

let may_map = Misc.may_map
