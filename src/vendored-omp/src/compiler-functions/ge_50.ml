let error_of_exn exn =
  match Location.error_of_exn exn with
  | Some (`Ok exn) -> Some exn
  | Some `Already_displayed -> None
  | None -> None

let get_load_paths () =
  Load_path.get_paths ()

let load_path_init l =
  let auto_include find_in_dir fn =
    if !Clflags.no_std_include then
      raise Not_found
    else
      let alert = Location.auto_include_alert in
      Load_path.auto_include_otherlibs alert find_in_dir fn
  in
  Load_path.init ~auto_include l

let get_unboxed_types () =
  !Clflags.unboxed_types

let set_unboxed_types b =
  Clflags.unboxed_types := b

let may_map = Option.map

let bad_docstring t = Warnings.Unexpected_docstring t
