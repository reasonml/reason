module Misc = struct

  let find_in_path = Misc.find_in_path
  let find_in_path_uncap = Misc.find_in_path_uncap

  type ref_and_value = R : 'a ref * 'a -> ref_and_value
  let protect_refs =
    let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
    fun refs f ->
      let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
      set_refs refs;
      match f () with
      | x           -> set_refs backup; x
      | exception e -> set_refs backup; raise e

  let may_map = Stdlib0.Option.map

  module Stdlib = struct
    module String = struct
      include String
      module Map = Map.Make (String)
    end
  end
end
