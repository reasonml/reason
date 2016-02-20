open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    flag ["ocaml"; "pp"; "pp_byte"; "reason"] &
      A"src/reason_pp.byte";
    flag ["ocaml"; "pp"; "pp_native"; "reason"] &
      A"src/reason_pp.native";

  | _ -> ())
